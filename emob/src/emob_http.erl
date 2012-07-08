%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya, Tom Heinan
%%% @doc Erlymob HTTP interface for the API that manages mobs and locations.
%%% @end
%%%-------------------------------------------------------------------
-module(emob_http).
-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-author('Tom Heinan <me@tomheinan.com>').

-compile([{parse_transform, lager_transform}]).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([init/3, handle/2, terminate/2]).
-export([twitter_time_to_epoch/1, twitter_time_to_datetime/1]).


%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------

-include("defaults.hrl").


%% ------------------------------------------------------------------
%% Defines
%% ------------------------------------------------------------------

-define(HEADER_CONTENT_TYPE, 'Content-Type').
-define(MIME_TYPE_JSON, "application/json").

-type http_req()                                :: tuple().

-record(state, {
         peer
         }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

init({tcp, http}, Req, _Opts) ->
    Peer = printable_peer(Req),
    %% {Method, _} = cowboy_http_req:method(Req),
    %% lager:debug("[~s] Initializing ~s ~s~n", [Peer, Method, printable_path(Req)]),
    {ok, Req, #state{peer = Peer}}.


handle(Req0, State) ->
    {Method, Req1} = cowboy_http_req:method(Req0),
    {Path, Req} = cowboy_http_req:path(Req1),
    lager:debug("[~s] ~s ~s~n", [State#state.peer, Method, printable_path(Req)]),
    {ok, Reply} = case Method of
                      'GET' ->
                          handle_get(Path, Req, State);
                      'POST' ->
                          handle_post(Path, Req, State);
                      'PUT' ->
                          handle_put(Path, Req, State);
                      'DELETE' ->
                          handle_delete(Path, Req, State);
                      _ ->
                          cowboy_http_req:reply(405, Req)    %% method not allowed
                  end,
    {ok, Reply, State}.


terminate(_Req, _State) ->
    %% {Method, _} = cowboy_http_req:method(_Req),
    %% lager:debug("[~s] Terminating ~s ~s~n", [State#state.peer, Method, printable_path(_Req)]),
    ok.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @doc Update an entity.
-spec handle_put(Path :: cowboy_dispatcher:tokens(), http_req(), #state{}) -> {ok, http_req()}.
handle_put(Path, Req, State) ->
    lager:warning("[~s] Malformed PUT request to ~p~n", [State#state.peer, Path]),
    cowboy_http_req:reply(400, Req).   %% bad request


%% @doc Retrieve an entity.
-spec handle_get(Path :: cowboy_dispatcher:tokens(), http_req(), #state{}) -> {ok, http_req()}.
handle_get([<<"mobs">>], Req0, _State) ->
    {Value, Req} = cowboy_http_req:qs_val(?TOKEN, Req0),
    Posts = case Value of
                %% /mobs
                undefined ->
                    emob_post_receiver:get_all_posts();
                %% /mobs?token=:token
                Token ->
                    {ok, Values} = emob_user:get_posts(Token),
                    Values
            end,
    %% lager:debug("Found the foillowing posts:~n~p~n", [Posts]),
    Response = json_posts(Posts),
    cowboy_http_req:reply(200, [{?HEADER_CONTENT_TYPE, <<?MIME_TYPE_JSON>>}], Response, Req);

handle_get([<<"mob">>], Req0, _State) ->
    {Value, Req} = cowboy_http_req:qs_val(?ID, Req0),
    case Value of
        undefined ->
            lager:info("Missing 'id' in /mob request; qs=~p~n", [Req]),
            cowboy_http_req:reply(400, Req);   %% bad request
        PostId ->
            %% /mob?id=:id
            case emob_post_receiver:get_post(bstr:to_integer(PostId)) of
                #post{} = Post ->
                    Response = json_post(Post),
                    cowboy_http_req:reply(200, [{?HEADER_CONTENT_TYPE, <<?MIME_TYPE_JSON>>}], Response, Req);
                _ ->
                    lager:info("Could not find post with id=~p~n", [PostId]),
                    cowboy_http_req:reply(404, Req)    %% not found
            end
    end;

handle_get([<<"rsvp">>], Req0, _State) ->
    {Value, Req1} = cowboy_http_req:qs_val(?ID, Req0),
    case Value of
        undefined ->
            cowboy_http_req:reply(400, Req1);   %% bad request
        PostId ->
            %% /rsvp?id=:id&token=:token
            case cowboy_http_req:qs_val(?TOKEN, Req1) of
                {undefined, Req} ->
                    cowboy_http_req:reply(400, Req);   %% bad request
                {Token, Req} ->
                    emob_user:rsvp_post(Token, PostId),

                    Response = ejson:encode({[{<<"going">>, true}]}),
                    cowboy_http_req:reply(200, [{?HEADER_CONTENT_TYPE, <<?MIME_TYPE_JSON>>}], Response, Req)
            end
    end;

handle_get([<<"get_loc">>], Req0, _State) ->
    {Value1, Req} = cowboy_http_req:qs_val(?TOKEN, Req0),
    case Value1 of
        undefined ->
            cowboy_http_req:reply(400, Req);   %% bad request
        %% /get_loc?token=:token
        Token ->
            case emob_user:get_user(Token) of
                [#user{location = Location}] ->
                    {Lat, Lon} = location_center(Location),
                    Response = ejson:encode({[{<<"latitude">>, Lat}, {<<"longitude">>, Lon}]}),
                    cowboy_http_req:reply(200, [{?HEADER_CONTENT_TYPE, <<?MIME_TYPE_JSON>>}], Response, Req);
                _ ->
                    cowboy_http_req:reply(404, Req)   %% not found
            end
    end;

handle_get([<<"get_request_token">>], Req, _State) ->
    %% Here, the callback_url is in the body of the GET request
    {Args, _Req0} = cowboy_http_req:body_qs(Req),
    URL = proplists:get_value(?CALLBACK_URL, Args),
    Response =
    case emob_oauth_fsm:get_request_token(URL) of
        TokenData when is_record(TokenData, twitter_token_data) ->
            json_token(TokenData);
        Error ->
            json_error(Error)
    end,
    cowboy_http_req:reply(200, [{?HEADER_CONTENT_TYPE, <<?MIME_TYPE_JSON>>}], Response, Req);

handle_get([<<"get_access_token">>], Req, _State) ->
    %% Here, the Token and Verifier are in the query string itself
    {Args, _Req0} = cowboy_http_req:qs_vals(Req),
    Token = proplists:get_value(?OAUTH_TOKEN, Args),
    Verifier = proplists:get_value(?OAUTH_VERIFIER, Args),
    Response =
    case emob_oauth_fsm:get_access_token(Token, Verifier) of
       AccessData when is_record(AccessData, twitter_access_data) ->
            json_access(AccessData);
        Error ->
            json_error(Error)
    end,
    cowboy_http_req:reply(200, [{?HEADER_CONTENT_TYPE, <<?MIME_TYPE_JSON>>}], Response, Req);

handle_get([<<"get_credentials">>], Req, _State) ->
    %% Here, the Token is in the body of the request
    {Args, _Req0} = cowboy_http_req:body_qs(Req),
    Token = proplists:get_value(?OAUTH_TOKEN, Args),
    Response =
    case emob_oauth_fsm:get_credentials(Token) of
       AccessData when is_record(AccessData, twitter_access_data) ->
            json_access(AccessData);
        Error ->
            json_error(Error)
    end,
    cowboy_http_req:reply(200, [{?HEADER_CONTENT_TYPE, <<?MIME_TYPE_JSON>>}], Response, Req);

handle_get([<<"remove_credentials">>], Req, _State) ->
    %% Here, the Token is in the body of the request
    {Args, _Req0} = cowboy_http_req:body_qs(Req),
    Token = proplists:get_value(?OAUTH_TOKEN, Args),
    emob_oauth_fsm:remove_credentials(Token),
    Response = json_ok(),
    cowboy_http_req:reply(200, [{?HEADER_CONTENT_TYPE, <<?MIME_TYPE_JSON>>}], Response, Req);


handle_get(Path, Req, State) ->
    lager:warning("[~s] Malformed GET request to ~p~n", [State#state.peer, Path]),
    cowboy_http_req:reply(404, Req).   %% not found


%% @doc Create or update en entity.
-spec handle_post(Path :: cowboy_dispatcher:tokens(), http_req(), #state{}) -> {ok, http_req()}.
handle_post(Path, Req, State) ->
    lager:warning("[~s] Malformed POST request to ~p~n", [State#state.peer, Path]),
    cowboy_http_req:reply(404, Req).   %% not found


%% @doc Delete an entity.
-spec handle_delete(Path :: cowboy_dispatcher:tokens(), http_req(), #state{}) -> {ok, http_req()}.
handle_delete(Path, Req, State) ->
    lager:warning("[~s] Malformed DELETE request to ~p~n", [State#state.peer, Path]),
    cowboy_http_req:reply(400, Req).   %% bad request


% -spec ejson_to_proplist({[{Name :: binary(), Value :: binary()}]}) -> proplists:proplist().
% ejson_to_proplist({Doc}) ->
%     ejson_to_proplist(Doc, []).

% ejson_to_proplist([{Name, Value} | Tail], Acc) ->
%     %% FIXME We should be using binary_to_existing_atom/2 to convert the field
%     %%       names into atoms. Before we do that we must ensure that all the
%     %%       possible valid atoms are already in the atom table.
%     %% ejson_to_proplist(Tail, [{binary_to_existing_atom(Name, utf8), Value} | Acc]);
%     ejson_to_proplist(Tail, [{binary_to_atom(Name, utf8), Value} | Acc]);
% ejson_to_proplist([], Acc) ->
%     lists:reverse(Acc).


-spec printable_peer(Req :: term()) -> string().
printable_peer(Req) ->
    {{{I1, I2, I3, I4}, Port}, _} = cowboy_http_req:peer(Req),
    lists:flatten(io_lib:format("~w.~w.~w.~w:~w", [I1, I2, I3, I4, Port])).


-spec printable_path(Req :: term()) -> string().
printable_path(Req) ->
    {Path, _} = cowboy_http_req:raw_path(Req),
    Path.


location_center([Lat, Lon]) ->
    {Lat, Lon};
location_center([TopLat, LeftLon, BotLat, RightLon]) ->
    {TopLat + (BotLat - TopLat) / 2, LeftLon + (RightLon - LeftLon) / 2};
location_center(_Location) ->
    lager:debug("Invalid location ~p~n", [_Location]),
    {null, null}.


ok_to_ejson() ->
    {[{<<"code">>, <<"ok">>}]}.

token_to_ejson(TokenData) ->
    {[{?TOKEN, TokenData#twitter_token_data.access_token},
      {?SECRET, TokenData#twitter_token_data.access_token_secret}]}.

access_to_ejson(AccessData) ->
    {[{?TOKEN, AccessData#twitter_access_data.access_token},
      {?SECRET, AccessData#twitter_access_data.access_token_secret},
      {?USER_ID, AccessData#twitter_access_data.user_id},
      {?SCREEN_NAME, AccessData#twitter_access_data.screen_name}]}.

post_to_ejson(Post = #post{post_data = Tweet}) ->
    Rsvps = emob_post_receiver:get_rsvps(Post#post.id),
    %% lager:debug("Rsvps: ~p~n", [Rsvps]),
    {UserName, Going} = case Tweet#tweet.user of
                            #twitter_user{screen_name = ScreenName, id_str = UserId} when is_binary(ScreenName) ->
                                {ScreenName, lists:member(UserId, Rsvps)};
                            _ ->
                                {null, false}
                        end,
    lager:debug("User: ~p~n", [UserName]),
    %% lager:debug("Post ID: ~p~n", [Post#post.id]),
    {Lat, Lon} = location_center(Tweet#tweet.coordinates),
    {[{<<"id">>,      Post#post.id},
      {<<"tweet">>,   Tweet#tweet.text},
      {<<"user">>,    UserName},
      {<<"created">>, Tweet#tweet.created_at},
      {<<"where">>, {[{<<"latitude">>, Lat},
                      {<<"longitude">>, Lon}]}},
      {<<"when">>, Tweet#tweet.created_at},
      {<<"rsvps">>, length(Rsvps)},
      {<<"going">>, Going}]}.


build_valid_response(Result) ->
    {[{result, Result},
      {version, ?API_VERSION}]}.

build_error_response({error, Error}) ->
    {[{error, bstr:bstr(Error)},
      {version, ?API_VERSION}]}.

json_error(Error) ->
    ejson:encode(build_error_response(Error)).

json_ok() ->
    Result = ok_to_ejson(),
    ejson:encode(build_valid_response(Result)).

json_access(AccessData) ->
    Result = access_to_ejson(AccessData),
    ejson:encode(build_valid_response(Result)).

json_token(TokenData) ->
    Result = token_to_ejson(TokenData),
    ejson:encode(build_valid_response(Result)).

json_post(Post) ->
    ejson:encode(post_to_ejson(Post)).

json_posts(Posts) ->
    ConvertedPosts = [convert_post_time(Post) || Post <- Posts],
    SortedPosts = lists:sort(fun compare_post/2, ConvertedPosts),
    ejson:encode([post_to_ejson(SortedPost) || SortedPost <- SortedPosts]).

convert_post_time(#post{post_data = Tweet} = Post) ->
    Time = Tweet#tweet.created_at,
    NewTweet = Tweet#tweet{created_at = twitter_time_to_epoch(Time)},
    Post#post{post_data = NewTweet}.

compare_post(#post{post_data = T1}, #post{post_data = T2}) ->
    T1#tweet.created_at < T2#tweet.created_at.


%% Tuple containing a date and time.
-type datetime()                                :: {calendar:date(), {calendar:hour(), calendar:minute(), calendar:second() | float()}}.
%% A floating point number representing the number of seconds elapsed since
%% Jan 1, 1970, 00:00:00 (Unix epoch).
-type epoch()                                   :: non_neg_integer() | float().

%% Days between Jan 1, 0001 (beginning of the Gregorian calendar) and Jan 1, 1970 (Unix epoch) in seconds.
%% 62167219200 = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(SECONDS_TO_UNIX_EPOCH, 62167219200).

%% @doc Convert a date and time as binary string in the ISO 8601 format to the
%%      number of seconds since the Unix epoch (Jan 1, 1970, 00:00:00) with
%%      millisecond precision.
-spec twitter_time_to_epoch(binary()) -> epoch().
twitter_time_to_epoch(TwitterDatetime) when is_binary(TwitterDatetime) ->
    datetime_to_epoch(twitter_time_to_datetime(TwitterDatetime));
twitter_time_to_epoch(_TwitterDatetime) ->
    null.

%% @doc Convert a datetime in the format returned by the calendar:universal_time/0 function
%%      into a timestamp as a floating-point with the number of seconds since
%%      the Unix Epoch (Jan 1, 1970, 00:00:00) and a precision of microseconds.
-spec datetime_to_epoch(datetime()) -> epoch().
datetime_to_epoch({{_Year, _Month, _Day}, {_Hour, _Min, Sec}} = Datetime) when is_integer(Sec) ->
    calendar:datetime_to_gregorian_seconds(Datetime) - ?SECONDS_TO_UNIX_EPOCH;
datetime_to_epoch({{_Year, _Month, _Day} = Date, {Hour, Min, Sec}}) when is_float(Sec) ->
    TruncatedSec = trunc(Sec),
    Subsec = round((Sec - TruncatedSec) * 1000000.0) / 1000000.0,
    float(calendar:datetime_to_gregorian_seconds({Date, {Hour, Min, TruncatedSec}}) - ?SECONDS_TO_UNIX_EPOCH) + Subsec.


%% @doc Convert a datetime in the format used by Twitter to a date and time in the
%%      format returned by calendar:universal_time/0.
-spec twitter_time_to_datetime(binary()) -> datetime().
twitter_time_to_datetime(<<_DDD:3/binary, " ", MMM:3/binary, " ", DD:2/binary, " ",
                      Hh:2/binary, $:, Mm:2/binary, $:, Ss:2/binary, " ",
                      Sign, TzHour:2/binary, TzMin:2/binary, " ", YYYY:4/binary, _Tail/binary>>) ->
    Month = month(MMM),
    Date1 = {bstr:to_integer(YYYY), Month, bstr:to_integer(DD)},
    Hour1 = bstr:to_integer(Hh),
    Min1 = bstr:to_integer(Mm),
    Sec1 = bstr:to_integer(Ss),

    if
        TzHour =:= <<"00">> andalso TzMin =:= <<"00">> ->
            {Date1, {Hour1, Min1, Sec1}};
        true ->
            LocalSec = calendar:datetime_to_gregorian_seconds({Date1, {Hour1, Min1, Sec1}}),
            %% Convert the the seconds in the local timezone to UTC.
            UtcSec = case ((bstr:to_integer(TzHour) * 3600 + bstr:to_integer(TzMin)) * 60) of
                         Offset when Sign =:= $- -> LocalSec - Offset;
                         Offset                  -> LocalSec + Offset
                     end,
            calendar:gregorian_seconds_to_datetime(UtcSec)
    end;
twitter_time_to_datetime(TwitterDatetime) ->
    null.


month(<<"Jan">>) ->  1;
month(<<"Feb">>) ->  2;
month(<<"Mar">>) ->  3;
month(<<"Apr">>) ->  4;
month(<<"Jun">>) ->  6;
month(<<"Jul">>) ->  7;
month(<<"Aug">>) ->  8;
month(<<"Sep">>) ->  9;
month(<<"Oct">>) -> 10;
month(<<"Nov">>) -> 11;
month(<<"Dec">>) -> 12.

