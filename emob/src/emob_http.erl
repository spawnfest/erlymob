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
    {Value, Req} = cowboy_http_req:qs_val(Req0, ?TOKEN),
    Posts = case Value of
                %% /mobs
                undefined ->
                    emob_post_receiver:get_all_posts();
                %% /mobs?token=:token
                Token ->
                    emob_user:get_posts(Token)
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
      {<<"when">>, <<>>},
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
    ejson:encode([post_to_ejson(Post) || Post <- Posts]).
