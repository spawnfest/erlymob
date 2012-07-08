%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2011-2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Processing when each post is received
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(emob_post_receiver).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-author('Tom Heinan <me@tomheinan.com>').

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([process_post/1]).

% Testing
-export([get_post/1]).
-export([get_rsvps/1]).
-export([get_ignores/1]).
-export([get_all_posts/0]).
-export([empty_posts/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Includes & Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").

-record(post_receiver_state, {
            stream_pid
            }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


%% @doc Process the incoming tweet
%%          sent to Target
-spec process_post(#post{}) -> ok.
process_post(Post) ->
    emob_manager:safe_cast({?EMOB_POST_RECEIVER, ?EMOB_POST_RECEIVER}, {process_post, Post}).

-spec get_post(post_id()) -> #post{} | undefined | error().
get_post(PostId) ->
    case app_cache:get_data(?POST, PostId) of
        [Post] ->
            Post;
        _ ->
            undefined
    end.

-spec get_rsvps(post_id()) -> [user_id()] | error().
get_rsvps(PostId) ->
    case app_cache:get_bag_data(?POST_RSVP, PostId) of
        [] ->
            [];
        Rsvps ->
            [X#post_rsvp.rsvp_user || X <- Rsvps]
    end.

-spec get_ignores(post_id()) -> [user_id()] | error().
get_ignores(PostId) ->
    case app_cache:get_bag_data(?POST_IGNORE, PostId) of
        [] ->
            [];
        Ignores ->
            [X#post_ignore.ignore_user || X <- Ignores]
    end.


-spec get_all_posts() -> [#post{}] | error().
get_all_posts() ->
    app_cache:get_after(?POST, ?FIRST_POST).

-spec empty_posts() -> {atomic, ok} | error().
empty_posts() ->
    mnesia:clear_table(?POST).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

start_link(Token, Secret) ->
    gen_server:start_link(?MODULE, [Token, Secret], []).

init([Token, Secret]) ->
    process_flag(trap_exit, true),
    emob_manager:register_process(?EMOB_POST_RECEIVER, ?EMOB_POST_RECEIVER),
    DestPid = self(),
    process_tweets(DestPid, Token, Secret),

    State = #post_receiver_state{},
    {ok, State}.



handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({process_post, Tweet}, State) ->
    % TODO error?
    lager:debug("2,process_post  Tweet:~p~n", [Tweet]),
    PostId = Tweet#tweet.id,
    case app_cache:key_exists(?POST, PostId) of
        false ->
            PostRecord = #post{
                    id = PostId,
                    post_data = Tweet},
            app_cache:set_data(PostRecord),
            respond_to_post(Tweet),
            emob_post_distributor:distribute_post(PostId);
        true ->
            ok
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Get the tweet from twitterl
handle_info(Tweet, State) when is_record(Tweet, tweet) ->
    process_post(Tweet),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec process_tweets(pid(), token(), secret()) -> ok | pid().
process_tweets(DestPid, Token, Secret) ->
    SinceId =
    case app_cache:get_last_data(?POST) of
        [] ->
            ?FIRST_POST;
        [Post] ->
            Post#post.id
    end,
    SSinceId = emob_util:get_string(SinceId),
    proc_lib:spawn_link(fun() ->
                %% TODO fix this so this happens only after init is completed
                timer:sleep(?STARTUP_TIMER),
                twitterl:statuses_home_timeline({process, DestPid}, [{"since_id", SSinceId}], Token, Secret),
                %% TODO fill hole between these two requests
                twitterl:statuses_user_timeline_stream({process, DestPid}, [], Token, Secret)
        end).

respond_to_post(Tweet) ->
    Id = Tweet#tweet.id_str,
    UserId = (Tweet#tweet.user)#twitter_user.id,
    ScreenName = (Tweet#tweet.user)#twitter_user.screen_name,
    SScreenName = emob_util:get_string(ScreenName),
    DefaultUser = twitterl:get_env(default_user_id, <<"undefined">>),
    Token = twitterl:get_env(oauth_access_token, <<"undefined">>),
    Secret = twitterl:get_env(oauth_access_token_secret, <<"undefined">>),
    if UserId =:= DefaultUser ->
            void;
        true ->
            ResponseHash = "@" ++ SScreenName ++ "  erlymobaaaa",
            lager:debug("ResponseHash:~p, Id:~p, Token:~p, Secret:~p~n", [ResponseHash, Id, Token, Secret]),
            Result = twitterl:statuses_update({debug, foo}, [{"status", ResponseHash}, {"in_reply_to_status_id", Id}], Token, Secret),
            lager:debug("Foo:~p~n", [Result]),
            Result
    end.
