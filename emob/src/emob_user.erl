%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2011-2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Manages the user
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(emob_user).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-author('Tom Heinan <me@tomheinan.com>').

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([get_posts/1]).
-export([process_post/2]).
-export([notify_all_users/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Includes & Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").

-record(state, {
            user_id
            }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Retrieve the latest posts from the user
get_posts(UserId) ->
    emob_manager:safe_call({?EMOB_USER, UserId}, {get_posts}).

%% @doc Process the incoming tweet
%%          sent to Target
process_post(UserId, PostId) ->
    emob_manager:safe_cast({?EMOB_USER, UserId}, {process_post, PostId}).

%% @doc Notify all the users that a new post exists
notify_all_users(PostId) ->
    Post = app_cache:get_data(?POST, PostId),
    UserFun = mnesia:foldl(fun(X, Acc) ->
                    CallbackPid = X#user.callback_pid,
                    case is_process_alive(CallbackPid) of
                        true ->
                            CallbackPid ! {post, Post},
                            Acc;
                        false ->
                            Acc
                    end end, [], ?USER),
    mnesia:transaction(UserFun).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

start_link(UserId) ->
    gen_server:start_link(?MODULE, [UserId], []).

init([UserId]) ->
    emob_manager:register_process(?EMOB_USER, UserId),
    % get data from the cache
    State = #state{user_id = UserId},
    {ok, State}.

handle_call({get_posts}, _From, State) ->
    UserId = State#state.user_id,
    User = get_user(UserId),
    Posts = update_posts_from_cache(User),
    {reply, {ok, Posts}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({process_post, PostId}, State) ->
    UserId = State#state.user_id,
    User = get_user(UserId),
    notify_user(User, PostId),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec get_user(user_id()) -> #user{}.
get_user(UserId) ->
    case app_cache:get_data(?USER, UserId) of
        [User] ->
            User;
        _ ->
            #user{}
    end.

-spec update_posts_from_cache(#user{}) -> list().
update_posts_from_cache(User) ->
    case app_cache:get_after(User#user.last_post_processed) of
        [_|_] ->
            AllPosts = app_cache:get_after(User#user.last_post_processed),
            SortedPosts = lists:reverse(lists:keysort(2, AllPosts)),
            LimitedPosts = lists:sublist(SortedPosts, ?MAX_POSTS),
            [LastPost|_] = LimitedPosts,
            app_cache:set_data(?USER, User#user{last_post_processed = LastPost#post.id}),
            LimitedPosts;
        _ ->
            []
    end.

notify_user(User, PostId) ->
    TargetPid = User#user.callback_pid,
    case is_process_alive(TargetPid) of
        true ->
            Post = app_cache:get(?POST, PostId),
            TargetPid ! {post, Post};
        false ->
            undefined
    end.
