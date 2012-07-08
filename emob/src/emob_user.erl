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

-export([get_user/1]).
-export([set_callback/2]).

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
%%% USER
%% @doc Get the User profile
get_user(UserId) ->
    emob_manager:safe_call({?EMOB_USER, UserId}, {get_user}).

-spec set_callback(user_id(), target()) -> ok | error().
set_callback(UserId, Callback) ->
    emob_manager:safe_call({?EMOB_USER, UserId}, {set_callback, Callback}).

%%% POSTS
%% @doc Retrieve the latest posts from the user
get_posts(UserId) ->
    emob_manager:safe_call({?EMOB_USER, UserId}, {get_posts}).

%% @doc Process the incoming tweet
%%          sent to Target
process_post(UserId, PostId) ->
    emob_manager:safe_cast({?EMOB_USER, UserId}, {process_post, PostId}).

%% @doc Notify all the users that a new post exists
% TODO have callback users in a seperate table, efficiently notify them, etc.
notify_all_users(Post) ->
    UserFun = fun() -> mnesia:foldl(fun(X, Acc) ->
                    CallbackPid = X#user.callback,
                    twitterl:respond_to_target({process, CallbackPid}, {post, Post}),
                    Acc
                end, [], ?USER) end,
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

handle_call({get_user}, _From, State) ->
    UserId = State#state.user_id,
    User = get_user_data(UserId),
    Response = validate_user(User),
    {reply, Response, State};

handle_call({set_callback, Callback}, _From, State) ->
    UserId = State#state.user_id,
    Response = set_callback_internal(UserId, Callback),
    {reply, Response, State};

handle_call({get_posts}, _From, State) ->
    UserId = State#state.user_id,
    User = get_user_data(UserId),
    Posts = update_posts_from_cache(User),
    {reply, {ok, Posts}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({process_post, PostId}, State) ->
    UserId = State#state.user_id,
    User = get_user_data(UserId),
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

validate_user(User) ->
    if User#user.id  =/= undefined ->
            User;
        true ->
            {error, ?INVALID_USER}
    end.

-spec get_user_data(user_id()) -> #user{}.
get_user_data(UserId) ->
    case app_cache:get_data(?USER, UserId) of
        [User] ->
            User;
        _ ->
            #user{}
    end.

-spec set_callback_internal(user_id(), target()) -> #user{}.
set_callback_internal(UserId, Target) ->
    case app_cache:get_data(?USER, UserId) of
        [User] ->
            app_cache:set_data(User#user{callback = Target});
        _ ->
            {error, ?INVALID_USER}
    end.

-spec update_posts_from_cache(#user{}) -> list().
update_posts_from_cache(User) ->
    case app_cache:get_after(?POST, User#user.last_post_processed) of
        [_H|_Tail] = AllPosts ->
            SortedPosts = lists:sort(fun(A,B) -> A#post.id >= B#post.id end, AllPosts),
            LimitedPosts = lists:sublist(SortedPosts, ?MAX_POSTS),
            case LimitedPosts of
                [LastPost|_] ->
                    app_cache:set_data(User#user{last_post_processed = LastPost#post.id});
                [] ->
                    void
            end,
            LimitedPosts;
        _ ->
            []
    end.

notify_user(User, PostId) ->
    TargetPid = User#user.callback,
    case TargetPid =/= undefined of
        true ->
            Post = app_cache:get(?POST, PostId),
            twitterl:respond_to_target({process, TargetPid}, {post, Post});
        false ->
            void
    end.
