%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2011-2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Processing when each post is distributed
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(emob_post_distributor).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-author('Tom Heinan <me@tomheinan.com>').

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([distribute_post/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Includes & Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


%% @doc Process the incoming tweet
%%          sent to Target
distribute_post(PostId) ->
    emob_manager:safe_cast({?EMOB_POST_DISTRIBUTOR, ?EMOB_POST_DISTRIBUTOR}, {distribute_post, PostId}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    emob_manager:register_process(?EMOB_POST_DISTRIBUTOR, ?EMOB_POST_DISTRIBUTOR),
    State = {},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({distribute_post, PostId}, State) ->
    {IsProcessed, Post} = get_processed_status_and_post(PostId),
    case IsProcessed of
        true ->
            ok;
        false ->
            send_post_to_users(Post),
            set_post_to_processed(Post)
    end,
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
-spec get_processed_status_and_post(post_id()) -> {post_processed_status(), #post{}}.
get_processed_status_and_post(PostId) ->
    case app_cache:get_data(?POST, PostId) of
        [Post] ->
            {Post#post.processed, Post};
        _ ->
            {false, #post{}}
    end.

-spec send_post_to_users(#post{}) -> ok | error().
send_post_to_users(Post) ->
    emob_user:notify_all_users(Post).

-spec set_post_to_processed(#post{}) -> ok | error().
set_post_to_processed(Post) ->
    app_cache:set_data(Post#post{processed = true}).
