%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (c) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc emob oauth fsm supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(emob_oauth_fsm_sup).
-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_oauth_fsm/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Type, Module, Args), {Id, {Module, start_link, Args}, temporary, 5000, Type, [Module]}).



%% ===================================================================
%% API functions
%% ===================================================================
start_oauth_fsm() ->
    supervisor:start_child(?MODULE, []).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    OAuthFsm = ?CHILD(make_ref(), worker, emob_oauth_fsm, []),
    {ok, { {simple_one_for_one, 5, 300}, [OAuthFsm]} }.
