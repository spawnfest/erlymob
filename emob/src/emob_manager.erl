%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2011-2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc emob manager
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(emob_manager).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-author('Tom Heinan <me@tomheinan.com>').


-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([get_process/1]).
-export([register_process/2]).
-export([safe_sync_send_event/2, safe_sync_send_event/3]).
-export([safe_cast/2, safe_call/2, safe_call/3]).

%%
%% For testing
%%
-export([start_oauth_fsm/0, stop_oauth_fsm/1]).


%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------

-include("defaults.hrl").

%% ------------------------------------------------------------------
%% Defines
%% ------------------------------------------------------------------


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Register a process locally
-spec register_process(Type::atom(),Name:: atom()|binary() | reference()) -> true.
register_process(Type, Name) ->
    gproc:reg({p, l, Type}, Name).

%% @doc Get the Pid for a given process type, or {Type, Name}
-spec get_process({Type::atom(), Name::atom()} | atom()) -> pid() | error().
get_process(Key) ->
    get_child_pid(Key).

-spec start_oauth_fsm() -> {ok, pid()} | error().
start_oauth_fsm() ->
    emob_oauth_fsm_sup:start_oauth_fsm().

-spec stop_oauth_fsm(binary()) -> ok | error().
stop_oauth_fsm(Token) ->
    case get_process({?OAUTH_FSM, Token}) of 
        Pid when is_pid(Pid) ->
            emob_oauth_fsm_sup:terminate_child(Pid);
        _ ->
            {error, ?GPROC_UNKNOWN_PROCESS}
    end.


%% @doc Unified mechanism to send a gen_server call request to a supervised process
-spec safe_call({Type :: atom(), Name :: atom()} | atom(), Request :: any()) -> {ok, pid()} | {ok, Result :: any(), pid()} | error().
safe_call({_Type, _Name} = Key, Request) ->
    safe_call(Key, Request, ?DEFAULT_TIMER_TIMEOUT);

safe_call(Type, Request) ->
    safe_call(Type, Request, ?DEFAULT_TIMER_TIMEOUT).


-spec safe_call({Type :: atom(), Name :: atom()} | atom(), Request::any(), timeout()) -> {ok, pid()} | {ok, Result :: any(), pid()} | error().
safe_call({Type, Name}, Request, Timeout) ->
    % Send the request to the process
    case get_process({Type, Name}) of 
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, Request, Timeout);
        _ ->
            if Type =:= ?EMOB_USER ->
                    {ok, Target} = emob_user_sup:start_user(Name),
                    gen_server:call(Target, Request);
                true -> 
                    {error, ?GPROC_UNKNOWN_PROCESS}
            end
    end;

safe_call(Type, Request, Timeout) ->
    % Send the request to the process
    case get_process(Type) of 
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, Request, Timeout);
        _ ->
            {error, ?GPROC_UNKNOWN_PROCESS}
    end.

%% @doc Unified mechanism to send a gen_server cast request to a supervised process
-spec safe_cast({Type :: atom(), Name :: atom()} | atom(), Request :: any()) -> ok | error().
safe_cast({Type, Name}, Request) ->
    % Send the request to the process
    case get_process({Type, Name}) of 
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, Request);
        _ ->
            if Type =:= ?EMOB_USER ->
                    {ok, Target} = emob_user_sup:start_user(Name),
                    gen_server:cast(Target, Request);
                true -> 
                    {error, ?GPROC_UNKNOWN_PROCESS}
            end
    end;

safe_cast(Type, Request) ->
    % Send the request to the process
    case get_process(Type) of 
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, Request);
        _ ->
            {error, ?GPROC_UNKNOWN_PROCESS}
    end.

%% @doc Unified mechanism to send a gen_fsm request to a supervised fsm
-spec safe_sync_send_event({atom(), binary()} , Request :: any()) -> {ok, pid()} | {ok, Result :: any(), pid()} | error().
safe_sync_send_event({_Type, _Name} = Key, Request) ->
    safe_sync_send_event(Key, Request, ?DEFAULT_TIMER_TIMEOUT);

safe_sync_send_event(Pid, Request) when is_pid(Pid) ->
    safe_sync_send_event(Pid, Request, ?DEFAULT_TIMER_TIMEOUT).

-spec safe_sync_send_event({atom(), binary()} , Request::any(), timeout()) -> {ok, pid()} | {ok, Result :: any(), pid()} | error().
safe_sync_send_event({Type, Name}, Request, Timeout) ->
    % Send the request to the fsm
    case get_process({Type, Name}) of 
        Pid when is_pid(Pid) ->
            gen_fsm:sync_send_event(Pid, Request, Timeout);
        _ ->
            {error, ?GPROC_UNKNOWN_PROCESS}
    end;

safe_sync_send_event(Pid, Request, Timeout) ->
    gen_fsm:sync_send_event(Pid, Request, Timeout).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


get_child_pid({Type, Name}) ->
    % If the process was started, it will be registered in gproc
    case gproc:select({local, all}, [{{{p, l, Type}, '$1', Name}, [], ['$1']}]) of
        [Pid] when is_pid(Pid) ->
            Pid;
        [] ->
            {error, ?GPROC_UNKNOWN_PROCESS}
    end.
