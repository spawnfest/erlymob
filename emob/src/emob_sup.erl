%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya, Tom Heinan
%%% @doc Main supervisor for the Erlymob server
%%% @end
%%%-------------------------------------------------------------------
-module(emob_sup).
-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-author('Tom Heinan <me@tomheinan.com>').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(SUPERVISOR(Id, Type, Module, Args), {Id, {Module, start_link, Args}, permanent, infinity, Type, [Module]}).
-define(CHILD(Id, Type, Module, Args), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).



%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init(_Args::any()) -> {ok, any()} | {ok, any(), timeout()} | ignore | {stop, Reason :: any()}.
init([]) ->
    Receiver = ?SUPERVISOR(make_ref(), supervisor, emob_post_receiver_sup, []),
    Distributor = ?SUPERVISOR(make_ref(), supervisor, emob_post_distributor_sup, []),
    User = ?SUPERVISOR(make_ref(), supervisor, emob_user_sup, []),
    {ok, { {one_for_one, 5, 300}, [Receiver, Distributor, User]}}.
