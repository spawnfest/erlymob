%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2011-2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Main module for the timer2 application.
%%% @end
%%%-------------------------------------------------------------------
-module(app_cache_tests).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------

-include("../src/app_cache.hrl").
-include("../src/defaults.hrl").
-include_lib("eunit/include/eunit.hrl").


%% ------------------------------------------------------------------
%% Defines
%% ------------------------------------------------------------------

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define(foreach(F), {foreach, fun start/0, fun stop/1, F}).

%% ------------------------------------------------------------------
%% Test Function Definitions
%% ------------------------------------------------------------------

%%
%% Test Descriptions
%%
set_data_once_test_() ->
    [{"A single record is inserted",
     ?setup(fun t_set_data_once/1)}].

set_and_delete_data_once_test_() ->
    [{"A single record is inserted and deleted",
     ?setup(fun t_set_and_delete_data_once/1)}].

set_and_get_data_once_test_() ->
    [{"A single record is inserted and read",
     ?setup(fun t_set_and_get_data_once/1)}].

set_and_get_data_many_test_() ->
    [{"many record is inserted and read",
     ?setup(fun t_set_and_get_data_many/1)}].

time_set_and_get_data_many_test_() ->
    [{"loads and gets are timed",
     ?setup(fun t_time_set_and_get_data_many/1)}].

cache_expiration_test_() ->
    [{"cache expiration",
     ?setup(fun t_cache_expiration/1)}].

%%
%% Setup Functions
%%
start() ->
    app_cache:setup(),
    app_cache:start().


stop(_) ->
    mnesia:delete_table(?TEST_TABLE_1),
    app_cache:stop(),
    mnesia:delete_schema([node()]).



%%
%% Helper Functions
%%
t_set_data_once(_In) ->
    Record = #test_table_1{key = 1, value = foo},
    Result = app_cache:set_data(Record),
    ?_assertEqual(Result, ok).

t_set_and_delete_data_once(_In) ->
    Record = #test_table_1{key = 1, value = foo},
    ok = app_cache:set_data(Record),
    Result = app_cache:remove_data(test_table_1, 1),
    ?_assertEqual(Result, ok).

t_set_and_get_data_once(_In) ->
    Record = #test_table_1{key = 1, value = foo},
    app_cache:set_data(Record),
    [Data] = app_cache:get_data(?TEST_TABLE_1, 1),
    ?_assertEqual(Data#test_table_1.value, foo).

t_set_and_get_data_many(_In) ->
    LoadFun = get_load_data_fun(100),
    LoadFun(),
    [Data] = app_cache:get_data(?TEST_TABLE_1, 35),
    ?_assertEqual(Data#test_table_1.value, {35}).

t_time_set_and_get_data_many(_In) ->
    LoadFun = get_load_data_fun(10000),
    {LTime, _LValue} = timer:tc(LoadFun),
    ?debugFmt("LoadTime:~p~n", [LTime]),
    GetFun = fun() -> app_cache:get_after(?TEST_TABLE_1, 9900) end,
    {GTime, _GValue} = timer:tc(GetFun),
    ?debugFmt("GetTime:~p~n, GetValue:~p~n", [GTime, _GValue]),
    [Data] = app_cache:get_data(?TEST_TABLE_1, 35),
    ?_assertEqual(Data#test_table_1.value, {35}).

t_cache_expiration(_In) ->
    update_table_ttl(?TEST_TABLE_1, 5),
    app_cache_scavenger:reset_timer(?TEST_TABLE_1),
    LoadFun = get_load_data_fun(10000),
    {_LTime, _LValue} = timer:tc(LoadFun),
    timer:sleep(10000),
    Data1 = app_cache:get_after(?TEST_TABLE_1, 0),
    ?_assertEqual(Data1, []).

get_load_data_fun(Count) ->
    fun() -> 
            lists:map(fun(X) ->
                        Record = #test_table_1{key = X, value = {X}},
                        app_cache:set_data(Record)
                end, lists:seq(1,Count)) 
    end.
update_table_ttl(Table, TimeToLive) ->
    UpdateFun = fun() -> 
            [OldRecord] = mnesia:read(?METATABLE, Table),
            NewRecord = OldRecord#app_metatable{
                    table = Table,
                    time_to_live = TimeToLive,
                    last_update = app_cache:current_time_in_gregorian_seconds(),
                    reason = update_ttl
                    }, 
    ?debugFmt("NewRecord:~p~n", [NewRecord]),
            mnesia:write(NewRecord)
    end,
    {atomic, ok} = mnesia:transaction(UpdateFun).










