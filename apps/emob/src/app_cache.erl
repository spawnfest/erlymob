%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2011-2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Generic app to provide caching
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(app_cache).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-author('Tom Heinan <me@tomheinan.com>').

-compile([{parse_transform,dynarec}]).
-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

%% Mnesia utility APIs
-export([get_env/0, get_env/1, get_env/2]).
-export([setup/0, start/0, stop/0, init/0, init/1, init_metatable/1, init_table/2,
         create_tables/1, create_metatable/1, create_table/2,
         upgrade_metatable/0, upgrade_table/1, tables/0,
         table_info/1, table_version/1, table_time_to_live/1,
         last_update_to_datetime/1, current_time_in_gregorian_seconds/0,
         cache_time_to_live/1
        ]).

%% Public APIs
-export([get_data/2, get_after/2, set_data/1, remove_data/2]).
-export([test/0]).

%% ------------------------------------------------------------------
%% Includes & Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").

-define(APP, ?MODULE).
test() ->
    get_position(timestamp, test_table_2).

-spec tables() -> [#table_info{}].
tables() ->
    [
     #table_info{table = ?TEST_TABLE_1,              version = 1, time_to_live = ?TEST_TABLE_1_TTL, type = ordered_set},
     #table_info{table = ?TEST_TABLE_2,              version = 1, time_to_live = ?TEST_TABLE_2_TTL, type = set}
     #table_info{table = ?SESSION,                   version = 1, time_to_live = ?SESSION_TTL,          type = set}
    ].


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
%%
%% Environment helper functions
%%

%% @doc Retrieve all key/value pairs in the env for the specified app.
-spec get_env() -> [{Key :: atom(), Value :: term()}].
get_env() ->
    application:get_all_env(?APP).

%% @doc The official way to get a value from the app's env.
%%      Will return the 'undefined' atom if that key is unset.
-spec get_env(Key :: atom()) -> term().
get_env(Key) ->
    get_env(Key, undefined).

%% @doc The official way to get a value from this application's env.
%%      Will return Default if that key is unset.
-spec get_env(Key :: atom(), Default :: term()) -> term().
get_env(Key, Default) ->
    case application:get_env(?APP, Key) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.
%%
%% Mnesia utility functions
%%

%% @doc Start the application and all its dependencies.
-spec start() -> ok.
start() ->
    start_deps(?APP).

-spec start_deps(App :: atom()) -> ok.
start_deps(App) ->
    application:load(App),
    {ok, Deps} = application:get_key(App, applications),
    lists:foreach(fun start_deps/1, Deps),
    start_app(App).

-spec start_app(App :: atom()) -> ok.
start_app(App) ->
    case application:start(App) of
        {error, {already_started, _}} -> ok;
        ok                            -> ok
    end.


%% @doc Stop the application and all its dependencies.
-spec stop() -> ok.
stop() ->
    stop_deps(?APP).

-spec stop_deps(App :: atom()) -> ok.
stop_deps(App) ->
    stop_app(App),
    {ok, Deps} = application:get_key(App, applications),
    lists:foreach(fun stop_deps/1, lists:reverse(Deps)).

-spec stop_app(App :: atom()) -> ok.
stop_app(kernel) ->
    ok;
stop_app(stdlib) ->
    ok;
stop_app(App) ->
    case application:stop(App) of
        {error, {not_started, _}} -> ok;
        ok                        -> ok
    end.


%% @doc setup mnesia on this node for disc copies
-spec setup() -> {atomic, ok} | {error, Reason::any()}.
setup() ->
    mnesia:create_schema([node()]).

-spec init() -> ok | {aborted, Reason :: any()}.
init() ->
    init(get_env(cache_nodes, [node()])).


-spec init([node()]) -> ok | no_return(). %% exit({aborted, Reason :: any()}).
init(Nodes) ->
    try
        init_metatable(Nodes),
        lists:foreach(fun (#table_info{table = Table, version = Version, time_to_live = TimeToLive, type = Type}) ->

                          init_table(Table, Version, TimeToLive, Type, Nodes)
                  end, tables())
    catch
        _:Error ->
            lager:error("Error ~p initializing mnesia.  Did you forget to run ~p:setup()?~n", [Error, ?APP]),
            Error
    end.


-spec init_metatable([node()]) -> ok | {aborted, Reason :: any()}.
init_metatable(Nodes) ->
    Fields = fields(?METATABLE),
    %% Make sure that the schema of the Mnesia table is up-to-date.
    try ((length(Fields) + 1 =:= mnesia:table_info(?METATABLE, arity)) andalso
         (Fields -- mnesia:table_info(?METATABLE, attributes)) =:= []) of
        true ->
            ok;
        false ->
            upgrade_metatable()
    catch
        _ : _ ->
            create_metatable(Nodes)
    end.

-spec init_table(table(), [node()]) -> ok | {aborted, Reason :: any()}.
init_table(Table, Nodes) ->
    {Version, TimeToLive, Type} = table_info(Table),
    init_table(Table, Version, TimeToLive, Type, Nodes).

-spec init_table(table(), table_version(), time_to_live(), table_type(), [node()]) -> ok | {aborted, Reason :: any()}.
init_table(Table, Version, TimeToLive, Type, Nodes) ->
    Fields = fields(Table),
    OldVersion = case mnesia:dirty_read(?METATABLE, Table) of
                     [#app_metatable{version = Number}] ->
                         Number;
                     [] ->
                         Version
                 end,
    %% Make sure that the schema of the Mnesia table is up-to-date.
    try ((OldVersion =:= Version) andalso
         (length(Fields) + 1 =:= mnesia:table_info(Table, arity)) andalso
         (Fields -- mnesia:table_info(Table, attributes)) =:= []) of
        true ->
            ok;
        false ->
            upgrade_table(Table, OldVersion, Version, Fields)
    catch
        _ : _ ->
            create_table(Table, Version, TimeToLive, Type, Nodes)
    end.


-spec create_tables([node()]) -> ok | {aborted, Reason :: any()}.
create_tables(Nodes) ->
    create_metatable(Nodes),
    lists:foreach(fun (#table_info{table = Table, version = Version, time_to_live = TimeToLive, type = Type}) ->
                          create_table(Table, Version, TimeToLive, Type, Nodes)
                  end, tables()).


-spec create_metatable([node()]) -> ok | {aborted, Reason :: any()}.
create_metatable(Nodes) ->
    case mnesia:create_table(?METATABLE, [{access_mode, read_write},
                                               {record_name, ?METATABLE},
                                               {attributes, fields(?METATABLE)},
                                               {disc_copies, Nodes},
                                               {type, set}]) of
        {atomic, ok} ->
            ok;
        Error ->
            throw(Error)
    end.

-spec create_table(table(), [node()]) -> ok | {aborted, Reason :: any()}.
create_table(Table, Nodes) ->
    {Version, TimeToLive, Type} = table_info(Table),
    create_table(Table, Version, TimeToLive, Type, Nodes).

-spec create_table(table(), table_version(), time_to_live(), table_type(), [node()]) -> ok | {aborted, Reason :: any()}.
create_table(Table, Version, TimeToLive, Type, Nodes) ->
    {atomic, ok} = mnesia:create_table(Table, [{access_mode, read_write},
                                               {record_name, Table},
                                               {attributes, fields(Table)},
                                               {disc_copies, Nodes},
                                               {type, Type},
                                               {local_content, true}]),
    {atomic, ok} = mnesia:add_table_index(Table, ?TIMESTAMP),
    WriteFun = fun() -> 
            mnesia:write(#app_metatable{
                        table = Table,
                        version = Version,
                        time_to_live = TimeToLive,
                        type = Type,
                        last_update = current_time_in_gregorian_seconds(),
                        reason = create_table
                        }) end,
    {atomic, ok} = mnesia:transaction(WriteFun).


-spec upgrade_metatable() -> {atomic, ok} | {aborted, Reason :: any()}.
upgrade_metatable() ->
    upgrade_table(?METATABLE, fields(?METATABLE)).


-spec upgrade_table(table()) -> {atomic, ok} | {aborted, Reason :: any()}.
upgrade_table(Table) ->
    upgrade_table(Table, fields(Table)).


-spec upgrade_table(table(), [app_field()]) -> {atomic, ok} | {aborted, Reason :: any()}.
upgrade_table(Table, Fields) ->
    %% Replace 'ignore' with a function that performs the schema upgrade once the schema changes.
    mnesia:transform_table(Table, ignore, Fields, Table).


-spec upgrade_table(table(), OldVersion :: non_neg_integer(), NewVersion :: non_neg_integer(), [app_field()]) ->
                           {atomic, ok} | {aborted, Reason :: any()}.
upgrade_table(Table, _OldVersion, _NewVersion, Fields) ->
    %% Replace 'ignore' with a function that performs the schema upgrade once the schema changes.
    mnesia:transform_table(Table, ignore, Fields, Table).

-spec table_info(table()) -> {table_version(), time_to_live(), table_type()} | undefined.
table_info(Table) ->
    case lists:keyfind(Table, #table_info.table, tables()) of
        #table_info{version = Version, time_to_live = TimeToLive, type = Type} ->
            {Version, TimeToLive, Type};
        false ->
            undefined
    end.

-spec table_version(table()) -> table_version().
table_version(Table) ->
    case lists:keyfind(Table, #table_info.table, tables()) of
        #table_info{version = Version} ->
            Version;
        false ->
            undefined
    end.

-spec table_time_to_live(table()) -> time_to_live().
table_time_to_live(Table) ->
    case lists:keyfind(Table, #table_info.table, tables()) of
        #table_info{time_to_live = TimeToLive} ->
            TimeToLive;
        false ->
            get_env(cache_time_to_live)
    end.

-spec last_update_to_datetime(last_update()) -> calendar:datetime().
last_update_to_datetime(LastUpdate) ->
    calendar:gregorian_seconds_to_datetime(LastUpdate).

%%
%% Table accessors
%%
-spec get_data(Table::table(), Key::table_key()) -> any().
get_data(Table, Key) ->
    read_data(Table, Key).

%% Get data after (in erlang term order) a value
-spec get_after(table(), table_key()) -> any().
get_after(Table, After) ->
    read_after(Table, After).

-spec set_data(Value::any()) -> ok | error().
set_data(Value) ->
    write_data(Value).

-spec remove_data(Table::table(), Key::table_key()) -> ok | error().
remove_data(Table, Key) ->
    delete_data(Table, Key).

%%====================================================================
%% Internal functions
%%====================================================================
-spec read_data(table(), table_key()) -> list().
read_data(Table, Key) ->
    TableTTL = cache_time_to_live(Table),
    CachedData = cache_entry(Table, Key),
    filter_data_by_ttl(TableTTL, CachedData).

-spec read_after(table(), table_key()) -> list().
read_after(Table, After) ->
    MatchHead = '$1',
    Guard =  [{'>=', {element, 2, '$1'}, After}],
    Result = ['$_'],
    CachedData = mnesia:dirty_select(Table, [{MatchHead, Guard, Result}]),

    % return only the valid values
    TableTTL = cache_time_to_live(Table),
    FilteredData = lists:foldl(fun(X, Acc) ->
                case filter_data_by_ttl(TableTTL, X) of
                    [Data] ->
                        [Data|Acc];
                    _ ->
                        Acc
                end
        end, [], CachedData),
    lists:reverse(FilteredData).

-spec write_data(any()) -> ok | error().
write_data(Data) ->
    TimestampedData = timestamp_data(Data),
    WriteFun = fun () -> mnesia:write(TimestampedData) end,
    case mnesia:transaction(WriteFun) of
        {atomic, ok} ->
            ok;
        {aborted, {Reason, MData}} ->
            {error, {Reason, MData}}
    end.

-spec delete_data(Table::table(), Key::table_key()) -> ok | error().
delete_data(Table, Key) ->
    DeleteFun = fun () -> mnesia:delete({Table, Key}) end,
    case mnesia:transaction(DeleteFun) of
        {atomic, ok} ->
            ok;
        {aborted, {Reason, MData}} ->
            {error, {Reason, MData}}
    end.

-spec cache_entry(table(), table_key()) -> {last_update() | ?DEFAULT_TIMESTAMP, Data :: any() | 'undefined'}.
cache_entry(Table, Key) ->
    case mnesia:dirty_read(Table, Key) of
        [Data] ->
            Data;
        [] ->
            undefined
    end.


-spec cache_time_to_live(table()) -> time_to_live().
cache_time_to_live(Table) ->
    case mnesia:dirty_read(app_metatable, Table) of
        [#app_metatable{time_to_live = TimeToLive}] ->
            TimeToLive;
        [] ->
            get_env(cache_time_to_live, ?DEFAULT_CACHE_TTL)
    end.


-spec is_cache_valid(timestamp() | ?INFINITY, last_update() | ?DEFAULT_TIMESTAMP, timestamp()) -> boolean().
is_cache_valid(_TableTTL, ?DEFAULT_TIMESTAMP, _CurrentTime) ->
    %% There is no timestamp, so the data is not valid
    false;
is_cache_valid(?INFINITY, _LastUpdate, _CurrentTime) ->
    %% The table has in infinite TTL
    true;
is_cache_valid(TableTTL, LastUpdate, CurrentTime) ->
    LastUpdate + TableTTL > CurrentTime.

-spec current_time_in_gregorian_seconds() -> non_neg_integer().
current_time_in_gregorian_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

-spec get_last_upate(Data::any()) -> timestamp() | ?DEFAULT_TIMESTAMP.
get_last_upate(Data) ->
    try
        get_value(?TIMESTAMP, Data)
    catch
        _:_ ->
            ?DEFAULT_TIMESTAMP
    end.

-spec timestamp_data(any()) -> any().
timestamp_data(Data) ->
    CurrentTime = current_time_in_gregorian_seconds(),
    set_value(?TIMESTAMP, CurrentTime, Data).

-spec filter_data_by_ttl(TableTTL::timestamp(), Data::any()) -> any().
filter_data_by_ttl(TableTTL, Data) ->
    LastUpdate = get_last_upate(Data),
    CurrentTime = current_time_in_gregorian_seconds(),
    %% We store the datetime as seconds in the Gregorian calendar (since Jan 1, 0001 at 00:00:00).
    case is_cache_valid(TableTTL, LastUpdate, CurrentTime) of
        true ->
            [Data];
        false ->
            %% TODO: do something to trigger a cleanup
            []
    end.

