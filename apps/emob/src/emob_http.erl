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
    cowboy_http_req:reply(404, Req).   %% not found


%% @doc Retrieve an entity.
-spec handle_get(Path :: cowboy_dispatcher:tokens(), http_req(), #state{}) -> {ok, http_req()}.
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
    cowboy_http_req:reply(404, Req).   %% not found


-spec ejson_to_proplist({[{Name :: binary(), Value :: binary()}]}) -> proplists:proplist().
ejson_to_proplist({Doc}) ->
    ejson_to_proplist(Doc, []).

ejson_to_proplist([{Name, Value} | Tail], Acc) ->
    %% FIXME We should be using binary_to_existing_atom/2 to convert the field
    %%       names into atoms. Before we do that we must ensure that all the
    %%       possible valid atoms are already in the atom table.
    %% ejson_to_proplist(Tail, [{binary_to_existing_atom(Name, utf8), Value} | Acc]);
    ejson_to_proplist(Tail, [{binary_to_atom(Name, utf8), Value} | Acc]);
ejson_to_proplist([], Acc) ->
    lists:reverse(Acc).


-spec printable_peer(Req :: term()) -> string().
printable_peer(Req) ->
    {{{I1, I2, I3, I4}, Port}, _} = cowboy_http_req:peer(Req),
    lists:flatten(io_lib:format("~w.~w.~w.~w:~w", [I1, I2, I3, I4, Port])).


-spec printable_path(Req :: term()) -> string().
printable_path(Req) ->
    {Path, _} = cowboy_http_req:raw_path(Req),
    Path.
