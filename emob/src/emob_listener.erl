%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya, Tom Heinan
%%% @doc Erlymob HTTP listener.
%%% @end
%%%-------------------------------------------------------------------
-module(emob_listener).
-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-author('Tom Heinan <me@tomheinan.com>').

-compile([{parse_transform, lager_transform}]).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0, stop/1]).

%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------

-include("defaults.hrl").

%% ------------------------------------------------------------------
%% Defines
%% ------------------------------------------------------------------

-define(DEFAULT_LISTENERS, [{http_listener, [{transport, tcp}, {port, 8080}]}]).
-define(DEFAULT_ACCEPTOR_COUNT, 100).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Start the HTTP listeners. The listener processes are already supervised
%%      by the cowboy application, so there's no need to add them as children of
%%      this application's supervisor.
-spec start() -> ok.
start() ->
    Dispatch = [
                %% {Host, [{Path, Handler, Opts}]}
		{'_', [
                       {[<<"mobs">>, '_'], emob_http, []},        %% return the list of upcoming mobs
                       {[<<"mob">>, '_'], emob_http, []},         %% return information for an individual mob
                       {[<<"rsvp">>, '_'], emob_http, []},        %% inform that a user will be attending a mob
                       {[<<"get_loc">>, '_'], emob_http, []},     %% return the currently logged in user's most recent location
                       {[<<"set_loc">>, '_'], emob_http, []},     %% set the currently logged in user's location
                       {[<<"get_request_token">>], emob_http, []},     %% get a request token for the user to initiate oauth
                       {[<<"get_access_token">>], emob_http, []},     %% get the oauth access credentials for the user
                       {[<<"get_credentials">>], emob_http, []},     %% get the oauth access credentials for the user
                       {[<<"remove_credentials">>], emob_http, []},     %% get the oauth access credentials for the user
                       {'_', default_handler, []}
                      ]}
               ],
    Listeners     = emob:get_env(http_listeners, ?DEFAULT_LISTENERS),
    AcceptorCount = emob:get_env(http_acceptor_count, ?DEFAULT_ACCEPTOR_COUNT),
    lists:foreach(
      fun ({Name, Options}) ->
              {Transport, TransportOptions} = transport_options(Options),
              lager:info("Starting HTTP listener with options ~p~n", [Options]),
              cowboy:start_listener(Name, AcceptorCount,
                                    Transport, TransportOptions,
                                    cowboy_http_protocol, [{dispatch, Dispatch}])
      end, Listeners).


-spec stop(Name :: atom()) -> ok | emob_error().
stop(Name) ->
    cowboy:stop_listener(Name).


-spec transport_options(Options :: proplists:proplist()) -> {module(), proplists:proplist()}.
transport_options(Options) ->
    case lists:keytake(transport, 1, Options) of
        {value, {transport, tcp}, TransportOptions} -> {cowboy_tcp_transport, TransportOptions};
        {value, {transport, ssl}, TransportOptions} -> {cowboy_ssl_transport, TransportOptions};
        false                                       -> {cowboy_tcp_transport, Options}
    end.
