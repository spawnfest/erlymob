%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2011-2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc oauth authentication fsm
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(emob_oauth_fsm).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-author('Tom Heinan <me@tomheinan.com>').

-compile([{parse_transform, lager_transform}]).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([get_request_token/1,
         get_access_token/2,
         get_credentials/1,
         remove_credentials/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, 
         unauthorized/2, unauthorized/3, 
         token_requested/2, token_requested/3, 
         handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% ------------------------------------------------------------------
%% Includes and Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").

-record(state, {
            token                                        :: binary(),
            secret                                       :: binary(),
            access_data = #twitter_access_data{}         :: #twitter_access_data{}
            }).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_fsm:start_link(?MODULE, [], []).

%% @doc Get a request token.
-spec get_request_token(url()) -> #twitter_token_data{} | error().
get_request_token(CallbackURL) ->
    {ok, Pid} = start_authorizer(),
    lager:debug("Pid:~p~n", [Pid]),
    request_token(Pid, CallbackURL).

           
%% @doc Get an access token.
-spec get_access_token(token(), verifier()) -> #twitter_access_data{} | error().
get_access_token(Token, Verifier) ->
    lager:debug("Token:~p, Verifier:~p~n", [Token, Verifier]),
    case authorize_user(Token, Verifier) of
        {ok, AccessData} ->
            store_credentials(AccessData),
            % Clobber the gen_fsm, 'cos you don't need it any more
            stop_authorizer(Token),
            AccessData;
        Error ->
            lager:debug("error:~p~n", [Error]),
            Error
    end.

%% @doc Get stored credentials. Returns #access_data{}
-spec get_credentials(token()) -> #twitter_access_data{} | error().
get_credentials(Token) ->
    case get_user_from_token(Token) of
        [User] when is_record(User, ?USER) ->
            get_credentials_from_user(User);
        _ ->
            {error, ?INVALID_USER}
    end.

%% @doc Remove stored credentials
-spec remove_credentials(token()) -> ok.
remove_credentials(Token) ->
    app_cache:remove_data(?USER, Token),
    ok.

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    State = #state{},
    {ok, unauthorized, State}.

unauthorized(_Event, State) ->
    {next_state, unauthorized, State}.

unauthorized({request_token, CallbackURL}, _From, OldState) ->
    case twitterl:get_request_token(CallbackURL) of
        TokenData when is_record(TokenData, twitter_token_data) ->
            Token = TokenData#twitter_token_data.access_token,
            emob_manager:register_process(?OAUTH_FSM, Token),
            TokenData = #twitter_token_data{
                    access_token = TokenData#twitter_token_data.access_token,
                    access_token_secret = TokenData#twitter_token_data.access_token_secret},
            State = #state{
                    token = TokenData#twitter_token_data.access_token,
                    secret = TokenData#twitter_token_data.access_token_secret},
            {reply, TokenData, token_requested, State};
        Error ->
            {reply, {error, Error}, unauthorized, OldState}
    end.

token_requested(_Event, State) ->
    {next_state, token_requested, State}.

token_requested({authorize, Verifier}, _From, State) ->
    lager:debug("a, Verifier:~p~n, State:~p~n", [Verifier, State]),
    Token = State#state.token,
    Secret = State#state.secret,
    case twitterl:get_access_token(Verifier, Token, Secret) of
        AccessData when is_record(AccessData, twitter_access_data) ->
            lager:debug("1:~n~n~n~n~n", []),
            NewState = State#state{access_data = AccessData},
            lager:debug("NewState:~p~n", [NewState]),
            {stop, normal, {ok, AccessData}, NewState};
        _ ->
            lager:debug("2:~n~n~n~n~n", []),
            {reply, {error, invalid_token_data}, token_requested, State}
    end;

token_requested(_Event, _From, State) ->
    lager:debug("a1, Event:~p~n, State:~p~n", [_Event, State]),
    {reply, {error, invalid_event}, token_requested, State}.

handle_event(_Event, StateName, State) ->
    lager:debug("aa, Event:~p~n, StateName:~p~n, State:~p~n", [_Event, StateName, State]),
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    lager:debug("bb, Event:~p~n, StateName:~p~n, State:~p~n", [_Event, StateName, State]),
  {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    lager:debug("cc, Info:~p~n, StateName:~p~n, State:~p~n", [_Info, StateName, State]),
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    lager:debug("ending:~p~n", [_Reason]),
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_authorizer() ->
    emob_manager:start_oauth_fsm().

stop_authorizer(Token) ->
    lager:debug("stop:~p~n", [Token]),
    emob_manager:stop_oauth_fsm(Token).

request_token(Pid, CallbackURL) ->
    lager:debug("CallbackURL:~p~n", [CallbackURL]),
    emob_manager:safe_sync_send_event(Pid, {request_token, CallbackURL}).

authorize_user(Token, Verifier) ->
    lager:debug("Token:~p~n, Verifier:~p~n", [Token, Verifier]),
    emob_manager:safe_sync_send_event({?OAUTH_FSM, Token}, {authorize, Verifier}).

store_credentials(AccessData) ->
    lager:debug("AccessData:~p~n", [AccessData]),
    User = #user{id = AccessData#twitter_access_data.user_id,
                 access_token = AccessData#twitter_access_data.access_token,
                 access_token_secret = AccessData#twitter_access_data.access_token_secret,
                 screen_name = AccessData#twitter_access_data.screen_name,
                 user_id = AccessData#twitter_access_data.user_id},
    lager:debug("User:~p~n", [User]),
    app_cache:set_data(User).

get_user_from_token(Token) ->
    app_cache:get_data_from_index(?USER, Token, ?USER_ACCESS_TOKEN).

get_credentials_from_user(User) ->
    #twitter_access_data{
        access_token = User#user.access_token,
        access_token_secret = User#user.access_token_secret,
        screen_name = User#user.screen_name,
        user_id = User#user.user_id}.

