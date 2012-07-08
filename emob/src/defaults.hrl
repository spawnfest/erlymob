%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya, Tom Heinan
%%% @doc Common header files and definitions.
%%% @end
%%%-------------------------------------------------------------------

-include("types.hrl").

%% EMOB DEFINES
-define(EMOB_POST_RECEIVER, emob_post_receiver).
-define(EMOB_POST_DISTRIBUTOR, emob_post_distributor).
-define(EMOB_USER, emob_user).
-define(LATEST, <<"latest">>).
-define(INVALID_BOOLEAN, <<"invalid_boolean">>).
-define(INVALID_BINARY, <<"invalid_binary">>).
-define(INVALID_STRING, <<"invalid_string">>).
-define(EMPTY_ERROR, <<"empty_error">>).
-define(STARTUP_TIMER, 5000).
-define(MAX_POSTS, 200).


%% TWITTERL DEFINES
-define(CALLBACK_URL, <<"callback_url">>).
-define(TOKEN, <<"token">>).
-define(OAUTH_TOKEN, <<"oauth_token">>).
-define(SECRET, <<"secret">>).
-define(OAUTH_VERIFIER, <<"oauth_verifier">>).
-define(USER_ID, <<"user_id">>).
-define(SCREEN_NAME, <<"screen_name">>).
-define(API_VERSION, <<"1.0">>).

-define(INVALID_SESSION, <<"invalid_session">>).
-define(GPROC_UNKNOWN_PROCESS, <<"gproc_unknown_process">>).
-define(INVALID_TOKEN_DATA, <<"invalid_token_data">>).

-define(DEFAULT_TIMER_TIMEOUT, 5000).
-define(OAUTH_FSM, oauth_fsm).

-define(FIRST_POST, 1).

%% APP_CACHE DEFINES
-define(INFINITY,     infinity).
-define(META_VERSION, 1).
-define(TEST_TABLE_1_TTL,     60).
-define(TEST_TABLE_2_TTL,     160).
-define(SESSION_TTL,     ?INFINITY).
-define(POST_TTL,        ?INFINITY).
-define(USER_TTL,        ?INFINITY).

-define(SCAVENGE_FACTOR, 5*1000).       %% 1000 'cos of microseconds

-define(DEFAULT_CACHE_TTL, ?INFINITY).
-define(TIMESTAMP,     timestamp).
-define(DEFAULT_TIMESTAMP, undefined).

-define(METATABLE, app_metatable).

%% TWITTER SPECIFIC RECORDS

-record(twitter_token_data, {
            access_token            :: binary(),
            access_token_secret     :: binary()
            }).

-record(twitter_access_data, {
            access_token            :: binary(),
            access_token_secret     :: binary(),
            user_id                 :: binary(),
            screen_name             :: binary()
            }).

-record(oauth_step_1, {
            access_token            :: binary(),
            access_token_secret     :: binary()
            }).

-record(oauth_step_2, {
            access_token            :: binary(),
            access_verifier         :: binary()
            }).

%% Twitter

-record(bounding_box, {
            type          :: binary(),
            coordinates   :: list()
            }).

-record(twitter_place, {
            id            :: binary(),
            url           :: binary(),
            place_type    :: binary(),
            name          :: binary(),
            full_name     :: binary(),
            country_code  :: binary(),
            country       :: binary(),
            bounding_box  :: #bounding_box{}
            }).

-record(twitter_user, {
            id_str        :: binary(),
            id            :: integer(),
            name          :: binary(),
            screen_name   :: binary(),
            location      :: any(),
            description   :: any(),
            profile_image_url   :: any()
            }).

-record(entity_url, {
            url           :: binary(),
            expanded_url  :: binary(),
            display_url   :: binary()
            }).

-record(entities, {
            hashtags      :: list(),
            urls          :: list()
            }).
            
-record(tweet, {
            id_str        :: binary(),
            id            :: integer(),
            text          :: binary(),
            coordinates   :: any(),
            place         :: any(),
            created_at    :: any(),
            user          :: #twitter_user{},
            entities      :: #entities{}
            }).




%% APP_CACHE TABLES and records
%% All tables must have a field called timestamp. Somewhere. For sure.
-define(TEST_TABLE_1, test_table_1).
-record(test_table_1, {
          key                                       :: binary(),
          timestamp                                 :: timestamp(),
          value                                     :: binary()
         }).

-define(TEST_TABLE_2, test_table_2).
-record(test_table_2, {
          id                                        :: binary(),
          timestamp                                 :: timestamp(),
          name                                      :: binary(),
          pretty_name                               :: binary()
         }).

-define(SESSION, session).
-record(session, {
          id                                        :: binary(),
          timestamp                                 :: timestamp(),
          value                                     :: any(),
          last_processed                            :: integer()
         }).

-define(POST, post).
-record(post, {
          id                                        :: twitter_id(),
          timestamp                                 :: timestamp(),
          location                                  :: any(),
          post_data                                 :: #tweet{},
          processed                                 :: boolean()
         }).

-define(USER, user).
-record(user, {
          id                                        :: twitter_id(),
          timestamp                                 :: timestamp(),
          location                                  :: any(),
          last_post_processed = ?FIRST_POST         :: integer(),
          callback_pid                              :: callback_pid()
         }).


%% APP_CACHE METADATA
-record(app_metatable, {
          table                                             :: table(),
          version                                           :: table_version(),
          time_to_live                                      :: time_to_live(),    %% in seconds
          type                                              :: table_type(),
          last_update                                       :: non_neg_integer(),
          reason                                            :: any()
         }).

-record(table_info, {
          table                                  :: table(),
          version                                :: table_version(),
          time_to_live                           :: time_to_live(),
          type                                   :: table_type()
         }).


