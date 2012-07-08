%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya, Tom Heinan
%%% @doc Common type definitions.
%%% @end
%%%-------------------------------------------------------------------

-type error()                             :: {error, Reason :: term()}.
-type emob_error()                        :: error().

%% app_cache
-type table()           :: atom().
-type table_key()       :: any().
-type table_key_position()    :: non_neg_integer().
-type table_version()   :: non_neg_integer().
-type table_type()      :: atom().
-type timestamp()       :: non_neg_integer().
-type last_update()     :: timestamp().
-type time_to_live()    :: non_neg_integer().
-type index_fields()    :: [table_key()].
-type index_field_positions()    :: [table_key_position()].
-type target()          :: twitterl:target().

-type app_field()                       :: atom().

%% Twitter
-type token()                                 :: binary().
-type secret()                                :: binary().
-type screen_name()                           :: binary().
-type profile_picture()                       :: binary().
-type user_id()                               :: binary().
-type status()                                :: binary().
-type verifier()                              :: binary().
-type url()                                   :: binary().
-type method()                                :: atom().
-type string_method()                         :: string().
-type params()                                :: list().
-type twitter_id()                            :: integer().
-type consumer()                              :: {string(), string(), atom()}.

%% emob
-type callback_pid()                          :: pid() | atom().
-type post_id()                               :: integer().
-type post_processed_status()                 :: boolean().


