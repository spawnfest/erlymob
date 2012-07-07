-module(emob_ui_user).
-export([check_auth/1]).

check_auth(SessionID) ->
	case boss_session:get_session_data(SessionID, access_token) of
		undefined ->
			{ok, false};
		_ ->
			{ok, true}
	end.
