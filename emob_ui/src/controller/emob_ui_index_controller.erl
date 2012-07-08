-module(emob_ui_index_controller, [Req, SessionID]).
-export([before_/1, landing_page/3, home/3]).

before_(_) ->
	emob_ui_user:check_auth(SessionID).

landing_page('GET', [], true) ->
	{redirect, [{action, "home"}]};
landing_page('GET', [], false) ->
	{ok, []}.


home('GET', [], false) ->
	{redirect, [{controller, "index"}, {action, "landing_page"}]};
home('GET', [], true) ->
	AccessToken = boss_session:get_session_data(SessionID, access_token),
	AccessTokenSecret = boss_session:get_session_data(SessionID, access_token_secret),
	UserId = boss_session:get_session_data(SessionID, user_id),
	ScreenName = boss_session:get_session_data(SessionID, screen_name),
	{ok,[
		{logged_in, true},
		{session_id, SessionID},
		{access_token, AccessToken},
		{access_token_secret, AccessTokenSecret},
		{user_id, UserId},
		{screen_name, ScreenName}]}.
