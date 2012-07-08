-module(emob_ui_authentication_controller, [Req, SessionID]).
-export([login/2, post_login/2, logout/2]).

login('GET', []) ->
	{Json} = emob_transport:get_request_token(),
	RequestToken = kvc:path(result.token, Json),
	Url = "https://api.twitter.com/oauth/authenticate?oauth_token=" ++ binary_to_list(RequestToken),
	{redirect, [Url]}.

post_login('GET', []) ->
	OAuthToken = Req:query_param("oauth_token"),
	OAuthVerifier = Req:query_param("oauth_verifier"),
	io:format("oauth token: ~s~noauth verifier: ~s~n", [OAuthToken, OAuthVerifier]),

	{Json} = emob_transport:get_access_token(OAuthToken, OAuthVerifier),
	AccessToken = kvc:path(result.token, Json),
	AccessTokenSecret = kvc:path(result.secret, Json),
	UserId = kvc:path(result.user_id, Json),
	ScreenName = kvc:path(result.screen_name, Json),
	io:format("access token: ~s~n~nnow logged in as: ~s~n~n", [AccessToken, ScreenName]),

	boss_session:set_session_data(SessionID, access_token, AccessToken),
	boss_session:set_session_data(SessionID, access_token_secret, AccessTokenSecret),
	boss_session:set_session_data(SessionID, user_id, UserId),
	boss_session:set_session_data(SessionID, screen_name, ScreenName),
	{redirect, [{controller, "index"}, {action, "home"}]}.

logout('GET', []) ->
	boss_session:delete_session(SessionID),
	{redirect, [{controller, "index"}, {action, "landing_page"}]}.
