-module(emob_ui_authentication_controller, [Req, SessionID]).
-export([login/2]).

login('GET', []) ->
	{Json} = emob_transport:get_request_token({callback_url, "http://localhost:3000/post_login"}),
	RequestToken = kvc:path(result.token, Json),
	Url = "https://api.twitter.com/oauth/authenticate?oauth_token=" ++ binary_to_list(RequestToken),
	{redirect, [Url]}.

post_login('GET', []) ->
	{output, ["test"]}.
