-module(emob_ui_authentication_controller, [Req, SessionID]).
-export([login/2]).

login('GET', []) ->
	RequestBody = emob_transport:get_request_token({callback_url, "http://localhost:3000/post_login"}),
	Json = element(1, RequestBody),
	RequestToken = kvc:path(result.token, Json),
	Url = "https://api.twitter.com/oauth/authenticate?oauth_token=" ++ binary_to_list(RequestToken),
	io:format("~p~n", [Url]),
	{redirect, [Url]}.
