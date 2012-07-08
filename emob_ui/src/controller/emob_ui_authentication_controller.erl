-module(emob_ui_authentication_controller, [Req, SessionID]).
-export([login/2]).

login('GET', []) ->
	RequestToken = emob_transport:get_request_token(),
	{ok, []}.
