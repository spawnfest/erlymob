-module(emob_ui_authentication_controller, [Req, SessionID]).
-export([login/2]).

login('GET', []) ->
	{ok, []}.
