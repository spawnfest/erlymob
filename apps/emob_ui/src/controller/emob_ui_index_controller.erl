-module(emob_ui_index_controller, [Req, SessionID]).
-export([home/2]).

home('GET', []) ->
	{output, "testing, testing, 1 2 3."}.
