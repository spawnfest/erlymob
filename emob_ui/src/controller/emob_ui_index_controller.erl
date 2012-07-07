-module(emob_ui_index_controller, [Req, SessionID]).
-export([home/2]).

home('GET', []) ->
	{ok, [{session_id, SessionID}]}.
