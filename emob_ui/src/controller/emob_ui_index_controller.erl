-module(emob_ui_index_controller, [Req, SessionID]).
-export([landing_page/2, home/2]).

landing_page('GET', []) ->
	{ok, []}.

home('GET', []) ->
	{ok, [{session_id, SessionID}]}.
