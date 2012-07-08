-module(emob_ui_mob_controller, [Req, SessionID]).
-export([view/2]).

view('GET', []) ->
	{ok, []}.
