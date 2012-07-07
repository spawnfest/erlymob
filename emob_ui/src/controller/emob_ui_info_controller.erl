-module(emob_ui_info_controller, [Req]).
-export([about/2, team/2]).

about('GET', []) ->
	{ok, []}.

team('GET', []) ->
	{ok, []}.
