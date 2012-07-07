-module(emob_ui_index_controller, [Req, SessionID]).
-compile(export_all).

home('GET', []) ->
	{output, "testing, testing, 1 2 3."}.
