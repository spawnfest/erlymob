-module(emob_ui_index_controller, [Req, SessionID]).
-export([before_/1, landing_page/3, home/3]).

before_(_) ->
	emob_ui_user:check_auth(SessionID).

landing_page('GET', [], true) ->
	{redirect, [{action, "home"}]};
landing_page('GET', [], false) ->
	{ok, []}.


home('GET', [], false) ->
	{redirect, [{action, "landing_page"}]};
home('GET', [], true) ->
	{ok, [{session_id, SessionID}]}.
