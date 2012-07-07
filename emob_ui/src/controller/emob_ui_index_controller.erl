-module(emob_ui_index_controller, [Req, SessionID]).
-export([before_/1, landing_page/3, home/3]).

before_(_) ->
    case boss_session:get_session_data(SessionID, access_token) of
        undefined ->
            {ok, false};
        _ ->
            {ok, true}
    end.


landing_page('GET', [], true) ->
    {ok, []};
landing_page('GET', [], false) ->
    {redirect, [{action, "home"}]}.


home('GET', [], true) ->
    {ok, [{session_id, SessionID}]};
home('GET', [], false) ->
    {redirect, [{action, "landing_page"}]}.
