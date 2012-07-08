-module(emob_transport).
-export([get_request_token/0, get_access_token/2]).

get_request_token() ->
	{ok, ApiUrl} = application:get_env(emob_ui, api_root),
	TargetUrl = lists:flatten(io_lib:format("~s/get_request_token", [ApiUrl])),
	{ok, CBUrl} = application:get_env(emob_ui, local_root),
	GetData = lists:flatten(io_lib:format("callback_url=~s/post_login", [CBUrl])),
	io:format("TargetUrl: ~p; GetData: ~p~n", [TargetUrl, GetData]),
	case ibrowse:send_req(TargetUrl, [{"Accept", "application/json"}], get, [GetData], [{response_format, binary}]) of
		{ok, Code, Headers, Body} ->
			try
				case Code of
					"2" ++ _Tail ->
						ejson:decode(Body);
					_  ->
						{error, Code}
				end
			catch
				throw:Reason ->
					{error, Reason}
			end;

		{error, _Reason} = Error ->
			Error
	end.

get_access_token(OAuthToken, OAuthVerifier) ->
	{ok, ApiUrl} = application:get_env(emob_ui, api_root),
	TargetUrl = io_lib:format("~s/get_access_token?oauth_token=~s&oauth_verifier=~s", [ApiUrl, OAuthToken, OAuthVerifier]),
	case ibrowse:send_req(TargetUrl, [{"Accept", "application/json"}], get, [], [{response_format, binary}]) of
		{ok, Code, Headers, Body} ->
			try
				case Code of
					"2" ++ _Tail ->
						ejson:decode(Body);
					_  ->
						{error, Code}
				end
			catch
				throw:Reason ->
					{error, Reason}
			end;

		{error, _Reason} = Error ->
			Error
	end.
