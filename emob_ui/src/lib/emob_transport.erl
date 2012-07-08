-module(emob_transport).
-export([get_request_token/1, get_access_token/2]).

get_request_token(CallbackUrl) ->
	{_, Url} = CallbackUrl,
	GetData = "callback_url=" ++ Url,
	case ibrowse:send_req("http://localhost:8080/get_request_token", [{"Accept", "application/json"}], get, [GetData], [{response_format, binary}]) of
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
	Url = io_lib:format("http://localhost:8080/get_access_token?oauth_token=~s&oauth_verifier=~s", [OAuthToken, OAuthVerifier]),
	case ibrowse:send_req(Url, [{"Accept", "application/json"}], get, [], [{response_format, binary}]) of
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
