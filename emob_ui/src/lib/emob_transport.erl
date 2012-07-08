-module(emob_transport).
-export([get_request_token/1]).

get_request_token(CallbackUrl) ->
	{_, Url} = CallbackUrl,
	Body = "callback_url=" ++ Url,
	case ibrowse:send_req("http://localhost:8080/get_request_token", [{"Accept", "application/json"}], get, [Body], [{response_format, binary}]) of
		{ok, Code, Headers, Body} ->
			try
				case Code of
					%% Only 2xx HTTP response codes are considered successful (is this correct?)
					"2" ++ _Tail ->
						JsonData = ejson:decode(Body),
						JsonData;
					_  ->
						%% In case of errors, return the reason corresponding to the HTTP response code and
						%% the error document returned by MLAPI.
						{error, Code}
				end
			catch
				throw:Reason ->
					{error, Reason}
			end;

		{error, _Reason} = Error ->
			Error
	end.
