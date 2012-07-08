-module(emob_transport).
-export([get_request_token/0]).

get_request_token() ->
	case ibrowse:send_req("http://localhost:8080/get_request_token", [{"Accept", "application/json"}], get, ["callback_url=http://localhost:8080/get_access_token"], [{response_format, binary}]) of
		{ok, Code, Headers, Body} ->
			try
				case Code of
					%% Only 2xx HTTP response codes are considered successful (is this correct?)
					"2" ++ _Tail ->
						{ok, Body};
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
