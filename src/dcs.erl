%% =============================================================================
%% Adapive Replications DC - SyncFree
%%
%% Support function to access the current DC and other DCs
%% 
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

%% 
%% @doc Provides operations required in a database.
-module(dcs).
-author('aas@trifork.co.uk').

-ifdef(EUNIT).
-compile(export_all).
-else.
-compile(report).
-export([forwardMsg/2,forwardMsg/3,getDCsReplica/1,setDCsReplica/2]).
-endif.


%% =============================================================================
%% Data Centers support
%% =============================================================================

%% @spec sendToOneDC(Key::atom(), Msg::tuple()) -> {ok}
%% 
%% @doc Sends the specified message to one of DCs with replica.
sendToOneDC(Key, Msg) ->
    Pid = lists:nth(1, getDCsReplica(Key)),
    gen_server:cast(Pid, Msg),
    {ok}.

%% @spec forwardMsg(Key::atom(), Msg) -> Response::tuple()
%% 
%% @doc Forwards the specified message to one of the DCs with replica and waits for the 
%%		response. This execution is synchronous. The result values are Response, 
%%		{error::atom(), invalid_msg_format} or {error::atom(), timeout::atom()}.
forwardMsg(Key, Msg) ->
	forwardMsg(Key, Msg, false).
%% @spec forwardMsg(Key::atom(), Msg, WaitReply::boolean()) -> Response::tuple()
%% 
%% @doc Forwards the specified message to one of the DCs with replica and waits for the 
%%		response id WaitReply is true. The possible result values are Response, 
%%		{id::atom(), Id::integer()}, {error::atom(), invalid_msg_format} or 
%%		{error::atom(), timeout::atom()}.
forwardMsg(Key, Msg, WaitReply) ->
    % Get the type and ID of the message to forward
    {Type, Id} = case Msg of
        {T, _Pid, _Id, _Value} ->
            {T, _Id};
        {T, _Pid, _Id} ->
            {T, _Id};
        _Other ->
            % No supported message
            {{error, invalid_msg_format}, none}
    end,
    if
        Id == none ->
            Type;
        true ->
            % Send to one of the DCs with replica
            Id1 = adpreps_:getNewID(Key),
            Pid = sendToOneDC(Key, {forward, {Key, node()}, Id1, Msg}),
			if
				WaitReply ->
		            % Wait for the reply
		            Results = receive
		                {reply, Type, Pid, Id1, Value1} -> 
		                    Value1
		            after
		                60000 -> 
		                    % 1 min timeout
		                    {error, timeout}
		            end,
		            {reply, Type, Key, Id, Results};

				true ->
					{id, Id}
			end
    end.

%% @spec setDCsReplica(Key::atom(), DCs::list()) -> Result::atom()
%%
%% @doc Sets the list of DCs with replica. If the DC has an interal copy of the data then 
%%		it is replicated, otherwise it is not. The result expected may be any of 
%%		{ok, Replicated} or {error, ErrorCode}.
setDCsReplica(Key, DCs) ->
    Pid = adpreps_:getReplicationLayerPid(Key),
	Id = adpreps_:getNewID(Key),
    Pid ! {set_dcs, Key, Id, DCs},
	Result = receive
		{reply, set_dcs, Pid, Id, R} ->
			R

	after
		60000 ->
			{error, timeout}
	end,
	Result.


%% =============================================================================
% Internal functions
%% =============================================================================

%% @spec getDCsReplica(Key::atom()) -> DCs::list()
%% 
%% @doc Provides the current list of DCs with replica.
getDCsReplica(Key) ->
    Pid = adpreps_:getReplicationLayerPid(Key),
    Id = adpreps_:getNewID(Key),
    Pid ! {get_dcs, Key, Id},
    receive
        {reply, set_dcs, Pid, Id, {ok, DCs}} ->
            DCs;
        {reply, set_dcs, Pid, Id, {error, error_name}} ->
            []
    after
        600000 ->
            []
    end.
