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
-export([buildReply/2,buildReply/3,createReplica/1,createReplica/2,forwardMsg/2,forwardMsg/3,getAllDCs/0,getDCsReplica/1,getNewID/1,getNewID/0,newReplica/2,setDCsReplica/2,sendReply/4,sendToAllDCs/1]).
-endif.


%% =============================================================================
%% Data Centers support
%% =============================================================================

%% @spec createReplica(Key::atom(), Value) -> Result::typle()
%% 
%% @doc Creates the replica locally, sets the specified value and notifies all existing 
%%		DC with replicas of the new replica. The result may have the values {ok} or 
%%		{error, ErrorCode}
createReplica(Key, Value) ->
    send(new_replica, Key, Value).

%% @spec createReplica(Key::atom()) -> Result::tuple()
%% 
%% @doc Creates a replica locally and notifies all existing DC with replicas of the new 
%%		replica. The result may have the values {ok, Value} or {error, ErrorCode}.
createReplica(Key) ->
	send(new_replica, Key).

%% @spec newReplica(Key::atom(), Dc) -> {ok}
%% 
%% @doc Sends a new replica message to the replication layer.
newReplica(Key, Dc) ->
    % Send to the replication layer
    Pid = getReplicationLayerPid(Key),
    Id = getNewID(Key),
    Pid ! {new_replica, Dc, Id},
	{ok}.

%% @spec sendToAllDCs(Msg) -> {ok}
%% 
%% @doc Sends the specified message to all the DCs.
sendToAllDCs(Msg) ->
	sendToDCs(getAllDCs(), Msg).

%% @spec sendToOneDC(Key::atom(), Msg) -> {ok}
%% 
%% @doc Sends the specified message to one of DCs with replica.
sendToOneDC(Key, Msg) ->
    Pid = lists:nth(1, getDCsReplica(Key)),
    Pid ! Msg,
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
            Id1 = getNewID(Key),
            Pid = sendToOneDC(Key, {forward, {node(), Key}, Id1, Msg}),
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

%% @spec buildReply(Type::atom(), Id::integer(), Result) -> Msg::atom()
%%
buildReply(Type, Id, Results) ->
	{reply, Type, Id, Results}.
%% @spec buildReply(Type::atom(), Id::integer()) -> Msg::atom()
%%
buildReply(Type, Id) ->
	{reply, Type, Id}.

%% @spec sendReply(Destination::pid(), Type::atom(), Id::integer(), Result) -> {ok}
%%
%% @doc Builds a reply message and sends it to the specified destination. Reduces the 
%%		chances of making mistakes when building a reply message, keeping the format 
%%		hidden.
sendReply(Destination, Type, Id, Result) -> 
	Destination ! {reply, Type, self(), Id, Result},
	{ok}.

%% @spec sendToDCs(DCs::list(), Msg) -> {ok}
%% 
%% @doc Sends the specified message to each of the provided list of DCs.
sendToDCs([], _Msg) ->
	{ok};
sendToDCs([Dc|DCs], Msg) ->
	try 
		Dc ! Msg
    after
		sendToDCs(DCs, Msg)
	end.

%% @spec setDCsReplica(Key::atom(), DCs::list()) -> Result::atom()
%%
%% @doc Sets the list of DCs with replica. If the DC has an interal copy of the data then 
%%		it is replicated, otherwise it is not. The result expected may be any of 
%%		{ok, Replicated} or {error, ErrorCode}.
setDCsReplica(Key, DCs) ->
    Pid = getReplicationLayerPid(Key),
	Id = getNewID(Key),
    Pid ! {set_dcs, Key, Id, DCs},
	Result = receive
		{reply, set_dcs, Pid, Id, R} ->
			R

	after
		60000 ->
			{error, timeout}
	end,
	Result.

%% @spec getNewID(Key::atom()) -> Id::integer()
%% 
%% @doc Provides a new ID for the specified key.
getNewID(Key) ->
	Pid = getReplicationLayerPid(Key),
	{reply, new_id, 0, Results} = gen_server:call(Pid, {new_id, 0, Key}, 1000),
	Results.
%% @spec getNewID() -> Id::integer()
%% 
%% @doc Provides a new ID.
getNewID() ->
	Key = process_info(self(), registered_name),
	getNewID(Key).

%% @spec getReplicationLayerPid(Key::atom()) -> Pid::pid()
%% 
%% @doc Provides the local Replication Layer process ID for the specified data.
getReplicationLayerPid(_Key) ->
	getReplicationLayerPid().
getReplicationLayerPid() ->
%	list_to_atom(string:concat(Key, "_rl")).
	'rl'.


%% =============================================================================
% Internal functions
%% =============================================================================
%% @spec getAllDCs() -> DCs::List
%% 
%% @doc The list of all the DCs.
getAllDCs() ->
    nodes().

%% @spec getDCsReplica(Key::atom()) -> DCs::list()
%% 
%% @doc Provides the current list of DCs with replica.
getDCsReplica(Key) ->
    Pid = getReplicationLayerPid(Key),
    Id = getNewID(Key),
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

%% @spec send(Type::atom(), Key::atom()) -> Result::tuple()
%% 
%% @doc Sends message to the replication layer. The results may have one of these values 
%%		{ok} or {error, ErrorCode}.
send(Type, Key) ->
    Pid = getReplicationLayerPid(Key),
    Id = getNewID(Key),
    Pid ! {Type, Key, Id},
    % Wait for the reply
    receive
        {reply, Type, Pid, Id, Reply} ->
            Reply
    after
        60000 ->
            {error, timeout}
    end.

%% @spec send(Type::atom(), Key::atom(), Value) -> Result::tuple()
%% 
%% @doc Sends message to the replication layer.
send(Type, Key, Value) ->
    Pid = getReplicationLayerPid(Key),
    Id = getNewID(),
    Pid ! {Type, Key, Id, Value},
    % Wait for the reply
    receive
        {reply, Type, Pid, Id, Reply} ->
            Reply
    after
        60000 ->
            {error, timeout}
    end.
