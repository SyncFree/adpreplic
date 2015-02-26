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

-ifdef(TEST).
-compile(export_all).
-else.
-compile(report).
-export([buildReply/2,buildReply/3,create/2,createReplica/1,createReplica/2,forwardMsg/2,forwardMsg/3,getAllDCs/0,getDCsReplica/1,getNewID/1,getNewID/0,newReplica/2,read/1,replicated/1,rmvFromReplica/2,rmvReplica/2,setDCsReplica/2,sendReply/4,sendToAllDCs/1,write/2,updates/2,startReplicationLayer/0]).
-endif.


startReplicationLayer() ->
	gen_server:start({global, getReplicationLayerPid()}, adprep, [], []).

%% =============================================================================
%% Data Centers support
%% =============================================================================
%% @spec create(Key::atom(), Value::tuple()) -> Result::typle()
%% 
%% @doc Creates the replica locally. The result may have the values {ok} or 
%%		{error, ErrorCode}.
%%
%%		The passed Value is a tuple that contains {Value, NextDCFunc::function(), Args}.
create(Key, {Value, NextDCFunc, Args}) ->
    send(create, Key, {Value, NextDCFunc, Args}).

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

%% @spec read(Key::atom()) -> Result::typle()
%% 
%% @doc Reads the data locally. The result may have the values {ok, Value} or 
%%		{error, ErrorCode}.
read(Key) ->
    send(read, Key).

%% @spec write(Key::atom(), Value) -> Result::typle()
%% 
%% @doc Writes the specified value into the local data and forward update messages to the 
%%		other DCs with replicas. The result may have the values {ok} or 
%%		{error, ErrorCode}.
write(Key, Value) ->
	% Write new value
    send(write, Key, Value).

%% @spec newReplica(Key::atom(), Dc) -> {ok}
%% 
%% @doc Sends a new replica message to the replication layer.
newReplica(Key, Dc) ->
    % Send to the replication layer
    Pid = getReplicationLayerPid(Key),
    Id = getNewID(Key),
    Pid ! {new_replica, Dc, Id},
	{ok}.

%% @spec rmvFromReplica(Key::atom(), MinNumReplicas::integer()) -> Result::typle()
%% 
%% @doc Removes this replica if the data is sufficiently replicated and notify all other 
%%		replicas. The result may have the values {ok} or {error, insuficient_replicas}.
rmvFromReplica(Key, MinNumReplicas) ->
	% Only process it if there are not sufficient no. of replicas when this 
	% replica is removed
	NumReplicas = getNumReplicas(),
	if 
		NumReplicas > MinNumReplicas ->
			% There may be sufficient then try to remove
	    	dcs:sendToAllDCs({rmv_replica, {node(), Key}, 0}),
			NumReplicas1 = getNumRemovedFromReplicas({node(), Key}),
			if
				NumReplicas1 >= MinNumReplicas ->
					% There are sufficient then try to remove locally
					rmvReplica(Key, Key);

				NumReplicas1 > 0 ->
					% There are insufficient
					Result = dcs:read(Key),
					case Result of
						{ok, Value} ->
							createReplica(Key, Value)
					end,
					{error, insuficient_replicas}
			end;

		true ->
			{error, insuficient_replicas}
	end.

%% @spec rmvReplica(Key::atom(), Dc::pid()) -> {ok}
%% 
%% @doc Sends a removed replica message to the replication layer.
rmvReplica(Key, Dc) ->
    % Send to the replication layer
    Pid = getReplicationLayerPid(Key),
    Id = getNewID(Key),
    Pid ! {rmv_replica, Dc, Id},
    {ok}.

%% @spec update(Key::atom(), Value) -> {ok}
%% 
%% @doc Updates the local replica.
update(Key, Value) ->
    send(update, Key, Value).

%% @spec updates(Key::atom(), Value) -> Result
%% 
%% @doc Updates the local replica and sends appropiate messages to the other DCs with 
%%		replicas to update their replica too. Returns the message ID used to send update 
%%		message to other DCs.
updates(Key, Value) ->
    % Update new value locally
    Result = update(Key, Value),
    % Notify all other DCs with replica irrespective of result from previous
    Id = getNewID(Key),
    sendToDCsReplica(Key, {update, Key, Id, Value}),
    Result.

%% @spec replicated(Key::atom()) -> HasReply::boolean()
%% 
%% @doc Checks if the data is locally replicated.
replicated(Key) ->
	{reply, has_a_replica, 0, {ok, HasReply}} = gen_server:call(getReplicationLayerPid(key), {has_a_replica, 0, Key}, 1000),
	HasReply.

%% @spec sendToAllDCs(Msg) -> {ok}
%% 
%% @doc Sends the specified message to all the DCs.
sendToAllDCs(Msg) ->
	sendToDCs(getAllDCs(), Msg).

%% @spec sendToDCsReplica(Key::atom(), Msg) -> {ok}
%% 
%% @doc Sends the specified message to all the DCs with replica.
sendToDCsReplica(Key, Msg) ->
	sendToDCs(getDCsReplica(Key), Msg).

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

%% @spec getNumReplicas() -> NumReplicas::integer()
%% 
%% @doc Provides the current number of replicas.
getNumReplicas() ->
	%% TODO: implement it
	0.

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
	rl.


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

%% @spec getNumRemovedFromReplicas(Sender::pid()) -> NumRemoved::integer()
%% 
%% @doc Sends message to the replication layer.
getNumRemovedFromReplicas(Sender) ->
	getNumRemovedFromReplicas(Sender, geNumReplicas, 0).
getNumRemovedFromReplicas(Sender, NumReplicas, NumResponses) ->
	receive
		{reply, rmv_replica, Sender, _Id, _Result} ->
			NumResponses1 = NumResponses + 1,
			if
				NumResponses < NumReplicas ->
					getNumRemovedFromReplicas(Sender, NumReplicas, NumResponses1)
			end
	after
		100 ->
			NumResponses
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