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
-module(adprep).
-author('aas@trifork.co.uk').

-ifdef(EUNIT).
-compile(export_all).
-else.
-compile(report).
% Interface calls
-export([start/0, stop/0, create/1, create/2, create/4, delete/1, hasReplica/1, read/1, 
		 update/2, remove/1, remove/3, getNumReplicas/1]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, 
		 terminate/2]).
-endif.
-behaviour(gen_server).

-include("adprep.hrl").


%% =============================================================================
%% Server interface
%% =============================================================================
%% @spec start() -> Result
%% 
%% @doc Start the server.
%%
%%  	Result = {ok, Pid::pid()} | ignore | {error, Error} with
%%		Error = {already_started, Pid} | term()
start() -> 
%    lager:info("Starting~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec stop() -> {ok}
%% 
%% @doc Stops the server asynchronously.
stop() ->
%    lager:info("Stopping~n"),
    gen_server:cast(?MODULE, shutdown).

%% @spec create(Key::atom(), Value, NextDCFunc::function(), Args) -> Result::tuple()
%% 
%% @doc Creates the replica locally and creates other replicas if the startegy requires. 
%		The result may have the values {ok} or {error, ErrorCode}.
%%
%%		NextDCFunc is a function which must take three arguments; the current DC, a list 
%%		of DCs and its own Args. Such function must return a tuple composed of the list 
%%		of DCs to replicate and another list of potential DCs to replicate. If any 
%%		replication to a DC from the list of DCs to replicate in failes a DC from the 
%%		potential list will be used instead.
create(Key, Value, NextDCFunc, Args) ->
%    lager:info("Creating entry for ~p",[Key]),
    gen_server:call(?MODULE, {create, Key, {Value, NextDCFunc, Args}}).
%% @spec create(Key::atom(), Value) -> Result::tuple()
%% 
%% @doc Creates the local replica only. The result may have the values {ok} or 
%%		{error, ErrorCode}.
create(Key, Value) ->
%    lager:info("Creating entry for ~p",[Key]),
    gen_server:call(?MODULE, {create, Key, {Value, Key}}).
%% @spec create(Key::atom()) -> Result::tuple()
%% 
%% @doc Creates the local replica only. The value for the specified key must already 
%%		exists in another DC. The result may have the values {ok} or {error, ErrorCode}. 
create(Key) ->
%    lager:info("Creating entry for ~p",[Key]),
    gen_server:call(?MODULE, {create, Key}).

%% @spec delete(Key::atom()) -> Result::tuple()
%% 
%% @doc Deletes an entry from within all the DCs with replica. The result is {ok} on 
%%		success or {error, ErrorCode} otherwise.
delete(Key) ->
%    lager:info("Removing entry for ~p",[Key]),
    gen_server:call(?MODULE, {delete, Key}).

%% @spec getNumReplicas(Key::atom()) -> Result::integer()
%% 
%% @doc Gets the number of replicas.
getNumReplicas(Key) ->
%    lager:info("Getting number of replicas of entry for ~p",[Key]),
    gen_server:call(?MODULE, {num_replicas, Key}).

%% @spec hasReplica(Key::atom()) -> Result::boolean()
%% 
%% @doc Checks if there is a local replica. The result may have the values {yes} or 
%%		{no}.
hasReplica(Key) ->
	gen_server:call(?MODULE, {has_a_replica, Key}).

%% @spec read(Key::atom()) -> Result::tuple()
%% 
%% @doc Reads specified entry. The result may have the values {ok, Value} or 
%%		{error, ErrorCode}.
read(Key) ->
%    lager:info("Reading entry for ~p",[Key]),
    gen_server:call(?MODULE, {read, Key}).

%% @spec remove(Key::atom()) -> Result::tuple()
%% 
%% @doc Removes the local entry. The result may have the values {ok} or 
%%		{error, ErrorCode}.
remove(Key) ->
%    lager:info("Removing entry for ~p",[Key]),
    gen_server:call(?MODULE, {remove, Key}).

%% @spec remove(Key::atom(), VerifyRemove::function(), Args) -> Result::tuple()
%% 
%% @doc Removes the local entry id the conditios are apropiated, which is check by calling 
%%		function VerifyRemove with the record associated to the passed key and the passed 
%%		arguments. The result may have the values {ok} or {error, ErrorCode}.
remove(Key, VerifyRemove, Args) ->
%    lager:info("Removing entry for ~p",[Key]),
    gen_server:call(?MODULE, {remove, Key, VerifyRemove, Args}).

%% @spec ipdate(Key::atom(), Value) -> Result::typle()
%% 
%% @doc Updates the specified value into the local data and forward update messages to the 
%%		other DCs with replicas. The result may have the values {ok} or 
%%		{error, ErrorCode}.
update(Key, Value) ->
%    lager:info("Updating entry for ~p",[Key]),
    gen_server:call(?MODULE, {write, Key, Value}).
	

%% =============================================================================
%% Propossed Adaptive Replication Strategy process
%% =============================================================================
%% @spec init([]) -> {ok, LoopData::tuple()}
%%
%% @doc Initialises the process and start the process.
init([]) ->
	{ok, {0, maps:new()}}.

%% @spec handle_info(Msg, LoopData) -> {noreply, LoopData}
%%
%% @doc Does nothing.
handle_info(_Msg, LoopData) ->
	{noreply, LoopData}.

%% @spec terminate(Reason, LoopData) -> ok
%%
%% @doc Does nothing.
terminate(_Reason, _LoopData) ->
	ok.

%% @spec code_change(PreviousVersion, State, Extra) -> Result::tuple()
%%
%% @doc Does nothing. No change planned yet.
code_change(_PreviousVersion, State, _Extra) ->
	% The function is there for the behaviour, but will not be used. Only a version on the
	% next
	{ok, State}.

%% =============================================================================
%% Messages handlers
%% =============================================================================
handle_call({num_replicas, Key}, _From, {OwnId, Map}) ->
	NumReplicas = getNumReplicas(Key, Map),
	{reply, NumReplicas, {OwnId, Map}};

handle_call({get_dcs, Key}, _From, {OwnId, Map}) ->
	{Response, OwnId1} = case getRecord(Key, Map) of
		none ->
			Response1 = getAllDCsWithReplicas(Key, OwnId),
			Response2 = case Response1 of
				{error, _} ->
					{exists, []};
				R ->
					R
			end,
			{Response2, OwnId+1};
		Record ->
			#replica{list_dcs_with_replicas=List}=Record,
			{{ok, List}, OwnId}
	end,
	{reply, Response, {OwnId1, Map}};

handle_call({create, Key}, _From, {OwnId, Map}) ->
	% The data should not already exist
	case getNumReplicas(Key, Map) of
		0 ->
			% Get current value
			{{ok, Value}, OwnId1} = read(Key, OwnId, Map),
			handle_call({create, Key, {Value, ?MODULE}}, _From, {OwnId1, Map}); % could be made more efficient
		_ ->
			% Ignore as there is a local replica
			{reply, {error, already_exists_replica}, {OwnId, Map}}
	end;
%% Returns {reply,create,{ok}} | {reply,create,{error, already_exists_replica}}
handle_call({create, Key, {Value, RegName}}, _From, {OwnId, Map}) ->
	% The data should not already exist
	case getNumReplicas(Key, Map) of
		0 ->
			% Create the record for the specified key and save it
			{Response, OwnId1, Record, Map1} = create_(Key, Value, Map, OwnId),
			{ok, DCs} = getAllDCsWithReplicas(Key, OwnId1),
			Record1 = Record#replica{num_replicas=sets:size(DCs)+1, 
					 	   			 list_dcs_with_replicas=DCs},
			Map2 = maps:put(Key, Record1, Map1),
			% Notify other DCs with replica
			% TODO: should be asynchronous
			gen_server:multi_call(sets:to_list(DCs), RegName, {new_replica, node(), Key, Value}),
			{reply, Response, {OwnId1+1, Map2}};
		_ ->
			% Ignore as there is a local replica
			{reply, {error, already_exists_replica}, {OwnId, Map}}
	end;
%% Returns {reply,create,{ok}} | {reply,create,{error, already_exists_replica}}
handle_call({create, Key, {Value, NextDCFunc, Args}}, _From, {OwnId, Map}) ->
	% The data should not already exist
	case getNumReplicas(Key, Map) of
		0 ->
			% Create the record for the specified key and save it
			{Response, OwnId1, Record, Map1} = create_(Key, Value, Map, OwnId),
			% Create all necessary replicas
			{Record1, OwnId2} = createOtherReplicas(Record, OwnId1, NextDCFunc, Args),
			Map2 = maps:put(Key, Record1, Map1),
			{reply, Response, {OwnId2, Map2}};
		_ ->
			% Ignore as there is a local replica
			{reply, {error, already_exists_replica}, {OwnId, Map}}
	end;

%% @spec handle_call({rmv_replica, Dc, Key}, From, Args) -> Result::tuple()
%%
%% @doc Removes the specified DC from the list of DCs with replica.
handle_call({rmv_replica, Dc, Key}, _From, {OwnId, Map}) ->
	% The data should already exist
	{Reply, Args} = case getRecord(Key, Map) of
		none ->
			% Ignore as there is no replica
			{{error, no_replica}, {OwnId, Map}};
		Record ->
			% The data exists
			#replica{num_replicas=Num,list_dcs_with_replicas=List}=Record,
			List1 = sets:del_element(Dc, List),
			Record1 = Record#replica{num_replicas=Num-1,list_dcs_with_replicas=List1},
			Map2 = maps:put(Key, Record1, Map),
			{{ok}, {OwnId, Map2}}
	end,
	{reply, Reply, Args};

handle_call({new_replica, Dc, Key, Value}, _From, {OwnId, Map}) ->
	{Reply, Args} = if 
		Dc == self() ->
			% Unable to update record of itself
			{{error, self}, {OwnId, Map}};
		true ->
			% The data should already exist as it comes from another DC
			case getRecord(Key, Map) of
				none ->
					% It does not alreday exist
					{{error, does_not_exist}, {OwnId, Map}};
				Record ->
					% Update it
					#replica{num_replicas = NumReplicas, 
							 list_dcs_with_replicas=List} = Record,
					List1 = sets:add_element(Dc, List),
					Record1 = Record#replica{value=Value,
											 num_replicas = NumReplicas+1, 
							 	   			 list_dcs_with_replicas=List1},
					Map1 = maps:put(Key, Record1, Map),
					{{ok}, {OwnId, Map1}}
			end
	end,
	{reply, Reply, Args};

handle_call({new_replica, Dc, Key}, _From, {OwnId, Map}) ->
	{Reply, Args} = if 
		Dc == self() ->
			% Unable to update record of itself
			{{error, self}, {OwnId, Map}};
		true ->
			% The data should already exist as it comes from another DC
			case getRecord(Key, Map) of
				none ->
					% It does not alreday exist
					{{error, does_not_exist}, {OwnId, Map}};
				Record ->
					% Update it
					#replica{num_replicas = NumReplicas, 
							 list_dcs_with_replicas=List} = Record,
					List1 = sets:add_element(Dc, List),
					Record1 = Record#replica{num_replicas = NumReplicas+1, 
							 	   			 list_dcs_with_replicas=List1},
					Map1 = maps:put(Key, Record1, Map),
					{{ok}, {OwnId, Map1}}
			end
	end,
	{reply, Reply, Args};

handle_call({read, Key}, _From, {OwnId, Map}) ->
	{Response, OwnId1} = read(Key, OwnId, Map),
	{reply, Response, {OwnId1, Map}};

handle_call({write, Key, Value}, _From, {OwnId, Map}) ->
	{Response, OwnId1, Map1} = write(Key, OwnId, Value, Map),
	{reply, Response, {OwnId1, Map1}};

handle_call({delete, Key}, _From, {OwnId, Map}) ->
	{Response, OwnId1, Map1} = rmvDel(Key, OwnId, Map, forward_delete),
	{reply, Response, {OwnId1, Map1}};

handle_call({forward_delete, Key}, _From, {OwnId, Map}) ->
	% Remove Strategy Layer for the key
	gen_server:cast({node(), Key}, shutdown),
	% Remove replica
	Map1 = maps:remove(Key, Map),
	{reply, {ok}, {OwnId, Map1}};

handle_call({remove, Key}, _From, {OwnId, Map}) ->
	{Response, OwnId1, Map1} = rmvDel(Key, OwnId, Map, rmv_replica),
	{reply, Response, {OwnId1, Map1}};
handle_call({remove, Key, VerifyRemove, Args}, _From, {OwnId, Map}) ->
	try maps:get(Key, Map) of
		Record ->
			% DCs with replica
			case VerifyRemove(Record, Args) of
				true ->
					% Proceed
					#replica{list_dcs_with_replicas=DCs}=Record,
					case forward({rmv_replica, node(), Key}, DCs) of
						{ok} ->
							% Success - Remove local replica
							Map1 = maps:remove(Key, Map),
							{reply, {ok}, {reply, Map1}};
						{error, no_replica} ->
							% Success- Remove local replica
							Map1 = maps:remove(Key, Map),
							{reply, {ok}, {OwnId, Map1}};
						R ->
							% Failure - should alreday have rolled back
							{reply, R, {OwnId, Map}}
					end;
				false ->
					{reply, {error, failed_verification}, {OwnId, Map}}
			end
	catch
		_:_ ->
			{reply, {ok}, {OwnId, Map}}
	end;

handle_call({has_a_replica, Key}, _From, {OwnId, Map}) ->
	case getRecord(Key, Map) of
		none ->
			{reply, {no},  {OwnId, Map}};
		_Record ->
			{reply, {yes}, {OwnId, Map}}
	end.

handle_cast({update, Id, Key, Value}, {OwnId, Map}) ->
	case getRecord(Key, Map) of
		none ->
			% Ignore as there is no replica
			{reply, adpreps_:buildReply(update, Id, {error, no_replica}), {OwnId, Map}};
		Record ->
			Record1 = Record#replica{value=Value},
			Map1 = maps:put(Key, Record1, Map),
			{reply, adpreps_:buildReply(update, Id, {ok, updated}), {OwnId, Map1}}
	end;

handle_cast(shutdown, {OwnId, Map}) ->
    {stop, normal, {OwnId, Map}};

handle_cast({has_replica, Origin, Id, Key}, {OwnId, Map}) ->
	case getRecord(Key, Map) of
		none ->
			{noreply, {OwnId, Map}};
		Record ->
			#replica{list_dcs_with_replicas=DCs}=Record,
			Origin ! adpreps_:buildReply(has_replica, Id, {exists, [node() | DCs]}),
			{noreply, {OwnId, Map}}
	end;

handle_cast({create_new, Id, Key, Value, DCs}, {OwnId, Map}) ->
	% Create replica but do not notify anyone
	% The data should not already exist
	{Reply, Args} = case getRecord(Key, Map) of
		0 ->
			% Create the record for the specified key and save it
			List = sets:del_element(self(), DCs),
			Record=#replica{key=Key,value=Value,num_replicas=sets:size(DCs),list_dcs_with_replicas=List},
			Map1 = maps:put(Key, Record, Map),
			{adpreps_:buildReply(create_new, Id, {ok}), {OwnId, Map1}};
		_ ->
			% Ignore as there is a replica
			{adpreps_:buildReply(create_new, Id, {error, no_replica}), {OwnId, Map}}
	end,
	{reply, Reply, Args};

% 
handle_cast({new_replica, Id, Key}, {OwnId, Map}) ->
	{Response, OwnId1} = read(Key, OwnId, Map),
	case Response of
		{ok, Value} ->
			Result = handle_call({new_replica, ?MODULE, Key, Value}, ?MODULE, {OwnId1, Map}),
			{reply, adpreps_:buildReply(new_replica, Id, Result), {OwnId1, Map}};
		_ ->
			{reply, adpreps_:buildReply(new_replica, Id, Response), {OwnId1, Map}}
	end;

handle_cast({reply, has_replica, _Id, _Result}, {OwnId, Map}) ->
	% Ignore
	{noreply, {OwnId, Map}};
handle_cast({reply, update, _Id, Key, Result}, {OwnId, Map}) ->
	Map1 = case Result of
		{error, no_replica, Dc} ->
			% The DC does not have a replica
			try maps:get(Key, Map) of
				Record ->
					% Remove it
					#replica{list_dcs_with_replicas=List}=Record,
					List1 = sets:del_element(Dc, List),
					Record1 = Record#replica{list_dcs_with_replicas=List1},
					maps:put(Key, Record1, Map)
			catch
				_ ->
					% Ignore as there is no replica anymore
					Map
			end;

		_ ->
			% Ignore as previous responses has been already processed getting oll needed 
			% data
			Map
	end,
	{noreply, {OwnId, Map1}}.


%% =============================================================================
%% Support functions
%% =============================================================================

%% @spec getAllDCs() -> DCs::List
%% 
%% @doc The list of all the DCs.
getAllDCs() ->
	% TODO: complete it
    [node() | []].

%% @spec getNumReplicas(Key::atom(), Map::map()) -> Result::integer()
%%
%% @doc Get the number of replicas, if any, or zero otherwise.
getNumReplicas(Key, Map) ->
	case maps:get(Key, Map, 0) of
		0 ->
			0;
		Value ->
			#replica{num_replicas=Num}=Value,
			Num
	end.

getRecord(Key, Map) ->
	case maps:get(Key, Map, 0) of
		0 ->
			none;
		Record ->
			Record
	end.

%% @spec create_(Key::atom(), Value, Map::map(), OwnId::integer()) -> Result::tuple()
%%
%% @doc Ceates a record for the specified data, adds it to the passed map and return all 
%%		the new information.
%%
%%		Returns {{ok}, Id::integer(), Record, NewMap}.
create_(Key, Value, Map, OwnId) ->
	% Create the record for the specified key and save it
	List = sets:new(),
	Record=#replica{key=Key,value=Value,num_replicas=1,list_dcs_with_replicas=List},
	Map1 = maps:put(Key, Record, Map),
	{{ok}, OwnId, Record, Map1}.

%% @spec createOtherReplicas(Record, OwnId::integer(), NextDCsFunc::function(), Args) -> Result::tuple()
%%
%% @doc Gets a list of DC where replicas should be created, updates the record with the 
%% 		new list of DCs with replicas and request the creation of the new replicas in each 
%%		of those DCs.
%%
%%		NextDCsFunc is a function that takes the current DC, the list of all DCs and the 
%%		provided arguments, Args and return a tuple with a list of DC to replicat in and a 
%%		list of potential DCs to replicate in if any of others fail.
createOtherReplicas(Record, OwnId, NextDCsFunc, Args) ->
	AllDCs = getAllDCs(), % get the list of all DCs with or without replica
	{DCs, PotentialDCs} = NextDCsFunc(node(), AllDCs, Args),
	Ds = sets:del_element(self(), sets:from_list(DCs)),
	Size = sets:size(Ds),
	DCs1 = sets:to_list(Ds),
	DCs2 = [self() | DCs1],
	Record1 = Record#replica{num_replicas=Size+1,list_dcs_with_replicas=DCs1},
	{registered_name, RegName} = process_info(self(), registered_name),
	createOtherReplicas_(RegName, Record1, OwnId, DCs2, DCs1, PotentialDCs).

createOtherReplicas_(RegName, Record, OwnId, AllReplicatedDCs, [Dc | DCs], PotentialDCs) ->
	#replica{key=Key,value=Value}=Record,
	{reply, create_new, OwnId, Result} = gen_server:call({RegName, Dc}, {create_new, OwnId, Key, Value, AllReplicatedDCs}),
	PotentialDCs1 = case Result of
		{error, _ErrorCode} ->
			% Failed, try with other potential DCs
			PotentialDCs2 = createReplicasPotentialDcs(RegName, Record, OwnId, AllReplicatedDCs, PotentialDCs, PotentialDCs),
			% Allow it to be later re-use the DC if needed
			[Dc | PotentialDCs2];
		_ ->
			PotentialDCs
	end,
	createOtherReplicas_(RegName, Record, OwnId, AllReplicatedDCs, DCs, PotentialDCs1);
createOtherReplicas_(_RegName, Record, OwnId, _AllReplicatedDCs, [], _PotentialDCs) ->
	{Record, OwnId+1}.

%% @spec createReplicasPotentialDcs(RegName::atom(), Record, OwnId::integer(), AllReplicatedDCs::List, PotentialDCs::List, NextPotentialDCs::List) -> NewPotentialDCs::List
%%
%% @doc Tries to create the replica to a DC from within the list of other potential DCs.
createReplicasPotentialDcs(RegName, Record, OwnId, AllReplicatedDCs, PotentialDCs, [Dc | NextPotentialDCs]) ->
	#replica{key=Key,value=Value}=Record,
	{reply, create_new, OwnId, Result} = gen_server:call({RegName, Dc}, {create_new, OwnId, Key, Value, AllReplicatedDCs}),
	case Result of
		{error, _ErrorCode} ->
			% Failed
			createReplicasPotentialDcs(RegName, Record, OwnId, AllReplicatedDCs, PotentialDCs, NextPotentialDCs);
		_ ->
			sets:del_element(Dc, PotentialDCs)
	end;
createReplicasPotentialDcs(_RegName, _Record, _OwnId, _AllReplicatedDCs, PotentialDCs, []) ->
	PotentialDCs.

%% @spec read(Key::atom(), OwnId::integer(), Map::map()) -> Result::tuple()
%%
%% @doc Reads the data locally if exist, i.e. replicated, or alternativelly get the data 
%%		from any of the other DCs with replicas.
%%
%%		The returned value is a tuple with the response of the form 
%%		{{ok, Value}, NewOwnId} or {{error, ErrorCode}, NewOwnId}.
read(Key, OwnId, Map) ->
	try maps:get(Key, Map) of
		Record ->
			#replica{value=Value}=Record,
			{{ok, Value}, OwnId}
	catch
		_:_ ->
			% Find DCs with replica
			case getAllDCsWithReplicas(Key, OwnId) of
				{ok, DCs} ->
					%% Get the data from one of the DCs with replica
					{registered_name, RegName} = process_info(self(), registered_name),
					{Response, OwnId1} = sendOne(read, OwnId+1, Key, {read, OwnId, Key}, RegName, DCs),
					{Response, OwnId1};
				{error, ErrorCode} ->
					% An error
					{{error, ErrorCode}, OwnId+1}
			end
	end.

%% @spec write(Key::atom(), OwnId::integer(), Value, Map::map()) -> Result::tuple()
%%
%% @doc Saves locally the new value and sets to send updates to all DCs with replicas if 
%%		the data exists locally, otherwise requested from DCs with replicas and if the 
%%		data does not esists an error is returned.
write(Key, OwnId, Value, Map) ->
	try maps:get(Key, Map) of
		Record ->
			% Update local data
			Record1 = Record#replica{value=Value},
			Map1 = maps:put(Key, Record1, Map),
			% Send updates to other DCs
			#replica{list_dcs_with_replicas=DCs}=Record1,
			gen_server:abcast(DCs, Key, {update, OwnId, Key, Value}),
			{{ok}, OwnId+1, Map1}
	catch
		_:_ ->
			% Find DCs with replica
			case getAllDCsWithReplicas(Key, OwnId) of
				{ok, DCs} ->
					% Send updates to each of the replicated sites
					gen_server:abcast(DCs, Key, {update, OwnId, Key, Value}),
					{{ok}, OwnId+1, Map};
				{error, ErrorCode} ->
					% An error
					{{error, ErrorCode}, OwnId+1, Map}
			end
	end.

%% @spec getAllDCsWithReplicas(Key::atom(), OwnId::integer()) -> Result::tuple()
%%
%% @doc Gets all the DCs with a replica.
%%
%%		Returs a tuple that can be {ok, DCS} on success or {error, timeout} otherwise.
getAllDCsWithReplicas(Key, OwnId) ->
	% Discover the DCs with replicas
	AllDCs = getAllDCs(),
	flush(OwnId),
	gen_server:abcast(AllDCs, Key, {has_replica, OwnId, Key}),
	% Only take the first one
	receive
		{reply, has_replica, OwnId, {exists, DCs}} ->
			{ok, DCs}
	after
		1000 ->
			{error, timeout}
	end.

%% @spec sendOne(Type::atom(), OwnId::integer(), Key::atom(), Msg, RegName::atom(), DCs::List) -> Result::tuple()
%%
%% @doc Sends synchronously the specified message to the first of the specified DC for its 
%%		process registered with the key and on failure will try with the other DCs.
%%
%%		Returned result is a tuple with the result and the new own internal ID.
sendOne(Type, OwnId, Key, Msg, RegName, [Dc | DCs]) ->
	{reply, Type, OwnId, {ResultType, Result}} = gen_server:call({RegName, Dc}, Msg, 1000),
	case ResultType of
		error ->
			% Should be a timeout, so try with the next DC
			sendOne(Type, OwnId, Key, Msg, RegName, DCs);
		_ ->
			{{ResultType, Result}, OwnId+1}
	end;
sendOne(_Type, OwnId, _Key, _Msg, _RegName, []) ->
	{{error, no_dcs}, OwnId+1}.

%% @spec flush(Id::integer()) -> {ok}
%%
%% @doc Removes all the messages that match the specified one from the mailbox.
flush(Id) ->
	receive
		{reply, has_replica, _Id, {exists, _DCs}} ->
			flush(Id)
	after
		0 ->
			{ok}
	end.

rmvDel(Key, OwnId, Map, Type) ->
	try maps:get(Key, Map) of
		Record ->
			% DCs with replica
			#replica{list_dcs_with_replicas=DCs}=Record,
			{Response, Map1} = case forward({Type, node(), Key}, DCs) of
				{ok} ->
					% Success - Remove local replica
					Map2 = maps:remove(Key, Map),
					{{ok}, Map2};
				{error, no_replica} ->
					% Success- Remove local replica
					Map2 = maps:remove(Key, Map),
					{{ok}, Map2};
				R ->
					% Failure - should alreday have rolled back
					{R, Map}
			end,
			{Response, OwnId + 1, Map1}
	catch
		_:_ ->
			{{ok}, OwnId, Map}
	end.

forward(_Msg, []) ->
	{ok};
forward(Msg, [Dc | DCs]) ->
	Result = gen_server:call({adpref, Dc}, Msg),
	case Result of
		{error, no_replica} ->
			forward(Msg, DCs);
		{error, _} ->
			% TODO: roll back
			Result;
		_ ->
			forward(Msg, DCs)
	end.
