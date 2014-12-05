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
%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-endif.
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").
-include("adprep.hrl").


%% =============================================================================
%% Propossed Adaptive Replication Strategy process
%% =============================================================================
%% @spec init(Args) -> {ok, LoopData::tuple()}
%%
%% @doc Initialises the process and start the process with the specified arguments.
init(_Args) ->
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
handle_call({num_replicas, Key, Id}, _From, {OwnId, Map}) ->
	NumReplicas = try maps:get(Key, Map) of
		Value ->
			#replica{num_replicas=Num}=Value,
			Num
	catch
		_ ->
			-1
	end,
	{reply, dcs:buildReply(num_replicas, Id, NumReplicas), {OwnId, Map}};

handle_call({get_dcs, Key, Id}, _From, {OwnId, Map}) ->
	{Response, OwnId1} = try maps:get(Key, Map) of
		Record ->
			#replica{list_dcs_with_replicas=List}=Record,
			{{ok, List}, OwnId}
	catch
		_ ->
			Response1 = getAllDCsWithReplicas(Key, OwnId),
			{Response1, OwnId+1}
	end,
	{reply, dcs:buildReply(get_dcs, Id, Response), {OwnId1, Map}};

handle_call({new_id, Id, _Key}, _From, {OwnId, Map}) ->
	{reply, dcs:buildReply(new_id, Id, OwnId), {OwnId+1, Map}};

handle_call({create, Key, Id, {Value, NextDCFunc, Args}}, _From, {OwnId, Map}) ->
	% The data should not already exist
	{Reply, Args} = try maps:get(Key, Map) of
		_Record ->
			% Ignore as there is a replica
			{dcs:buildReply(rmv_replica, Id, {error, no_replica}), {OwnId, Map}}
	catch
		_ ->
			% Create the record for the specified key and save it
			{Response, OwnId1, Record, Map1} = create(Key, Value, Map, OwnId),
			% Create all necessary replicas
			{Record1, OwnId2} = createOtherReplicas(Record, OwnId1, NextDCFunc, Args),
			Map2 = maps:put(Key, Record1, Map1),
			{dcs:buildReply(create, Id, Response), {OwnId2, Map2}}
	end,
	{reply, Reply, Args};

handle_call({rmv_replica, Dc, Id, Key}, _From, {OwnId, Map}) ->
	% The data should already exist
	{Reply, Args} = try maps:get(Key, Map) of
		Record ->
			% The data exists
			#replica{num_replicas=Num,list_dcs_with_replicas=List}=Record,
			List1 = sets:del_element(Dc, List),
			Record1 = Record#replica{num_replicas=Num-1,list_dcs_with_replicas=List1},
			Map2 = maps:put(Key, Record1, Map),
			{dcs:buildReply(rmv_replica, Id, {ok}), {OwnId, Map2}}
	catch
		_ ->
			% Ignore as there is no replica
			{dcs:buildReply(rmv_replica, Id, {error, no_replica}), {OwnId, Map}}
	end,
	{reply, Reply, Args};

handle_call({new_replica, Dc, Id, Key, Value}, _From, {OwnId, Map}) ->
	{Reply, Args} = if 
		Dc == self() ->
			% Unable to update record of itself
			{dcs:buildReply(new_replica, Id, {error, self}), {OwnId, Map}};
		true ->
			% The data should already exist as it comes from another DC
			try maps:get(Key, Map) of
				Record ->
					% Create it
					#replica{value=Value,
							 num_replicas = NumReplicas, 
							 list_dcs_with_replicas=List} = Record,
					List1 = sets:add_element(Dc, List),
					Record1 = Record#replica{num_replicas = NumReplicas+1, 
							 	   			 list_dcs_with_replicas=List1},
					Map1 = maps:put(Key, Record1, Map),
					{dcs:buildReply(new_replica, Id, {ok}), {OwnId, Map1}}
			catch
				_ ->
					% It does not alreday exist
					{dcs:buildReply(new_replica, Id, {error, does_not_exist}), {OwnId, Map}}
			end
	end,
	{reply, Reply, Args}.

handle_cast({has_replica, Id, Key}, {OwnId, Map}) ->
	try maps:get(Key, Map) of
		_Record ->
			{reply, dcs:buildReply(has_replica, Id, {exits, self()}), {OwnId, Map}}
	catch
		_ ->
		{noreply, {OwnId, Map}}
	end;

handle_cast({has_a_replica, Id, Key}, {OwnId, Map}) ->
	try maps:get(Key, Map) of
		_Record ->
			{reply, dcs:buildReply(has_replica, Id, {ok, true}), {OwnId, Map}}
	catch
		_ ->
			{reply, dcs:buildReply(has_replica, Id, {ok, false}), {OwnId, Map}}
	end;

handle_cast({create_new, Id, Key, Value, DCs}, {OwnId, Map}) ->
	% Create replica but do not notify anyone
	% The data should not already exist
	{Reply, Args} = try maps:get(Key, Map) of
		_Record ->
			% Ignore as there is a replica
			{dcs:buildReply(create_new, Id, {error, no_replica}), {OwnId, Map}}
	catch
		_ ->
			% Create the record for the specified key and save it
			List = sets:del_element(self(), DCs),
			Record=#replica{key=Key,value=Value,num_replicas=sets:size(DCs),list_dcs_with_replicas=List},
			Map1 = maps:put(Key, Record, Map),
			{dcs:buildReply(create_new, Id, {ok}), {OwnId, Map1}}
	end,
	{reply, Reply, Args};

% 
handle_cast({new_replica, Id, Key}, {OwnId, Map}) ->
	{Response, OwnId1} = read(Key, OwnId, Map),
	case Response of
		{ok, Value} ->
			handle_call({new_replica, self(), Id, Key, Value}, self(), {OwnId1, Map});
		_ ->
			{reply, dcs:buildReply(new_replica, Id, Response), {OwnId1, Map}}
	end;

handle_cast({read, Key, Id}, {OwnId, Map}) ->
	{Response, OwnId1} = read(Key, OwnId, Map),
	{reply, dcs:buildReply(read, Id, Response), {OwnId1, Map}};

handle_cast({write, Key, Id, Value}, {OwnId, Map}) ->
	{Response, OwnId1, Map1} = write(Key, OwnId, Value, Map),
	{reply, dcs:buildReply(write, Id, Response), {OwnId1, Map1}};

handle_cast({update, Key, _Id, Value}, {OwnId, Map}) ->
	try maps:get(Key, Map) of
		Record ->
			Record1 = Record#replica{value=Value},
			Map1 = maps:put(Key, Record1, Map),
%			{reply, {ok, updated}, {OwnId, Map1}} % maybe it should not be sent back
			{noreply, {OwnId, Map1}}
	catch
		_ ->
			% Ignore as there is no replica
			{{error, no_replica, node()}, Map}
		%	{reply, dcs:buildReply(update, Id, Response), {OwnId, Map1}};
	end;

handle_cast({reply, has_replica, _Id, _Key, _DCs}, {OwnId, Map}) ->
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

%% @spec create(Key::atom(), Value, Map::map(), OwnId::integer()) -> Result::tuple()
%%
%% @doc Ceates a record for the specified data, adds it to the passed map and return all 
%%		new information.
%%
%%		Returns {{ok}, Id::integer(), Record, NewMap}.
create(Key, Value, Map, OwnId) ->
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
	AllDCs = dcs:getAllDCs(), % get the list of all DCs with or without replica
	{DCs, PotentialDCs} = NextDCsFunc(self(), AllDCs, Args),
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
			PotentialDCs2 = createReplicasPotentiaDcs(RegName, Record, OwnId, AllReplicatedDCs, PotentialDCs, PotentialDCs),
			% Allow it to be later re-use the DC if needed
			[Dc | PotentialDCs2];
		_ ->
			PotentialDCs
	end,
	createOtherReplicas_(RegName, Record, OwnId, AllReplicatedDCs, DCs, PotentialDCs1);
createOtherReplicas_(_RegName, Record, OwnId, _AllReplicatedDCs, [], _PotentialDCs) ->
	{Record, OwnId+1}.

%% @spec createReplicasPotentiaDcs(RegName::atom(), Record, OwnId::integer(), AllReplicatedDCs::List, PotentialDCs::List, NextPotentialDCs::List) -> NewPotentialDCs::List
%%
%% @doc Tries to create the replica to a DC from within the list of other potential DCs.
createReplicasPotentiaDcs(RegName, Record, OwnId, AllReplicatedDCs, PotentialDCs, [Dc | NextPotentialDCs]) ->
	#replica{key=Key,value=Value}=Record,
	{reply, create_new, OwnId, Result} = gen_server:call({RegName, Dc}, {create_new, OwnId, Key, Value, AllReplicatedDCs}),
	case Result of
		{error, _ErrorCode} ->
			% Failed
			createReplicasPotentiaDcs(RegName, Record, OwnId, AllReplicatedDCs, PotentialDCs, NextPotentialDCs);
		_ ->
			sets:del_element(Dc, PotentialDCs)
	end;
createReplicasPotentiaDcs(_RegName, _Record, _OwnId, _AllReplicatedDCs, PotentialDCs, []) ->
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
		_ ->
			% Find DCs with replica
			case getAllDCsWithReplicas(Key, OwnId) of
				{ok, Ds} ->
					%% Get the data from one of the DCs with replica
					{registered_name, RegName} = process_info(self(), registered_name),
					{Response, OwnId1} = sendOne(read, OwnId+1, Key, {read, OwnId, Key}, RegName, Ds),
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
			{{ok, Value}, OwnId+1, Map1}
	catch
		_ ->
			% Find DCs with replica
			case getAllDCsWithReplicas(Key, OwnId) of
				{ok, DCs} ->
					% Send updates to each of the replicated sites
					gen_server:abcast(DCs, Key, {update, OwnId, Key, Value}),
					{{ok}, OwnId+1, Map};
				{error, ErrorCode} ->
					% An error
					{{ereor, ErrorCode}, OwnId+1, Map}
			end
	end.

%% @spec getAllDCsWithReplicas(Key::atom(), OwnId::integer()) -> Result::tuple()
%%
%% @doc Gets all the DCs with a replica.
%%
%%		Returs a tuple that can be {ok, DCS} on success or {error, timeout} otherwise.
getAllDCsWithReplicas(Key, OwnId) ->
	% Discover the DCs with replicas
	AllDCs = dcs:getAllDCs(),
	gen_server:abcast(AllDCs, Key, {has_replica, OwnId, Key}),
	flush({has_replica, OwnId, Key}),
	% Only take the first one
	receive
		{reply, has_replica, OwnId, Key, DCs} ->
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

%% @spec flush(Msg) -> {ok}
%%
%% @doc Removes all the messages that match the specified one from the mailbox.
flush(Msg) ->
	receive
		Msg ->
			flush(Msg)
	after
		0 ->
			{ok}
	end.
