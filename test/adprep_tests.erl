%% =============================================================================
%% EUnits for Adapive Replications DC - SyncFree
%% 
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

-module(adprep_tests).
-author('aas@trifork.co.uk').

%% =============================================================================
%% API functions
%% =============================================================================
-compile(export_all).
-export([]).


-include_lib("eunit/include/eunit.hrl").


%% =============================================================================
%% Internal functions
%% =============================================================================
getNumReplicas_test() ->
	% Initialise
	adprep:start(),
	Key = 'test',
	% Test
	NumReplicas = adprep:getNumReplicas(Key),
	?assertEqual(0, NumReplicas),
	% Clean-up
	adprep:stop().

create_test() ->
	% Initialise
	adprep:start(),
	Key = first,
	Value = "value",
	% Test - does not exist
	Response = create(Key, Value),
	?assertEqual({ok}, Response),
	% Test - altready exists
	Response1 = create(Key, Value),
	?assertEqual({error, already_exists_replica}, Response1),
	% Clean-up
	adprep:stop().

read_test() ->
	% Initialise
	adprep:start(),
	Key = 'first',
	Value = "value1",
	% Test - does not exist
	Response = adprep:read(Key),
	?assertEqual({error, timeout}, Response),
	% Test - already exists
	create(Key, Value),
	Response1 = adprep:read(Key),
	?assertEqual({ok, Value}, Response1),
	% Clean-up
	adprep:stop().

write_test() ->
	% Initialise
	adprep:start(),
	Key = 'first',
	Value = "value1",
	NewValue = "new_value",
	% Test - already exist
	create(Key, Value),
	Response1 = adprep:update(Key, NewValue),
	?assertEqual({ok}, Response1),
	Response2 = adprep:read(Key),
	?assertEqual({ok, NewValue}, Response2),
	% Clean-up
	adprep:stop().


%% ============================================================================
getDCs_test() ->
	% Initialise
	adprep:start(),
	Key = 'test',
	% Test
	?assertEqual({exists, []}, gen_server:call(adprep, {get_dcs, Key})),
	% Clean-up
	adprep:stop().

hasReplica_test() ->
	% Initialise
	adprep:start(),
	Key = 'first',
	Id = 0,
	Value = "value",
	% Test - data does not exist
	gen_server:cast(adprep, {has_replica, Id, Key}),
	Result = receive
		{reply, has_replica, Id, {exists, _DCs}} ->
			invalid;
		_ ->
			invalid
	after
		1000 ->
			time_out
	end,
	?assertEqual(time_out, Result),
	% Test - data exist
	create(Key, Value),
	gen_server:cast(adprep, {has_replica, Id, Key}),
	Result1 = receive
		{reply, has_replica, Id, Response1} ->
			Response1
	after
		1000 ->
			false
	end,
	?assertEqual({exits, []}, Result1),
	% Clean-up
	adprep:stop().

flush_test() ->
	?assertEqual({ok}, adprep:flush(none)).

%% ============================================================================
create(Key, Value) ->
	NextDCFunc = fun(_Rl, _AllDCs, _Args) -> {[], []} end,
	adprep:create(Key, Value, NextDCFunc, []).

checkNewId(_Pid, _Id, _ExpectedId, 0) ->
	{ok};
checkNewId(Pid, Id, ExpectedId, NumTimes) when NumTimes > 0 ->
	{reply, 'new_id', Id, Response} = gen_server:call(Pid, {new_id, Id, any}),
	?assertEqual(Response, ExpectedId),
	checkNewId(Pid, Id, ExpectedId+1, NumTimes-1).
