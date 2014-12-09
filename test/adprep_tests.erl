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
	Key = 'getNumReplicas_test',
	% Test - already running process
	NumReplicas = adprep:getNumReplicas(Key),
	?assertEqual(0, NumReplicas),
	% Clean-up
	adprep:stop(),
	erlang:yield().

create_test() ->
	% Initialise
	adprep:start(),
	Key = 'create_test',
	Value = "value",
	% Test - does not exist
	Response = create(Key, Value),
	?assertEqual({ok}, Response),
	% Test - altready exists
	Response1 = create(Key, Value),
	?assertEqual({error, already_exists_replica}, Response1),
	% Clean-up
	adprep:stop(),
	erlang:yield().

read_test() ->
	% Initialise
	adprep:start(),
	Key = 'read_test',
	Value = "value1",
	% Test - does not exist
	Response = adprep:read(Key),
	?assertEqual({error, timeout}, Response),
	% Test - already exists
	create(Key, Value),
	Response1 = adprep:read(Key),
	?assertEqual({ok, Value}, Response1),
	% Clean-up
	adprep:stop(),
	erlang:yield().

write_test() ->
	% Initialise
	adprep:start(),
	Key = 'write_test',
	Value = "value1",
	NewValue = "new_value",
	% Test - already exist
	create(Key, Value),
	Response1 = adprep:update(Key, NewValue),
	?assertEqual({ok}, Response1),
	Response2 = adprep:read(Key),
	?assertEqual({ok, NewValue}, Response2),
	% Clean-up
	adprep:stop(),
	erlang:yield().

remove_test() ->
	% Initialise
	adprep:start(),
	Key = 'remove_test',
	Value = "value",
	% Test - already exist
	create(Key, Value),
	Response1 = adprep:remove(Key),
	?assertEqual({ok}, Response1),
	Response2 = adprep:read(Key),  % should not exists
	?assertEqual({error, timeout}, Response2),
	Respose3 = create(Key, Value), % should be able to create it again
	?assertEqual({ok}, Respose3),
	% Clean-up
	adprep:stop(),
	erlang:yield().

%% ============================================================================
getDCs_test() ->
	% Initialise
	adprep:start(),
	Key = 'getDCs_test',
	% Test
	?assertEqual({exists, []}, gen_server:call(adprep, {get_dcs, Key})),
	% Clean-up
	adprep:stop(),
	erlang:yield().

hasReplica_test() ->
	% Initialise
	adprep:start(),
	Key = 'hasReplica_test',
	Id = 1,
	Value = "value",
	% Test - data does not exist
	gen_server:cast(adprep, {has_replica, self(), Id, Key}),
	Result = receive
		R ->
			R
	after
		1000 ->
			time_out
	end,
	?assertEqual(time_out, Result),
	% Test - data exist
	create(Key, Value),
	gen_server:cast(adprep, {has_replica, self(), Id, Key}),
	Result1 = receive
		{reply, has_replica, Id, Response1} ->
			Response1
	after
		1000 ->
			false
	end,
	?assertEqual({exists, [node()]}, Result1),
	% Clean-up
	adprep:stop(),
	erlang:yield().

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
