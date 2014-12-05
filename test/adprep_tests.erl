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


flush_test() ->
	?assertEqual(adprep:flush(""), {ok}).

getNewId_test() ->
	% Initialise
	dcs:startReplicationLayer(),
	% Test
	checkNewId(dcs:getReplicationLayerPid(), 0, 0, 3),
	% Clean-up
	gen_server:cast(dcs:getReplicationLayerPid(), stop).

getNumReplicas_test() ->
	% Initialise
	dcs:startReplicationLayer(),
	Id = 0,
	% Test
	{reply, 'num_replicas', Id, NumReplicas} = gen_server:call(dcs:getReplicationLayerPid(), {num_replicas, Id, any}),
	?assertEqual(NumReplicas, 8),
	% Clean-up
	gen_server:cast(dcs:getReplicationLayerPid(), stop).

checkNewId(Pid, Id, ExpectedId, 0) ->
	{ok};
checkNewId(Pid, Id, ExpectedId, NumTimes) when NumTimes > 0 ->
	{reply, 'new_id', Id, Response} = gen_server:call(Pid, {new_id, Id, any}),
	?assertEqual(Response, ExpectedId),
	checkNewId(Pid, Id, ExpectedId+1, NumTimes-1).
