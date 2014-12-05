%% =============================================================================
%% EUnits for Adapive Replications DC - SyncFree
%% 
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

-module(dcs_tests).
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
startReplicationLayer_test() ->
	?assertEqual(whereis(dcs:getReplicationLayerPid()), undefined),
	dcs:startReplicationLayer(),
	?assertNotEqual(whereis(dcs:getReplicationLayerPid()), undefined),
	gen_server:cast(dcs:getReplicationLayerPid(), stop),
	?assertEqual(whereis(dcs:getReplicationLayerPid()), undefined).

getReplicationLayerPid_test() ->
	Key = 'test',
	?assertEqual(dcs:getReplicationLayerPid(Key), 'rl').
