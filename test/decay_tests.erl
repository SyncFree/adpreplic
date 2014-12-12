%% =============================================================================
%% EUnits for the deycay process - SyncFree
%% 
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================


-module(decay_tests).
-author('aas@trifork.co.uk').

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).


-include_lib("eunit/include/eunit.hrl").


%% ====================================================================
%% Internal functions
%% ====================================================================
buildPid_test() ->
	Value = decay:buildPid("_test"),
	?assertEqual(decay_test, Value).

startStopDecay_test() ->
	% Initialise
	DecayTime = 10000,
	Key = test,
	StopPrevious = false,
	Name = decay:buildPid(Key),
	% Test - it is not already running
	Reg = registered(),
	?assertEqual(false, lists:member(Name, Reg)),
	% Test - Initialy the decays process is not running, test it, then it is started and 
	%		 should be running, so test it too
	decay:startDecay(DecayTime, Key, StopPrevious),
	Reg1 = registered(),
	?assertEqual(true, lists:member(Name, Reg1)),
	% Stop decay test and test it is not running anymore
	decay:stopDecay(Key),
	timer:sleep(100),
	Reg2 = registered(),
	?assertEqual(false, lists:member(Name, Reg2)).

startStartDecay_test() ->
	% Initialise
	DecayTime = 10000,
	Key = "test",
	Name = decay:buildPid(Key),
	% Test - it is not already running
	Reg = registered(),
	?assertEqual(false, lists:member(Name, Reg)),
	% Test - It does not axist any previous decay process so call to startDecay should 
	%        failed
	decay:startDecay(DecayTime, Key, false),
	Reg1 = registered(),
	?assertEqual(true, lists:member(Name, Reg1)),
	% Test - It is already running
	decay:startDecay(DecayTime, Key, true),
	Reg2 = registered(),
	?assertEqual(true, lists:member(Name, Reg2)),
	decay:stopDecay(Key).

startStopStoppedDecay_test() ->
	% Initialise
	Key = "test",
	Name = decay:buildPid(Key),
	% Test - it is not already running
	Reg = registered(),
	?assertEqual(false, lists:member(Name, Reg)),
	% Test - It does not axist any previous decay process so call to startDecay should 
	%        failed
	Result = decay:stopDecay(Key),
	?assertEqual({error, does_not_exist}, Result).
