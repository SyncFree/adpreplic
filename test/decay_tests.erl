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
	DecayTime = 1000,
	Key = test,
	StopPrevious = false,
	Name = decay:buildPid(Key),
	% Test - Initialy the decays process is not running, test it, then it is started and 
	%		 should be running, so test it too
	?assertEqual(undefined, whereis(Name)),
	register(Key, self()),
	decay:startDecay(DecayTime, Key, StopPrevious),
	?assertNotEqual(undefined, whereis(Name)),
	Result = receive
		{decay, _Pid, 0} -> 
			{ok}
	after
		DecayTime ->
			{error, timeout}
	end,
	?assertNotEqual({ok}, Result),
	% Test that we are receiving decay notifications
	repeat(DecayTime, Key, 1),
	% Stop decay test and test it is not running
	decay:stopDecay(Key),
	timer:sleep(100),
	?assertEqual(undefined, whereis(Name)).

startStartDecay_test() ->
	% Initialise
	DecayTime = 1000,
	Key = "test",
	StopPrevious = true,
	Name = decay:buildPid(Key),
	% Test - It does not axist any previous decay process so call to startDecay should 
	%        failed
	?assertEqual(undefined, whereis(Name)),
	decay:startDecay(DecayTime, Key, StopPrevious),
	?assertNotEqual(undefined, whereis(Name)),
	decay:stopDecay(Key).


%%%%%%
repeat(_DelayTime, _Key, 0) ->
	{ok};
repeat(DelayTime, Key, Num) when Num > 0 ->
	Result = receive
		{decay, _Pid, 0} ->
			repeat(DelayTime, Key, Num-1);
		_ ->
			{error}
	after
		DelayTime+100 ->
			{error}
	end,
	?assertEqual({ok}, Result).
