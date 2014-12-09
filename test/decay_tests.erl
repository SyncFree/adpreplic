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
	?assertEqual(decay:buildPid("test_"), decay).

startStopDecay_test() ->
	% Initialise
	DecayTime = 1000,
	Key = test,
	StopPrevious = false,
	Name = decay:buildPid(Key),
	% Test - Initialy the decays process is not running, test it, then it is started and 
	%		 should be running, so test it too
	?assertEqual(whereis(Name), undefined),
	decay:startDecay(DecayTime, Key, StopPrevious),
	?assertNotEqual(whereis(Name), undefined),
	% Test that we are receiving decay notifications
	repeat(DecayTime, Key, 1),
	% Stop decay test and test it is not running
	decay:stopDecay(Key),
	timer:sleep(200),
	?assertEqual(whereis(Name), undefined).

startStartDecay_test() ->
	% Initialise
	DecayTime = 1000,
	Key = "test",
	StopPrevious = true,
	Name = decay:buildPid(Key),
	% Test - It does not axist any previous decay process so call to startDecay should 
	%        failed
	?assertEqual(whereis(Name), undefined),
	?assertException(error, function_clause, decay:startDecay(DecayTime, Key, StopPrevious)),
	?assertNotEqual(whereis(Name), undefined).


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
	?assertEqual(Result, {ok}).
