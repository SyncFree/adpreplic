%% =============================================================================
%% Adapive Replication Decay - SyncFree
%% 
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

%% 
%% @doc Provides operations required in a database.
-module(decay).
-author('aas@trifork.co.uk').

-compile(export_all).
-export([startDecay/3,stopDecay/1]).


%% =============================================================================
%% Decay process
%% =============================================================================
%% @spec runDecay(Time::integer(), Key) -> {ok}
%% 
%% @doc Applies the decay as time passes.
runDecay(Time, Key) ->
	receive
		{stop} ->
			{ok}
	after 
        Time ->
		Key ! {decay},
		runDecay(Time, Key)
	end.

%% @spec startDecay(DecayTime::integer(), Key, StopPrevious::boolean()) -> true
%% 
%% @doc Starts the decay process for the specified key and time period.
startDecay(DecayTime, Key, StopPrevious) ->
	DecayKey = Key + "decay",
	if
		StopPrevious ->
			DecayKey ! {stop}
	end,
	Pid = spawn(decay, runDecay, [DecayTime | Key]),
	register(DecayKey, Pid).

%% @spec stopDecay(Key) -> {ok}
%%
%% @doc Stops the dacay process. No reply is sent back to sender.
stopDecay(Key) ->
	% Stops the decay process
	DecayKey = Key + "decay",
	DecayKey ! {stop},
	{ok}.
