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


-ifdef(EUNIT).
-compile(export_all).
-else.
-compile(report).
% Interface calls
-export([startDecay/3,stopDecay/1]).
% Callbacks
-export([init/1,buildPid/1]).
-endif.


%% =============================================================================
%% Decay process
%% =============================================================================

%% @spec init({Time::integer(), Key::atom()}) -> {ok}
%% 
%% @doc Applies the decay as time passes.
init({Time, Key}) ->
	loop(Time, Key, 0).

%% @spec loop(Time::integer(), Key::atom()) -> {ok}
%% 
%% @doc Processes the messages hold by the mailbox.
loop(Time, Key, Index) ->
	receive
		shutdown ->
			{ok}
	after 
        Time ->
			Key ! {decay, self(), Index},
			loop(Time, Key, Index+1)
	end.

%% =============================================================================
%% Decay process interface
%% =============================================================================

%% @spec startDecay(DecayTime::integer(), Key::atom(), StopPrevious::boolean()) -> true
%% 
%% @doc Starts the decay process for the specified key and time period.
startDecay(DecayTime, Key, true) ->
	stopDecay(Key),
	startDecay(DecayTime, Key, false);
startDecay(DecayTime, Key, false) ->
	DecayKey = buildPid(Key),
	register(DecayKey, spawn_link(decay, init, [{DecayTime, Key}])).

%% @spec stopDecay(Key::atom()) -> Results::tuple()
%%
%% @doc Stops the dacay process. Returns {ok} if no problem was found requesting the stop 
%%		of the process or {error, may_not_exists} otherwise.
stopDecay(Key) ->
	% Stops the decay process
	DecayKey = buildPid(Key),
	% No reply is sent back to sender
	try DecayKey ! shutdown of
		_ ->
			% Succeed
			{ok}
	catch
		error:badarg -> 
			{error, may_not_exists}
	end.

%% @spec buildPid(Key::list()) -> Pid::atom()
%%
%% @doc Builds the decay process ID for the specified key.
buildPid(Key) when is_list(Key) ->
	list_to_atom("decay" ++ Key);
%% @spec buildPid(Key::atom()) -> Pid::atom()
%%
%% @doc Builds the decay process ID for the specified key.
buildPid(Key) when is_atom(Key) ->
	buildPid(atom_to_list(Key)).
