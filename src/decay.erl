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
% Unit-test
-compile(export_all).
-else.
-compile(report).
% Interface calls
-export([startDecay/3,stopDecay/1]).
% Callbacks
-export([init/1,buildPid/1]).
-endif.

-include("adprep.hrl").

%% =============================================================================
%% Decay process interface
%% =============================================================================

%% @doc Starts the decay process for the specified key and time period.
% TODO Fix type spec!
%-spec startDecay(integer(), key(), boolean()) -> true.
startDecay(DecayTime, Key, true) ->
    ok = stopDecay(Key),
    erlang:yield(), % give a chance to shutdown
    startDecay(DecayTime, Key, false);
startDecay(DecayTime, Key, false) ->
    DecayKey = buildPid(Key),
    register(DecayKey, spawn_link(decay, init, [{DecayTime, Key}])).

%% @doc Stops the dacay process. Returns 'ok' if no problem was found requesting the stop 
%%        of the process or {error, may_not_exists} otherwise.
% TODO Fix type spec!
%-spec stopDecay(key()) -> ok | {error, does_not_exist}.
stopDecay(Key) ->
    DecayKey = buildPid(Key),
    % Stops the decay process
    % No reply is sent back to sender
    try DecayKey ! shutdown of
        _ ->
            % Succeed
            ok
    catch
        error:badarg ->
            {error, does_not_exist}
    end.

%% =============================================================================
%% Decay process
%% =============================================================================

%% @doc Applies the decay as time passes.
-spec init({non_neg_integer(), key()}) -> ok.
init({Time, Key}) ->
    loop(Time, Key, 0).

%% @doc Processes the messages hold by the mailbox.
-spec loop(non_neg_integer(), key(), non_neg_integer()) -> ok.
loop(Time, Key, Index) ->
    receive
        shutdown ->
            ok
    after 
        Time ->
            strategy_adprep:decay(Key, Index),
            loop(Time, Key, Index+1)
    end.


%% @doc Builds the decay process ID for the specified key.
% TODO Fix type spec!
%%-spec buildPid(list() | atom()) -> key().
buildPid(Key) when is_list(Key) ->
    list_to_atom("decay" ++ Key);
buildPid(Key) when is_atom(Key) ->
    buildPid(atom_to_list(Key)).
