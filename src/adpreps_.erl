%% =============================================================================
%% Adapive Replications - SyncFree
%% 
%% DC interface
%% 
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

%% 
%% @doc Provides operations required in a database.
-module(adpreps_).
-author('aas@trifork.co.uk').

-include("adprep.hrl").

-ifdef(EUNIT).
-compile(export_all).
-else.
-compile(report).
-export([buildReply/2, buildReply/3, create/4, delete/1, read/1, update/2, send/2, 
         send/3, stop/1]).
-export([getNewID/0, getNewID/1,getReplicationLayerPid/1]).
-endif.


%% =============================================================================
%% Adaptive Replication support
%% =============================================================================
%% @spec create(Key::atom(), Value, Strategy::float(), Args::tuple()) -> Result::tuple()
%% 
%% @doc Creates the first instance of the specified data in this DC. The results is of 
%%      the format ok or {error, ErrorCode}.
%%
%%      Returns: {reply,create,ok} | {reply,create,{error, already_exists_replica}}.
create(Key, Value, Strategy, Args) ->
    send(Key, {create, {Value, Strategy, Args}}).

%% @spec read(Key::atom()) -> Result::tuple()
%% 
%% @doc Reads the value of the specified data. The results is of the format {ok, Value} 
%%      or {error, ErrorCode}.
read(Key) ->
    send(Key, {read}).

%% @spec update(Key::atom(), Value::term()) -> Result::tuple()
%% 
%% @doc Writes the new value of the specified data. The results is of the format ok or 
%%      {error, ErrorCode}.
update(Key, Value) ->
    send(Key, {write, Value}).

%% @spec delete(Key::atom())-> Result::tuple()
%% 
%% @doc Deletes the data in all DCs where there is a replica. The results may have any of 
%%      the values ok or {error, ErrorCode::term()}.
delete(Key) ->
    try send(Key, {delete}) of 
        Result ->
            Result
    catch
        error:badarg ->
            {error, does_not_exist}
    end.

%% @spec stop(Key::atom()) -> Result::tuple()
%% 
%% @doc Requests to stop the process for the specified data. The results is of the format ok or 
%%      {error, ErrorCode}.
stop(Key) ->
    send(Key, shutdown, false).


%% =============================================================================
%% 
%% =============================================================================

%% @spec getAllDCs() -> DCs::List
%% 
%% @doc The list of all the DCs.
getAllDCs() ->
    nodes().

%% @doc Provides a new ID for the specified key.
-spec getNewID(key()) -> integer().
getNewID(Key) ->
    Pid = getReplicationLayerPid(Key),
    {reply, new_id, 0, Results} = gen_server:call(Pid, {new_id, 0, Key}, 1000),
    Results.

%% @doc Provides a new ID.
-spec getNewID() -> integer().
getNewID() ->
    Key = process_info(self(), registered_name),
    getNewID(Key).

%% @doc Provides the local Replication Layer process ID for the specified data.
%% @spec getReplicationLayerPid(Key::atom()) -> Pid::pid()
getReplicationLayerPid(_Key) ->
    getReplicationLayerPid().
getReplicationLayerPid() ->
%    list_to_atom(string:concat(Key, "_rl")).
    'rl'.

%% @doc Sends the specified message to all the DCs.
-spec sendToAllDCs(key(), {'has_replica', pid(), integer(), atom()}) -> ok.
sendToAllDCs(Key, Msg) ->
    sendToDCs(getAllDCs(), Key, Msg).

%% @doc Sends the specified message to each of the provided list of DCs.
-spec sendToDCs([atom()], key(), {'has_replica', pid(), integer(), atom()}) -> ok.
sendToDCs([], _Key, _Msg) ->
    ok;
sendToDCs([Dc | DCs], Key, Msg) ->
    try 
        gen_server:cast({Key, Dc}, Msg)
    after
        sendToDCs(DCs, Key, Msg)
    end.

%% @doc Sends the specified message for the specified data and wait for the replay, 
%%      synchronous.
 -spec send(key(), _ ) -> {create, {create, _ }} | {error, {error, _ }}.
send(Key, Msg) ->
    send(Key, Msg, true).

%% @doc Sends the specified message for the specified data and wait for the replay if 
%%      WaitReply is true. The results is of the format {ok, Value}, 
%%      {ok, without_replay}, {error, invalid_msg_format} or {error, timeout}.
-spec send(key(), _ , boolean()) -> {create, {create, term()}} | {error, {error, _ }}.
send(Key, Msg, WaitReply) ->
    {Type1, Msg1} = case Msg of
        {create, {Value, Strategy, Args}} ->
            case startProcess(Key, Strategy, Args) of
                created ->
                    {create, {create, Value}};
                already_started ->
                    {error, {error, already_exists}};
                {error, Reason} ->
                    {error, {error, Reason}}
            end;

        {Type, _Value} ->
            % Message to send to the strategy process
            {Type, Msg};

        {Type} ->
            % Message to send to the strategy process
            {Type, Msg};

        shutdown ->
            % Message to send to the strategy process
            {Msg, Msg};
            
        _ ->
            {error, {error, invalid_msg_format}}
    end,
    try
        case Type1 of
            error ->
                % Message is not supported
                Msg1;
            delete ->
                Reply = try gen_server:call(Key, Msg, 5000) of
                    {reply, Type1, Response} ->
                        Response;
                    Response ->
                        Response
                catch
                    exit:{noproc,R} ->
                        {error, does_not_exist}
                end;
            _ ->
                sendIt(Key, Type1, Msg1, WaitReply)
        end
    catch
        _ ->
            if
                Msg1 =:= shutdown ->
                    {ok, already_stopped};
                true ->
                    % The process for that data does not exist yet
                    Id1 = getNewID(Key),
                    ok = sendToAllDCs(Key, {has_replica, self(), Id1, Key}),
                    receive
                        {reply, has_replica, _, {ok, {Strategy1, _DCs, Args1}}} ->
                            %TODO Fix error case!
                            _ = startProcess(Key, Strategy1, Args1),
                            sendIt(Key, Type1, Msg1, WaitReply)
                    after 
                        6000 ->
                            {error, timeout}
                end
            end
    end.

%% @doc Sends the specified message of the specified type (Type) and waits for reply if 
%%      WaitReply is true. 
-spec sendIt(key(), _, _, boolean()) -> ok | {ok, term()} | {ok, without_replay} | {error, timeout}.
sendIt(Key, Type, Msg, WaitReply) ->
    case WaitReply of
         true ->
            case gen_server:call(Key, Msg, 5000) of
                {reply, Type, Key, Response} ->
                    Response;
                {reply, Type, Response} ->
                    Response;
                Response ->
                    Response
            end;
        false ->
            % Don't wait for reply
            gen_server:cast(Key, Msg),
            ok
    end.

%% @doc Sends the specified message for the specified data. The result may be 
%%      already_exist, ok or created.
-spec startProcess(key(), atom(), tuple()) -> already_started | created | ignore | {error, _ }.
startProcess(Key, StrategyName, Args) ->
    % Start the strategy process
    case gen_server:start({local, Key}, buildPid(StrategyName), % Strategy
                         {Key, Args}, []) of 
        {ok, _Pid} ->
            created;
        {error, {already_started, _Pid}} ->
            % The process already exist
            already_started;
        Result ->
            % Probablly the process already exist, so the data already exists locally or 
            % somewhere else
            %% TODO: verify this is the case in all circunstatnces
            Result
    end.

%% @spec buildPid(Key::list()) -> Pid::atom()
%%
%% @doc Builds the decay process ID for the specified key.
buildPid(Key) when is_list(Key) ->
    list_to_atom("strategy_" ++ Key);
%% @spec buildPid(Key::atom()) -> Pid::atom()
%%
%% @doc Builds the decay process ID for the specified key.
buildPid(Key) when is_atom(Key) ->
    buildPid(atom_to_list(Key)).

%% @spec buildReply(Type::atom(), Id::integer(), Result) -> Msg::atom()
%%
%% @doc Builds the reply from the passed parameters.
buildReply(Type, Id, Results) ->
    {reply, Type, Id, Results}.
%% @spec buildReply(Type::atom(), Id::integer()) -> Msg::atom()
%% @doc Builds the reply from the passed parameters. Id could be an integer or the result.
buildReply(Type, Id) ->
    {reply, Type, Id}.
