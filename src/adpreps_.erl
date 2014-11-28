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

-ifdef(EUNIT).
-compile(export_all).
-else.
-compile(report).
-export([create/5, read/2, write/3, send/2, send/3, stop/1]).
-endif.
%-import(dcs, [getNewID/0, sendToAllDCs/1]).


%% =============================================================================
%% Adaptive Replication support
%% =============================================================================
%% @spec create(Key, Id::integer(), Value, Strategy::float(), Args::tuple()) -> Result::tuple()
%% 
%% @doc Creates the first instance of the specified data in this DC. The results is of the format 
%%		{ok} or {error, ErrorCode}.
create(Key, Id, Value, Strategy, Args) ->
	send(Key, {create, self(), Id, {Value, Strategy, Args}}).

%% @spec read(Key, Id::integer) -> Result::tuple()
%% 
%% @doc Reads the value of the specified data. The results is of the format {ok, Value} or 
%%		{error, ErrorCode}.
read(Key, Id) ->
	send(Key, {read, self(), Id}).

%% @spec write(Key, Id::integer(), Value) -> Result::tuple()
%% 
%% @doc Writes the new value of the specified data. The results is of the format {ok} or 
%%		{error, ErrorCode}.
write(Key, Id, Value) ->
	send(Key, {read, self(), Id, Value}).

%% @spec stop(Key) -> Result::tuple()
%% 
%% @doc Requests to stop the process for the specified data. The results is of the format {ok} or 
%%		{error, ErrorCode}.
stop(Key) ->
	send(Key, {stop, self(), 0}).

%% @spec send(Key, Msg) -> Response::tuple()
%% 
%% @doc Sends the specified message for the specified data and wait for the replay, synchronous. The 
%%		results is of the format {ok, Value} or {error, ErrorCode}.
send(Key, Msg) ->
	send(Key, Msg, true).
%% @spec send(Key, Msg, WaitReply::boolean()) -> Response::tuple()
%% 
%% @doc Sends the specified message for the specified data and wait for the replay if WaitReply is 
%%		true. The results is of the format {ok, Value}, {ok, without_replay}, 
%%		{error, invalid_msg_format} or {error, timeout}.
send(Key, Msg, WaitReply) ->
    {Type, Id, Msg1} = case Msg of
        {create, Pid2, Id2, {Value, Strategy, Args}} ->
			Result = startProcess(Key, Strategy, [], Args),
			if
				Result == created ->
				    {create, Id2, {create, Pid2, Id2, Value}};

				true ->
					{error, none, already_exist}
			end;

		{Type1, _Pid, Id2, _Value} ->
            % Message to send to the strategy process
            {Type1, Id2, Msg};

		{Type1, _Pid, Id2} ->
            % Message to send to the strategy process
            {Type1, Id2, Msg};

		_ ->
            {error, none, {error, invalid_msg_format}}
    end,
	try
        case Type of
            error ->
                % Message is not supported
                Msg1;

			_ ->
        		sendIt(Key, Type, Id, Msg1, WaitReply)
        end
	catch
        _ ->
			% The process for that data does not exist yet
	        Id1 = dsc:getNewID(),
			dcs:sendToAllDCs({has_replica, self(), Id1}),
			receive
				{reply, has_replica, _, Id1, {ok, {Strategy1, DCs, Args1}}} ->
					startProcess(Key, Strategy1, DCs, Args1),
					sendIt(Key, Type, Id, Msg1, WaitReply)
			after 
				6000 ->
					{error, timeout}
			end
	end.

%% @spec sendIt(Key, Type, Id, Msg1, WaitReply::boolean()) -> Response::tuple()
%% 
%% @doc Sends the specified message of the specified type (Type) and identifier (Id) and waits for 
%%		reply if WaitReply is true. The results is of the format {ok, Value}, {ok, without_replay} 
%%		or {error, timeout}.
sendIt(Key, Type, Id, Msg, WaitReply) ->
    % Send message
    Key ! Msg,
	if
		WaitReply ->
		% Wait to get reply
        receive 
	        {reply, Type, Key, Id, Response} ->
        		Response
        after 
        	1000 -> 
        		{error, timeout}
		end;

		true ->
			% Don't wait for reply
			{ok, without_replay}
    end.

%% @spec startProcess(Key, Strategy::atom(), Dcs::list(), Args::tuple()) -> Result::atom()
%% 
%% @doc Sends the specified message for the specified data. The result may be already_exist, ok or 
%%		created.
startProcess(Key, StrategyName, Dcs, Args) ->
	% Check if the process already exists, i.e. the data is known to exist
	Result = nodes(Key),
	case Result of
		[nonode@nohost] ->
			% The process does not exists yet
		    % Start the strategy process
			Strategy = "strategy_" + StrategyName,
		    Pid = spawn(Strategy, run, {Key, Dcs, Args}),
		    R = register(Key, Pid),
			if
				R == undefined ->
					% The process alreday exist, so the data already exists locally or somewhere 
					% else.
					% Stop the started process
					Pid ! {stop, self(), 0},
					receive
						{reply, stop, Key, 0, _Response} ->
							already_exist
					after
						1000 -> 
							already_exist
					end;

				true ->
				    % Succeed
				    created
			end;

		_ ->
			% The process already exist, so the data already exists locally or somewhere else
			already_exist
	end.
