%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 SyncFree Consortium.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
%% =============================================================================
%% Adaptive Replication
%% 
%% @author Amadeo Asco, Annette Bieniusa
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

%% 
%% @doc Internal dispatch of messages.
-module(adpreps_).
-author(['aas@trifork.co.uk','bieniusa@cs.uni-kl.de']).

-ifdef(TEST).
-compile(export_all).
-else.
-compile(report).
-export([create/4, read/1, write/2]).
-endif.

-include("adprep.hrl").

%% =============================================================================
%% Public API
%% =============================================================================
%% @doc Creates the first instance of the specified data in this DC. 
%TODO Check if it has been created in other DCs already.
-spec create(key(), value(), strategy(), args()) -> ok | {error, reason()}.
create(Key, Value, StrategyName, Args) ->
    Strategy = list_to_atom(string:concat("strategy_", StrategyName)),
    Result = gen_server:start_link({global, Key}, Strategy, {Key, Value, Args}, []),
    case Result of
        {ok,_Pid}      -> ok;
        {error,_Error} -> {error, _Error};
        ignore         -> {error, ignored}
    end. 

%% @doc Reads the value of the specified data. 
-spec read(key()) -> {ok, value()} | {error, reason()}.
read(Key) ->
	send(Key, {read}, true).

%% @doc Writes the new value of the specified data. 
-spec write(key(), value()) -> ok | {error, reason()}.
write(Key, Value) ->
	send(Key, {write, Value}, false).

%% @doc Sends the specified message for the specified data and 
%%      wait for the replay if WaitReply is true. 
%TODO: Refine type!
%FIXME: Check if process exists and possibly forward to other DCs.
-spec send(key(), term(), boolean()) -> term().
send(Key, Msg, WaitForReply) ->
    case WaitForReply of
        true ->
            Reply = gen_server:call(Key, Msg, infinity),
            case Reply of
                {reply, Response} ->
                    Response;
                Response ->
                    Response
            end;
        false ->
            gen_server:cast(Key, Msg)
    end.

