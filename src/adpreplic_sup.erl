-module(adpreplic_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    %% The supervisor will start the Datastore application
    lager:info("Starting datastore application"),
    Datastore = {datastore, {datastore, start, []},
                 permanent, 5000, worker, [datastore]},

    {ok, { {one_for_one, 5, 10}, [Datastore]} }.

