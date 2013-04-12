-module(mcds_sync_sup).
-behaviour(supervisor).

%% API
-export([start_link/2, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Queue, Server) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Queue, Server]).

start_child() ->
    supervisor:start_child(?SERVER, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Queue, Server]) ->
    SyncServer = {ds_sync_server, {ds_sync_server, start_link, [Queue, Server]},
                    temporary, brutal_kill, worker, [ds_sync_server]},
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, [SyncServer]}}.