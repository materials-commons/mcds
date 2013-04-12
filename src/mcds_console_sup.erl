-module(mcds_console_sup).
-behaviour(supervisor).

-define(SERVER, ?MODULE).

%% API
-export([start_link/1, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(LSock) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock]).

start_child() ->
    supervisor:start_child(?SERVER, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([LSock]) ->
    ConsoleServer = {ds_console_server, {ds_console_server, start_link, [LSock]},
                        temporary, brutal_kill, worker, [ds_console_server]},
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, [ConsoleServer]}}.