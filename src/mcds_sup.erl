
-module(mcds_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

-define(SERVER, ?MODULE).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, [LSock, Queue]}, temporary, brutal_kill, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(LSock, Queue, Server) ->
    supervisor:start_link(?SERVER, ?MODULE, [LSock, Queue, Server]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([LSock, Queue, Server]) ->
    MCDS_Console_Sup = {mcds_console_sup, {mcds_console_sup, start_link, [LSock]},
                permanent, 2000, supervisor, [mcds_console_sup]},
    MCDS_Sync_Sup = {mcds_sync_sup, {mcds_sync_sup, start_link, [Queue, Server]},
                        permanent, 2000, supervisor, [mcds_sync_sup]},
    Children = [MCDS_Console_Sup, MCDS_Sync_Sup],
    RestartStrategy = {one_for_one, 4, 3600},
    {ok, { RestartStrategy, Children}}.

