-module(mcsf_writer_sup).
-behaviour(supervisor).

%% API
-export([start_link/2, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(LSock, Queue) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock, Queue]).

start_child() ->
    supervisor:start_child(?SERVER, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([LSock, Queue]) ->
    SaveFileWriterProtocol = {sf_writer_protocol,
                            {sf_writer_protocol, start_link, [LSock, Queue]},
                            temporary, brutal_kill, worker, [sf_writer_protocol]},
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, [SaveFileWriterProtocol]}}.