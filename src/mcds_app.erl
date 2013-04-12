-module(mcds_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_PORT, 10101).
-define(DEFAULT_MC_SERVER, "materialscommons.org").
-define(DEFAULT_INTERFACE, 1).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Port = get_env_default(port, ?DEFAULT_PORT),
    Server = get_env_default(mc_server, ?DEFAULT_MC_SERVER),
    SyncQueue = get_sync_queue(),
    {ok, LSocket} = gen_tcp:listen(Port, [{active, true}]),
    mcds_sup:start_link(LSocket, Server, SyncQueue).

stop(_State) ->
    ok.

get_env_default(What, Default) ->
    case application:get_env(ds_dir, What) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

get_sync_queue() ->
    Interface = get_env_default(interface, ?DEFAULT_INTERFACE),
    {ok, Address} = get_address(netutil:get_external_addrs(), Interface),
    string:concat("/queue/", Address).

get_address([], Interface) ->
    {error, no_interfaces};
get_address(Interfaces, InterfaceIndex) when is_integer(InterfaceIndex) ->
    {_InterfaceName, Address} = lists:nth(InterfaceIndex, Interfaces),
    {ok, Address};
get_address(Interfaces, InterfaceName) ->
    {_IName, Address} = lists:keyfind(InterfaceName, 1, Interfaces),
    {ok, Address}.