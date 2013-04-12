-module(ds_sync_server).
-behaviour(gen_stomp).

%% API
-export([start_link/2]).

%% Gen Stomp callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-define(DEFAULT_STOMP_PORT, 61613).

%% Figure out where this really belongs, and where we should do
%% the look up (probably in the application, or top level supervisor).
-define(DEFAULT_STOMP_USERNAME, "guest").
-define(DEFAULT_STOMP_PASSWORD, "guest").

-record(state, {server}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Queue, Server) ->
    Port = mc_config:get_env_default(stomp_port, ?DEFAULT_STOMP_PORT),
    Username = mc_config:get_env_default(stomp_username, ?DEFAULT_STOMP_USERNAME),
    Password = mc_config:get_env_default(stomp_password, ?DEFAULT_STOMP_PASSWORD),
    gen_stomp:start_link(?MODULE, Server, Port, Username, Password, [{Queue, []}], []).

%% ===================================================================
%% gen_stomp callbacks
%% ===================================================================

init([]) ->
    {ok, #state{server = "something_here"}}.

handle_call(Message, _From, State) ->
    {reply, {ok, Message}, State}.

handle_cast({stomp, Message}, State) ->
    %% Start ds_files for directories to sync
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Ignore, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

