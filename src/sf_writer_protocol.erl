-module(sf_writer_protocol).
-behaviour(gen_server).
-behaviour(ranch_protocol).

-include_lib("kernel/include/file.hrl").

%% API (ranch) callback
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-record(state, {socket, transport, lref, fd}).

%% ===================================================================
%% API (ranch) functions
%% ===================================================================

start_link(ListenerRef, Socket, Transport, _Opts) ->
    gen_server:start_link(?MODULE, [ListenerRef, Socket, Transport], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([ListenerRef, Socket, Transport]) ->
    {ok, #state{socket = Socket, transport = Transport, lref = ListenerRef, fd = not_open}, 0}.

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RequestData},
            #state{socket = Socket, transport = Transport} = State)
                when State#state.fd =:= not_open ->
    {ok, Filename, Uuid, Size, Checksum} = splitout_request_data(RequestData),
    Filepath = construct_file_path(Uuid, Filename),
    DownloadedSize = get_file_size(Filepath),
    case size_and_checksum_match(Filepath, Size, DownloadedSize, Checksum) of
        true ->
            send_already_downloaded(Socket, Transport),
            {stop, normal, State};
        _ ->
            NewState = prepare_download(Filepath, DownloadedSize, State),
            ok = Transport:setopts(Socket, [{active, once}]),
            {noreply, NewState}
    end;
handle_info({tcp, Socket, RawData},
            #state{socket = Socket, transport = Transport, fd = Fd} = State) ->
    ok = file:write(Fd, RawData),
    ok = Transport:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info(timeout, #state{socket = Socket, transport = Transport, lref = LRef} = State) ->
    ok = ranch:accept_ack(LRef),
    ok = Transport:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, _Socket}, #state{fd = Fd} = State) ->
    file:close(Fd),
    % DO WE NEED TO DO Transport:close(Socket)?
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% Should we close the file here?
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

send_already_downloaded(Socket, Transport) ->
    Transport:send(Socket, jiffy:encode([{status, already_downloaded}, {size, 0}])).

prepare_download(Filepath, FileSize,
            #state{socket = Socket, transport = Transport, fd = Fd} = State) ->
    {ok, Fd} = open_file(Filepath, FileSize),
    Transport:send(Socket, jiffy:encode([{status, ok}, {size, FileSize}])),
    State#state{fd = Fd}.

size_and_checksum_match(Filepath, Size, DownloadedSize, Checksum) ->
    case Size =:= DownloadedSize of
        true ->
            DownloadedChecksum = checksums:md5sum(Filepath),
            Checksum =:= DownloadedChecksum;
        false ->
            false
    end.

open_file(Filepath, FileSize) ->
    case FileSize of
        0 ->
            file:open(Filepath, [raw, binary, write]);
        _ ->
            file:open(Filepath, [raw, binary, append])
    end.

get_file_size(Filename) ->
    case file:read_file_info(Filename) of
        {ok, FileInfo} ->
            FileInfo#file_info.size;
        {error, enoent} ->
            0
    end.

splitout_request_data(RequestData) ->
    {[{_, Filename}, {_, Uuid}, {_, Size}, {_, Checksum}]} = jiffy:decode(RequestData),
    {ok, Filename, Uuid, Size, Checksum}.

construct_file_path(Uuid, Filename) ->
    filename:join(["/data", Uuid, Filename]).


