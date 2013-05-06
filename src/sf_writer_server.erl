%%% ===================================================================
%%% @doc Started on a file write request. Accepts file data and writes
%%%      it to the file system. Also updates a minimal set of meta data,
%%%      such as .
%%%
%%% Copyright (c) 2013, Regents of the University of Michigan.
%%% All rights reserved.
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%% ===================================================================

-module(sf_writer_server).
-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").

%% API callback
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-record(state, {socket, fd}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([LSock]) ->
    {ok, #state{socket = LSock, fd = not_open}, 0}.

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RequestData}, #state{} = State)
                when State#state.fd =:= not_open ->
    io:format("~p handle_info got data~n", [self()]),
    RequestDataBin = list_to_binary(RequestData),
    {ok, Filename, Uuid, Size, Checksum} = splitout_request_data(RequestDataBin),
    Filepath = construct_file_path(Uuid, Filename),
    DownloadedSize = get_file_size(Filepath),
    case size_and_checksum_match(Filepath, Size, DownloadedSize, Checksum) of
        true ->
            send_already_downloaded(Socket),
            {stop, normal, State};
        _ ->
            NewState = prepare_download(Filepath, DownloadedSize, State),
            {noreply, NewState}
    end;
handle_info({tcp, _Sock, RawData}, #state{fd = Fd} = State) ->
    io:format("~p handle_info fd open~n", [self()]),
    ok = file:write(Fd, RawData),
    {noreply, State};
handle_info(timeout, #state{socket = Socket} = State) ->
    {ok, _Sock} = gen_tcp:accept(Socket),
    io:format("~p Past accept~n", [self()]),
    sf_writer_server_sup:start_child(),
    {noreply, State};
handle_info({tcp_closed, _Socket}, #state{fd = Fd} = State) ->
    file:close(Fd),
    {stop, normal, State};
handle_info(Info, State) ->
    io:format("~p State = ~p~n", [self(), State]),
    io:format("Info = ~p~n", [Info]),
    %io:format("As binary ~p~n", [list_to_binary(Info)]),
    %io:format("As term ~p~n", [binary_to_term()])
    %io:format("handle_info fall through: ~p~n", [binary_to_term(list_to_binary(Info))]),
    {noreply, State}.

%% Should we close the file here?
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

send_already_downloaded(Socket) ->
    gen_tcp:send(Socket, term_to_binary(already_downloaded)).

prepare_download(Filepath, FileSize, #state{socket = Socket} = State) ->
    {ok, Fd} = open_file(Filepath, FileSize),
    io:format("~p Sending {ok, FileSize} on socket as ~p~n", [self(), term_to_binary({ok, FileSize})]),
    gen_tcp:send(Socket, term_to_binary({ok, FileSize})),
    io:format("~p Done sending {ok, FileSize}~n", [self()]),
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
    [{_, Filename}, {_, Uuid}, {_, Size}, {_, Checksum}] = binary_to_term(RequestData),
    {ok, Filename, Uuid, Size, Checksum}.

construct_file_path(Uuid, Filename) ->
    filename:join(["/tmp", Uuid, Filename]).


