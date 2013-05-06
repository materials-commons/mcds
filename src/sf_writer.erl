%%% ===================================================================
%%% @doc API for file transfers.
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

-module(sf_writer).

%% API
-export([send_file/1]).

-include_lib("kernel/include/file.hrl").

-define(PORT, 11011).
-define(UUID, "abc123").

%% ===================================================================
%% API
%% ===================================================================

send_file(Filepath) ->
    {ok, #file_info{size = Size}} = file:read_file_info(Filepath),
    {ok, Socket} = gen_tcp:connect("127.0.0.1", ?PORT,
                        [binary, {packet, raw}, {active, false}]),
    Basename = filename:basename(Filepath),
    Checksum = checksums:md5sum(Filepath),
    BinTerm = term_to_binary([{filename, Basename}, {uuid, ?UUID},
                                {size, Size}, {checksum, Checksum}]),
    io:format("sf_writer: BinTerm = ~p~n", [BinTerm]),
    io:format("sf_writer: Sending ~p~n", [binary_to_term(BinTerm)]),
    gen_tcp:send(Socket, BinTerm),
    io:format("Going into recv~n"),
    {ok, Reply} = gen_tcp:recv(Socket, 0),
    io:format("sf_writer: Got back: ~p~n", [Reply]),
    io:format("  sf_writer as: ~p~n", [binary_to_term(Reply)]),
    case binary_to_term(Reply) of
        already_downloaded -> ok;
        {ok, SizeOfFileOnServer} ->
            send_file_data(Filepath, SizeOfFileOnServer, Socket)
    end,
    gen_tcp:close(Socket),
    ok.

send_file_data(Filepath, SizeOfFileOnServer, Socket) ->
    io:format("SizeOfFileOnServer ~p~n", [SizeOfFileOnServer]),
    file:sendfile(Filepath, Socket).
