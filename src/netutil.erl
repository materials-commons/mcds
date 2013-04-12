-module(netutil).
-export([get_external_addrs/0]).

get_external_addrs() ->
    {ok, Interfaces} = inet:getifaddrs(),
    NonLocalAddresses = lists:filter(fun is_external_addr/1, Interfaces),
    lists:map(fun addr_tuple_to_string/1, NonLocalAddresses).

is_external_addr({_IName, Values}) ->
    case lists:keyfind(addr, 1, Values) of
        {addr, {127,0,0,1}} -> false;
        _ -> true
    end.

addr_tuple_to_string({Interface, Values}) ->
    {addr, {Oct1, Oct2, Oct3, Oct4}} = lists:keyfind(addr, 1, Values),
    {Interface, lists:flatten(io_lib:format("~b.~b.~b.~b", [Oct1, Oct2, Oct3, Oct4]))}.
