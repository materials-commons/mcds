-module(mc_config).

%% API
-export([get_env_default/2, get_env_default/3]).

get_env_default(What, Default) ->
    get_env_default(mcds, What, Default).

get_env_default(App, What, Default) ->
    case application:get_env(App, What) of
        {ok, Value} -> Value;
        undefined -> Default
    end.