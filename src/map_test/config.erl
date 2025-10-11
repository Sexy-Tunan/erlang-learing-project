-module(config).
-export([read_config/1, validate/1]).

%% 读取 JSON 文件并转成 map
read_config(File) ->
    {ok, Bin} = file:read_file(File),
    Map = maps:form_json(Bin),
    Map.

%% 对配置 map 做合理性检查
validate(Config) ->
    case maps:get(<<"port">>, Config, undefined) of
        undefined -> {error, no_port};
        Port when is_integer(Port), Port > 0, Port < 65536 ->
            ok;
        _ -> {error, bad_port}
    end.
