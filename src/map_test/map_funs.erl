-module(map_funs).
-export([map_search_pred/2]).


map_search_pred(Map, Pred) ->
    lists:foldl(
      fun({K,V}, Acc) ->
          case Acc of
              none ->
                  case Pred(K,V) of
                      true -> {K,V};
                      false -> none
                  end;
              Found -> Found
          end
      end,
      none,
      maps:to_list(Map)
    ).
