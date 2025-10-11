-module(mysort).
-export([custom_sort/1]).


custom_sort([]) -> [];
custom_sort([H | T]) -> 
    Big = [X || X <- T, X >= H],
    Small = [X || X <- T, X <H],
    custom_sort(Big) ++ [H] ++ custom_sort(Small).
