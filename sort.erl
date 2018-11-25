%
% Erlang quicksort implemented.
%

-module(sort).
-export([quicksort/1]).

quicksort([]) -> [];
quicksort([Pivot | T]) ->
    quicksort([X || X <- T, X < Pivot]) ++
    [Pivot] ++
    quicksort([Y || Y <- T, Y >= Pivot]).
