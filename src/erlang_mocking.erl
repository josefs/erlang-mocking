-module(erlang_mocking).

-export([foo/0]).

foo() ->
    put(foo,1).
