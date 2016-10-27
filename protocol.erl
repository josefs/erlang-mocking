-module(protocol).
-author(josef).

-include_lib("proper/include/proper.hrl").

-export([send/1,rec/0]).
-export([protocol_prop/0]).

send(Msg) ->
    Parts = lists:split(Msg),
    lists:foreach(fun (L) ->
			  lower:send(L)
		  end
		 ,Parts).

rec() ->
    Msg = lower:rec(),
    lists:concat(Msg).

protocol_prop() ->
    ?FORALL(Msg,gen_msg(),
          begin
	      put(mock,[]),
	      meck:new(lower),
	      meck:expect(lower,send,fun (L) ->
					     Apa = get(mock),
					     put(mock,[L|Apa])
				     end),
	      meck:expect(lower,rec, fun () ->
					     get(mock)
				     end),
	      send(Msg),
              erase(mock),
	      meck:unload(lower),
	      Msg == rec() andalso
		  meck:verify_expects()
	  end).

gen_msg() ->
    string().
