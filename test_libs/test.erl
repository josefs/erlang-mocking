-module(test).
-author(Josef Svenningsson).

-include_lib("eunit/include/eunit.hrl").

order_matters_meck() ->
    meck:new(foo),
    meck:expect(foo,bar,fun () -> ok end),
    meck:expect(foo,baz,fun () -> nok end),
    
    % The test
    foo:bar(),
    foo:baz(),
    
    % Checking the callouts
    ?assertTrue(meck:validate(foo)),
    meck:unload(foo).
				 
