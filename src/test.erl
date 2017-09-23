-module(test).
-author(josef).

-include_lib("eunit/include/eunit.hrl").

order_matters_meck() ->
    meck:new(foo),
    meck:expect(foo,bar,fun () -> ok end),
    meck:expect(foo,baz,fun () -> nok end),
    
    % The test
    foo:bar(),
    foo:baz(),
    
    % Checking the callouts
    ?assert(meck:validate(foo)),
    meck:unload(foo).
				 
