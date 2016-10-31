-module(test).
-author(josef).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hoax/include/hoax.hrl").

%%
%% meck
%%

% Order does not matter for meck
order_matters_meck_test_() ->
    fun () ->
	    io:format("Running order_matters_meck test"),
	    meck:new(foo,[non_strict]),
	    meck:expect(foo,bar,fun () -> ok end),
	    meck:expect(foo,baz,fun () -> ok end),
	    
            % The test
	    foo:baz(),
	    foo:bar(),
	    
            % Checking the callouts
	    B = meck:validate(foo),
	    meck:unload(foo),
	    ?assert(B)
    end.

% We may call an expected function many times
one_or_many_meck_test_() ->
    fun () ->
	    meck:new(foo,[non_strict]),
	    meck:expect(foo,bar,fun () -> ok end),

	    foo:bar(),
	    foo:bar(),
	    
	    B = meck:validate(foo),
	    meck:unload(foo),
	    ?assert(B)
    end.

% meck simply picks the second "expect".
same_twice_meck_test_() ->
    fun () ->
	    meck:new(foo,[non_strict]),
	    meck:expect(foo,bar,fun () -> 1 end),
	    meck:expect(foo,bar,fun () -> 2 end),
	    
	    N = foo:bar(),
	    
	    B = meck:validate(foo),
	    meck:unload(foo),
	    ?assert(B),
	    ?assertEqual(N,2)
    end.
					 
% Apparently there is no need to actually call the expected function
call_needed_meck_test_() ->
    fun () ->
	    meck:new(foo,[non_strict]),
	    meck:expect(foo,bar,fun () -> ok end),
	    
	    % No call to foo:bar()

	    B = meck:validate(foo),
	    meck:unload(foo),
	    ?assert(B)
    end.
					 

%%
%% hoax
%%

% Order doesn't matter between two calls
order_matters_hoax_test_() ->
    fun () ->
	    hoax:test(fun () ->
			      hoax:mock(foo,[
					     ?expect(bar,?withArgs([]), ?andReturn(ok)),
					     ?expect(baz,?withArgs([]), ?andReturn(ok))
					    ]),

			      % The test
			      foo:baz(),
			      foo:bar()
		      end)
    end.


% We may call an expected function many times
one_or_many_hoax_test_() ->
    fun () ->
	    hoax:test(fun () ->
			      hoax:mock(foo,[
					     ?expect(bar,?withArgs([]),?andReturn(ok))
					    ]),

			      foo:bar(),
			      foo:bar()
		      end)
    end.

% Hoax doesn't allow expecting the same function many times
%% same_twice_hoax_test_() ->
%%     fun () ->
%% 	    hoax:test(fun () ->
%% 			      hoax:mock(foo,[?expect(bar,?withArgs([]),?andReturn(1)),
%% 					     ?expect(bar,?withArgs([]),?andReturn(1))]),
	    
%% 			      N = foo:bar(),
	    
%% 			      ?assertEqual(N,2)
%% 		      end)
%%     end.

% Apparently there is no need to actually call the expected function
call_needed_hoax_test_() ->
    fun () ->
	    hoax:test(fun () ->
			      hoax:mock(foo,[?expect(bar,?withArgs([]),?andReturn(ok))])
		      end)
    end.

%%
%%
%%

