% Mocking
% Josef Svenningsson
%

# Existing Mocking Frameworks

When I write like this what does it mean:

~~~{.erlang}
> meck:expect(foo,bar,fun () -> ok  end).
> meck:expect(foo,baz,fun () -> nok end).
~~~

It could sensibly mean one of two things:

* Expect one call to `foo:bar/0` and one to `foo:baz/0` in any order
* First expect a call to `foo:bar/0` and secondly a call to `foo:baz/0`.

Does the order of the calls matter or not?

# What behaviour does mocking libraries out there use?

TODO: Check what the various different mocking framework does.

# Order matters - or not

* Imposing an order on the calls is good because the tests become stricter

  We can catch more bugs!

* Allowing for any order is useful:

  * It might not matter in what order the calls happen.

    If there is a change in the code which alters the order of the calls we
    don't need to update the test.

    If we test several different implementations they should be allowed to
    behave differently if the order of the call doesn't matter.

Clearly both behaviours are desired!

# Same, same - but different

What does it mean to expect the same callout twice?

~~~{.erlang}
> meck:expect(foo,bar,fun () -> ok  end).
> meck:expect(foo,bar,fun () -> nok end).
~~~

* Expect two calls to `foo:bar/0`, the first return `ok` and the second
  returning `nok`.

* Expect any number of calls to `foo:bar/0` and return either `ok` or
  `nok` depending on the mood of the mocking framework.

# Two forms of composition

If it is useful to both require a strict order among calls and to
ignore the order,

PROVIDE BOTH BEHAVIOURS!

* mock:seq/2
* mock:par/2.
