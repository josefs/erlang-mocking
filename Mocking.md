% Mocking
% Josef Svenningsson
%

# What is the role of a mocking library?

Depending on how you answer this question you will get a very different mocking library.

## Mocking is used to make functional tests work

This view leads to a very loose design

* Order of calls to stubs doesn't matter.
* The number of calls to a stubbed function doesn't matter.
* It may not matter whether a stubbed function gets called.

## Mocking is an integral part in testing the input/output behavior of a function

This is a much stronger view where it is important to specify exactly when and how stubs are called.

* Order of calls matter.
* The number of calls to a stubbed function matters and it is important that the arguments are correct in each call.
* It is important that stubbed functions are called.

A mocking libraray should enable specifying the call-out behavior of a function.

# Meaning of a call

What does it really mean when I write that I expect a call like this?

~~~{.erlang}
> meck:expect(foo,bar,fun ()  -> ok end).
~~~

Two common semantics:

* Expect exactly one call to `foo:bar/0`.
* Expect at least one calls to `foo:bar/0` but possibly many.

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

# Implementing a Mocking library

How should we catch a call to a mocked function, e.g. foo:bar/0?

* process_flag(Flag :: error_handler, Module) is probably the simplest way.
  * Doesn't work if the code is in the path
  * Doesn't deal with multiple processes
* Create a new module `foo` which recieves the function calls. It will have to replace any previously loaded module `foo` which might cause problems
  * Deals with multiple processes
  * How does code loading work?
* Create a completely new error handler module which replaces the existing default error handler in the vm.
  * Deals with multiple processes
  * How does code loading work?
