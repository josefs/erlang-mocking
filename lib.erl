-module(lib).
-author(josef).

% Sequential composition of stubs
seq(StubList) when is_list(StubList)->
  {seq_list, StubList}.
seq(S1,S2) ->
  {seq, S1, S2}.

% Parallel composition of stubs
par(ParList) when is_list(ParList) ->
  {par_list, ParList}.
par(S1,S2) ->
  {par, S1, S2}.

% Permutations of stubts
perm(PermList) when is_list(PermList) ->
  {perm_list, PermList}.

% Stub a function call
call(MFA={M,F,A}, Result) ->
  {call, MFA, Result}.

% No stub at all
empty() ->
  {empty}.

% The software under test can behave in different ways.
choice(S1, S2) ->
  {choice, S1, S2}.
choice(ChoiceList) when is_list(ChoiceList) ->
  {choice_list, Choice_list}.

% A stub mayb be repeat an indefinite number of times.
star(S) ->
  {star, S}.

%% Operational semantics

step(MFA,{call, MFA, Result}) ->
  {Result, {empty}}.

