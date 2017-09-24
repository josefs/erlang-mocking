-module(lib).
-author(josef).

-export([seq/1, seq/2
	,par/1, par/2
	,choice/1, choice/2
	,perm/1, star/1, call/2, empty/0
	 % Running stubs
	,stub/1, check_stub/0
	% Functions used under the hood
	,undefined_call/3, stub_proc/1]).

-record(seq, { accepting :: boolean()
	     , left      :: stub()
	     , right     :: stub()
	     }).
-record(seq_list, { accepting :: boolean()
		  , stub_list :: list(stub())
		  }).

-record(par, { accepting :: boolean()
	     , left      :: stub()
	     , right     :: stub()
	     }).
-record(par_list, { accepting :: boolean()
		  , stub_list :: list(stub())
		  }).

-record(choice, { accepting :: boolean()
		, left      :: stub()
		, right     :: stub()
		}).
-record(choice_list, { accepting   :: boolean()
		     , stub_list :: list(stub())
		     }).

-record(perm_list, { accepting :: boolean()
		   , stub_list :: list(stub())
		   }).

-record(star, { accepting = true :: boolean()
	      , body             :: stub()
	      , copy             :: stub()
	      , progress         :: boolean()
	      }).

-record(call, { accepting = false :: boolean()
	      , mfa               :: mfa()
	      , result            :: term()
	      }).

-record(empty, { accepting = true :: boolean() }).

-type stub() :: #seq{} | #seq_list{}
	      | #par{} | #par_list{}
	      | #choice{} | #choice_list{}
	      | #perm_list{}
	      | #star{}
	      | #call{}
	      | #empty{}.

% Sequential composition of stubs
seq(StubList) when is_list(StubList)->
  #seq_list{ accepting = lists:all(fun accepting/1, StubList)
	   , stub_list = StubList
	   }.
seq(S1,S2) ->
  #seq{ accepting = accepting(S1) andalso accepting(S2)
      , left      = S1
      , right     = S2
      }.

% Parallel composition of stubs
par(ParList) when is_list(ParList) ->
  #par_list{ accepting = lists:all(fun accepting/1,ParList)
	   , stub_list = ParList
	   }.
par(S1,S2) ->
  #par{ accepting = accepting(S1) andalso accepting(S2)
      , left      = S1
      , right     = S2
      }.

% The software under test can behave in different ways.
choice(S1, S2) ->
  #choice{ accepting = accepting(S1) orelse accepting(S2)
	 , left      = S1
	 , right     = S2
	 }.
choice(ChoiceList) when is_list(ChoiceList) ->
  #choice_list{ accepting = lists:any(fun accepting/1,ChoiceList)
	      , stub_list = ChoiceList
	      }.

% Permutations of stubs
perm(PermList) when is_list(PermList) ->
  #perm_list{ accepting = lists:all(fun accepting/1,PermList)
	    , stub_list = PermList
	    }.

% A stub mayb be repeat an indefinite number of times.
% Invariant during stepping: body /= copy <=> progress
star(S) ->
  #star{ accepting = true
       , body      = S
       , copy      = S
       , progress  = false
       }.

% Stub a function call
call(MFA={_,_,_}, Result) ->
  #call{ accepting = false
       , mfa       = MFA
       , result    = Result
       }.

% No stub at all
empty() ->
  #empty{ accepting = false }.

%% Accepting predicate
%% Returns true if the stub can accept the empty call sequence

accepting(#seq        { accepting = A }) -> A;
accepting(#seq_list   { accepting = A }) -> A;
accepting(#par        { accepting = A }) -> A;
accepting(#par_list   { accepting = A }) -> A;
accepting(#choice     { accepting = A }) -> A;
accepting(#choice_list{ accepting = A }) -> A;
accepting(#perm_list  { accepting = A }) -> A;
accepting(#star       { accepting = A }) -> A;
accepting(#call       { accepting = A }) -> A;
accepting(#empty      { accepting = A }) -> A.

%% Operational semantics

-spec step(mfa(), stub()) -> {ok  , term(), stub()}
			   | {fail, term()}.
step(MFA, #call{ mfa = MFA, result = Result}) ->
    {ok, Result, empty()};
step(_MFA, CALL = #call{}) ->
    {fail, {expected_call, CALL}};
step(MFA, #seq{ left = S1, right = S2}) ->
    case step(MFA, S1) of
	{ok, Result, S} ->
	    {ok, Result, seq(S,S2)};
	Fail = {fail, Reason1} ->
	    case accepting(S1) of
		true ->
		    case step(MFA, S2) of
			Res = {ok, _, _} ->
			    Res;
			{fail, _Reason2} ->
			    {fail, Reason1} %% FIXME: Merge Reason1 and Reason2
		    end;
		false ->
		    Fail
	    end
    end;
step(MFA, #seq_list{ stub_list = SeqList }) ->
    step_seq_list(MFA, SeqList);
step(MFA, #par{ left = S1, right = S2 }) ->
    case step(MFA, S1) of
	{ok, Result, S} ->
	    {ok, Result, par(S, S2)};
	{fail, Reason1} ->
	    case step(MFA, S2) of
		{ok, Result, S} ->
		    {ok, Result, par(S1, S)};
		{fail, _Reason2} ->
		    {fail, Reason1} %% FIXME: Figure out a better failure message
	    end
    end;
step(MFA, #choice{ left = S1, right = S2 }) ->
    case step(MFA, S1) of
	Result = {ok, _, _} ->
	    Result;
	{fail, Reason1} ->
	    case step(MFA, S2) of
		Result = {ok, _, _} ->
		    Result;
		{fail, _Reason2} ->
		    {fail, Reason1} %% FIXME: Better failure message
	    end
    end;
step(MFA, Star = #star{ body = Body, copy = Copy, progress = Prog }) ->
    case step(MFA, Body) of
	{ok, Result, S } ->
	    {ok, Result, Star#star{ body = S, progress = true }};
	Fail = {fail, _Reason} ->
	    case accepting(Body) andalso Prog of
		true ->
		    step(MFA, Star#star{ body = Copy, progress = false });
		false ->
		    Fail
	    end
    end;
step(MFA, #empty{}) ->
    {fail, {unexpected_call, MFA}}.
% FIXME: Complete the definition of step/2.

step_seq_list(MFA, []) ->
    {fail, {no_call_matching, MFA}};
step_seq_list(MFA, [S|SeqList]) ->
    case step(MFA, S) of
	{ok, Result, #empty{}} ->
	    {ok, Result, #seq_list{
			    accepting =
				lists:all(fun accepting/1, SeqList),
			    stub_list =
				SeqList
			   }};
	Fail = {fail, _Reason} ->
	    case accepting(S) of
		true ->
		    step_seq_list(MFA, SeqList);
		false ->
		    Fail
	    end
    end.

%% Install the stubs

-spec stub(stub()) -> ok.
stub(Stub) ->
    % Create a new process which maintains the state of the stub
    Pid = spawn(?MODULE, stub_proc, Stub),

    % Register a name for the process so that we don't have to
    % pass around the Pid.
    register(stub_proc, Pid),

    % Install new call handler
    OLD = process_flag(error_handler, ?MODULE),
    put(old_error_handler, OLD).

-spec check_stub() -> boolean().
% Must be called in the same process that called stub/1
check_stub() ->
    stub_proc ! {finalize, self()},
    receive
	Result ->
	    unregister(stub_proc),
	    OLD = get(old_error_handler),
	    process_flag(error_handler, OLD),
	    erase(old_error_handler),
	    Result
    end.

stub_proc(Stub) ->
    receive
	{call, MFA, Pid} ->
	    case step(MFA, Stub) of
		{ok, Result, S} ->
		    Pid ! {ok, Result},
		    stub_proc(S);
		Fail = {fail, Reason} ->
		    Pid ! Fail,
		    stub_failed(Reason)
	    end;
	{finalize, Pid} ->
	    Pid ! accepting(Stub)
    end.

stub_failed(Reason) ->
    receive
	{call, _, Pid} ->
	    Pid ! {fail, Reason};
	{finalize, Pid} ->
	    Pid ! false
    end.

undefined_call(Module, Func, Args) ->
    % We currently don't stub loaded modules
    case code:if_loaded(Module) of
	true ->
	    error_handler:undefined_call(Module, Func, Args);
	false ->
	    stub_proc ! {call, {Module, Func, Args}, self()},
	    receive
		{ok, Result} ->
		    Result;
		{fail, Reason} ->
		    throw(Reason)
	    end
    end.
