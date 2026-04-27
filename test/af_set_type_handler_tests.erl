-module(af_set_type_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

%% Verify `set-type-handler` lets a4 source attach a handler word
%% (atom-named, dispatched per Ext-1) to a registered type without
%% touching Erlang.

setup() ->
    af_repl:init_types(),
    af_type_compiler:clear_pending_checks().

%% Define a product type, register an a4 handler word that drops the
%% incoming token, attach it, then run a sequence of tokens against
%% the type. Confirm the handler caught every non-dispatched token.
attach_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        Src = "type Box  count Int .  "
              %% Constructor `box` takes one Int and produces a Box.
              %% Handler word: takes the captured String off TOS,
              %% extracts the count from the Box below, increments it,
              %% writes it back. Box stays on TOS so subsequent tokens
              %% re-trigger this same handler.
              ": box-handler Box String -> Box ; "
              "  drop "                                      %% drop captured token
              "  count 1 + count! . "                        %% bump count
              "\"Box\" to-atom \"box-handler\" to-atom set-type-handler  "
              %% Build a Box(0), push it as TOS, fire 3 captured tokens.
              "0 box  foo bar baz",
        Tokens = af_parser:parse(Src, "test"),
        Cont = af_interpreter:interpret_tokens(
            Tokens, af_interpreter:new_continuation()),
        Stack = Cont#continuation.data_stack,
        %% Stack from TOS down: a single Box whose count was bumped
        %% three times.
        ?assertMatch([{'Box', 3}], Stack)
     end}.

unknown_type_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        Src = "\"NoSuchType\" to-atom \"anything\" to-atom set-type-handler",
        Tokens = af_parser:parse(Src, "test"),
        ?assertError(_,
            af_interpreter:interpret_tokens(
                Tokens, af_interpreter:new_continuation()))
     end}.
