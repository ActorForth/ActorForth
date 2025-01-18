-module(repl_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl"). 
-include("continuation.hrl").

-define(TOKEN(Value), repl:make_token(Value)).

interpret_tokens_test_() ->
    [
        {"Test interpreting tokens for '4 int 13 int + .'",
         fun() ->
             Tokens = [
                 ?TOKEN("4"), ?TOKEN("int"), ?TOKEN("13"), ?TOKEN("int"), ?TOKEN("+"), ?TOKEN(".")
             ],
             % Change group_leader to capture output
             OldGroupLeader = group_leader(),
             {ok, IoDevice} = file:open("/tmp/test_output", [write]),
             group_leader(IoDevice, self()),
             
             try
                 Result = repl:interpret_tokens(Tokens),
                 % Read the file to get the output
                 {ok, Output} = file:read_file("/tmp/test_output"),
                 ?assertEqual(<<"4 int 13 int + . ok.\n">>, Output),
                 %% Assert that the data stack contains the expected result after operations
                 ?assertEqual([{atom, "."}, {atom, "+"}, {atom, "int"}, {atom, "13"}, {atom, "int"}, {atom, "4"}], 
                              Result#continuation.data_stack)
             after
                 group_leader(OldGroupLeader, self()),
                 file:close(IoDevice),
                 file:delete("/tmp/test_output")
             end
         end},
        
        {"Test interpreting tokens for '5 int 8 int + .'",
         fun() ->
             Tokens = [
                 ?TOKEN("5"), ?TOKEN("int"), ?TOKEN("8"), ?TOKEN("int"), ?TOKEN("+"), ?TOKEN(".")
             ],
             OldGroupLeader = group_leader(),
             {ok, IoDevice} = file:open("/tmp/test_output", [write]),
             group_leader(IoDevice, self()),
             
             try
                 Result = repl:interpret_tokens(Tokens),
                 {ok, Output} = file:read_file("/tmp/test_output"),
                 ?assertEqual(<<"5 int 8 int + . ok.\n">>, Output),
                 ?assertEqual([{atom, "."}, {atom, "+"}, {atom, "int"}, {atom, "8"}, {atom, "int"}, {atom, "5"}], 
                              Result#continuation.data_stack)
             after
                 group_leader(OldGroupLeader, self()),
                 file:close(IoDevice),
                 file:delete("/tmp/test_output")
             end
         end}
    ].

make_token_test_() ->
    [
        {"Test make_token/1 with default values",
         fun() ->
             Token = repl:make_token("test"),
             ?assertEqual(#token{value = "test", column = 0, line = 0, file = "(unknown)"}, Token)
         end},
        
        {"Test make_token/4 with specific values",
         fun() ->
             Token = repl:make_token("test", 1, 2, "file.erl"),
             ?assertEqual(#token{value = "test", column = 2, line = 1, file = "file.erl"}, Token)
         end}
    ].