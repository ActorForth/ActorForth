-module(af_type_atom_tests).

-include_lib("eunit/include/eunit.hrl").

init_test() ->
    ?assertEqual(ok, af_type_atom:init()).
