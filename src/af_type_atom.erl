-module(af_type_atom).

-export([init/0]).

init() ->
    %% Atom type has no operations of its own.
    %% It exists so TOS-type dispatch can distinguish atoms from other types.
    %% Atoms are created by the interpreter's make_atom fallback.
    ok.
