# obsolete/

Legacy Erlang implementations kept in-tree for reference and as
fallback FFI targets for the a4-native versions.

These modules are compiled as part of the actorforth application
(see `rebar.config` `src_dirs`) so their exports remain callable.
Nothing in the active codepath treats them as first-class — new
work happens in the a4 source under `src/bootstrap/hos/`.

## Contents

- `af_hos_check.erl` — axiom checker. The a4 replacement lives at
  `src/bootstrap/hos/check.a4`; this module remains the FFI target
  for the subset of axioms the a4 checker bridges out, plus the
  system registry (ETS-backed) consumed by
  `af_hos_check:register_system/1`.

- `af_hos_dsl.erl` — `system ... end` DSL token interceptor. The
  token-handler hook still lives here because a4 types don't
  accept a4-word handlers yet. The parser has an a4 surface at
  `src/bootstrap/hos/dsl.a4` that delegates to
  `af_hos_dsl:parse_system_tokens_a4/1` via erlang-apply.

- `af_hos_runtime.erl` — thin Erlang-side public API +
  `a4_exec_stateless_body/2` FFI helper for the HOS runtime. The
  bulk of the runtime moved to `src/bootstrap/hos/runtime.a4`.

## When to delete

Any or all of these files can be removed once:

1. The active codepath no longer calls into their exports, AND
2. The equivalent a4 implementation covers every behaviour the
   Erlang version provided.

Until then: move-but-keep.
