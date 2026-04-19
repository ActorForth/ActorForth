# Elevator reference implementations

Three pairs of naive-and-correct reference implementations of the
same two-elevator controller, to read alongside
`../elevator.a4`. The claim the a4 spec makes is that it is
smaller than each correct reference and that its failure modes
are either uncompilable or absent by runtime construction; these
files are the evidence you compare against.

| Language | Naive                 | Correct                 |
|----------|-----------------------|-------------------------|
| Python   | `elevator_naive.py`   | `elevator_correct.py`   |
| OCaml    | `elevator_naive.ml`   | `elevator_correct.ml`   |
| Rust     | `elevator_naive.rs`   | `elevator_correct.rs`   |

## Reading order

1. Skim `elevator_naive.py`. It has seven explicit `FLAW N`
   comments enumerating the six design problems the a4 spec is
   built to avoid.
2. Compare against `elevator_correct.py`. Roughly twice the lines
   to close the same gaps by hand.
3. Open `../elevator.a4` side by side with the correct Python.
   The a4 spec is 233 lines total (93 non-comment) for the same
   behaviour, with the correctness properties enforced at compile
   time rather than defended by hand.
4. Skim the OCaml and Rust pairs to see the same shape in
   different languages. Their naive headers note that OCaml's
   module system and Rust's borrow checker make several of the
   Python-style flaws harder to write; the comparison is not
   "Python programmers are worse" but "hand-written defences
   cost lines in every language."

## Running them

Three per-language scripts handle the build and demo:

- `./run_python.sh` -- stdlib only, always runs where `python3` is present.
- `./run_ocaml.sh`  -- needs `ocamlfind` + `threads.posix`. Skips with
                       an install hint if missing.
- `./run_rust.sh`   -- needs `rustc`. Compiles single-file binaries to
                       `./out/`. Skips with an install hint if missing.

Each script runs both the naive and correct versions sequentially,
compiling as needed. Each demo issues two floor requests, sleeps a
few seconds, and prints final car state.

Or run the full proof walk from `..`:

```
bash ../run_hos_tests.sh
```

That invokes the five a4 test modules (the actual correctness
proof) and then calls each per-language script for context.

## A caveat, in bold

**Running these demos does not prove the naive versions are
broken.** FLAWs 1-4 are TOCTOU / ordering races that do not
always manifest under a short demo run. The proof that the naive
versions are broken lives in their `FLAW` source comments (design
evidence) and in the a4 comparison tests under
`test/af_hos_comparison_tests.erl` (which show what the naive
*could have* done wrong). Running is for "it executes," not for
"it fails."

## Build output

Compiled binaries land in `./out/`. That directory is created on
demand and can be deleted at any time; the source files are the
artifacts.
