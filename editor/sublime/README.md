# Sublime Text setup for ActorForth

Gives you:
- Syntax highlighting for `.a4` files
- LSP integration: hover shows the stack picture, completions, go-to-definition,
  stack-underflow / type-mismatch diagnostics

## Prerequisites

- **Sublime Text 4**
- **[LSP](https://packagecontrol.io/packages/LSP) plugin** from Package Control
- **Erlang/OTP** with `rebar3` on your PATH (for the LSP server)
- This repo cloned, and `rebar3 compile` run once from its root

## Install

1. **Copy the syntax file** into Sublime's user packages folder:

   ```bash
   cp ActorForth.sublime-syntax "$HOME/.config/sublime-text/Packages/User/"   # Linux
   # macOS: "$HOME/Library/Application Support/Sublime Text/Packages/User/"
   ```

   Sublime will pick it up immediately and apply it to any `.a4` file you open.

2. **Copy the LSP client config** into the same folder:

   ```bash
   cp LSP-ActorForth.sublime-settings "$HOME/.config/sublime-text/Packages/User/"
   ```

   Then edit the `"command"` line in that file so the absolute path points to
   `bin/af_lsp` in your local checkout of this repo. Example:

   ```json
   "command": ["/Users/you/src/ActorForth/bin/af_lsp"]
   ```

3. **Sanity check the server**:

   ```bash
   cd /path/to/ActorForth
   rebar3 compile
   ./bin/af_lsp < /dev/null    # should print nothing, exit cleanly (Ctrl-C after a moment)
   ```

4. **Restart Sublime** (or the LSP plugin via `LSP: Restart Servers`) and open any
   `.a4` file.

## Verify it works

Open a `.a4` file (try `samples/fib.a4` or `test/demos/debounce/debounce_demo.a4`).

- Hover on any token: should show a stack picture (`( Int Int -- TOS )`).
- Put your cursor inside a word body and type a letter: should suggest
  matching primitives from the current TOS type's dictionary + Any.
- Go-to-definition (default `F12`) on a user-defined word should jump to its
  `: name ...` line.
- Write `dup +` without anything below it: should underline `+` and report
  "stack underflow: '+' requires 2 item(s), stack has 0".

## Troubleshooting

- **"LSP server not reachable"** — the command path in the settings file is
  wrong, or you haven't `rebar3 compile`'d. Run the command directly in a
  terminal to see the error.
- **Hover shows nothing** — the file might be empty or contain a parser
  error. Check the LSP log (`LSP: Toggle Log Panel`).
- **Diagnostics don't update** — the LSP re-analyses on every change; if
  you see staleness, restart the server via `LSP: Restart Servers`.
