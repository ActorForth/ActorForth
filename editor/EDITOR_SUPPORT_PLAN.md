# Editor Support Plan for ActorForth

## Current Implementation

### LSP Server (`src/af_lsp.erl`)

The LSP server is written in Erlang with A4-written support modules. It speaks
the Language Server Protocol over stdio using JSON-RPC 2.0 with Content-Length
framing. Dependencies: `jsx` (JSON), `af_ring2` (self-hosted compiler), `af_type`
(type registry).

**Transport & Protocol:**
- stdio JSON-RPC 2.0 with Content-Length header framing
- `main/1` escript entry point, `start/0` for rebar3 shell
- Main loop: `read_message → handle_request → write_message`
- Content-Length header parsing with case-insensitive matching
- Graceful handling of EOF, read errors, JSON decode errors
- `<<"exit">>` method triggers `halt(0)`

**Lifecycle Methods:**
- `initialize` — returns server capabilities, stores `rootUri`, sets `initialized`
- `initialized` — acknowledged (noreply)
- `shutdown` — returns `null` (clean shutdown)
- `exit` — terminates the process

**Document Sync (full sync, mode 1):**
- `textDocument/didOpen` — tokenizes document, infers stacks, stores in state, publishes diagnostics
- `textDocument/didChange` — re-analyzes full document text, updates state, publishes diagnostics
- `textDocument/didClose` — removes document from state
- `textDocument/didSave` — acknowledged (noreply)
- Documents stored as maps: `#{uri, version, text, tokens, stack_snaps, diagnostics}`

**Hover (`textDocument/hover`):**
- Finds the nearest stack snapshot at the cursor position
- Returns Markdown content with:
  - Bold token name
  - Stack state as inline code: `( Type1 Type2 -- TOS )`
  - Stack picture as code block (TOS marked with arrow)
- Position matching: LSP 0-indexed → AF 1-indexed conversion
- Falls back to nearest preceding snap if no exact match on current line

**Completion (`textDocument/completion`):**
- Type-aware: determines TOS type from stack snapshot at cursor
- Retrieves ops from TOS type's dictionary via `af_type:get_type/1`
- Also retrieves ops from `Any` dictionary (global ops)
- Each item includes: label (op name), detail (sig_in → sig_out), kind (Function=3)
- Returns `isIncomplete: false` with full item list

**Go-to-Definition (`textDocument/definition`):**
- Finds token at cursor position (line + column range matching)
- Searches token list for word definition site (token after `:`)
- Returns location with URI and range pointing to the definition name token
- Returns `null` if word not defined in current document

**Stack Inference (`infer_stacks/1`):**
- Walks token list sequentially, simulating stack effects
- Produces one snapshot per token: `#{idx, line, col, token, stack}`
- Stack is a list of type name strings (e.g., `["Int", "Bool", "List"]`)
- Known effects for 50+ operations:
  - Stack ops: `dup`, `drop`, `swap`, `rot`, `over`, `2dup`
  - Arithmetic: `+`, `-`, `*`, `/`, `mod` (2 → Int)
  - Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=` (2 → Bool)
  - Logic: `not` (1 → Bool), `and`, `or` (2 → Bool)
  - List: `nil` (→ List), `cons` (2 → List), `head` (1 → Any), `tail` (1 → List), `length`, `empty?`, `append`, `reverse`
  - Map: `map-new` (→ Map), `map-put` (3 → Map), `map-get` (2 → Any), `map-delete`, `map-has?`, `map-keys`, `map-values`, `map-size`
  - String: `concat` (2 → String), `to-string`, `to-int`, `to-float`
  - I/O: `print` (1 →), `stack` (noop), `assert` (1 →), `assert-eq` (2 →)
  - Definition syntax: `:` pushes def_marker, `.` pops to marker, `;` and `->` pass through
- Literal detection: integers, floats (via parse), quoted strings (via token flag)
- Word lookup: checks `Any` dictionary, then all registered types for known words
- Stack underflow marked with `"?"` type
- Unknown tokens pushed as `"Atom"`

**Diagnostics:**
- Framework in place: `compute_diagnostics/2` called on every document change
- `publish_diagnostics/2` sends `textDocument/publishDiagnostics` notifications
- Diagnostic format: line, col, end_col, severity, message, source="actorforth"
- Currently returns empty list (detection logic TODO)

**Advertised Capabilities:**
- `textDocumentSync: 1` (full document sync)
- `hoverProvider: true`
- `completionProvider` with trigger character `" "`
- `definitionProvider: true`

### A4-Written LSP Modules (`src/bootstrap/lsp/`)

Three A4 source files compiled via the self-hosted pipeline at server startup:

**`lsp_types.a4`** — Product type definitions:
- `DocState` — uri, version, text, tokens, stack-snaps, diagnostics
- `StackSnap` — token-idx, line, col, stack
- `StackEffect` — name, pops, pushes
- `LspDiag` — line, col, end-col, severity, message
- `CompItem` — label, detail, kind

**`lsp_stack_infer.a4`** — Stack inference engine (A4 implementation):
- `prim-effect` — 50+ pattern-matched clauses mapping op names to `[PopCount, PushType...]` descriptors
- `apply-effect` — chain of type checks dispatching to specialized handlers
- Special stack op handlers: `apply-dup`, `apply-drop-effect`, `apply-swap-effect`, `apply-rot-effect`, `apply-over-effect`, `apply-2dup-effect`
- `apply-numeric-effect` — generic pop-N/push-types for arithmetic etc.
- `pop-n` / `push-types` — list manipulation helpers
- `is-int-token?` / `is-all-digits` — integer literal detection via byte values
- `make-snap` — creates snapshot map from idx/line/col/token/stack
- `infer-token` — per-token inference (stub, Erlang side currently used)

**`lsp_server.a4`** — Protocol dispatch:
- `lsp-response` — builds JSON-RPC success response map
- `lsp-error` — builds JSON-RPC error response map
- `server-capabilities` — returns capability declaration map
- `dispatch-method` — 10-clause pattern match mapping LSP method strings to internal names
- `format-stack` / `format-stack-items` / `format-stack-join` — stack picture as string
- `format-stack-item` / `format-stack-other` / `format-stack-any` — per-type formatting

### Sublime Text Support (`editor/sublime/`)

**`ActorForth.sublime-syntax`** — TextMate-compatible syntax definition:
- Scope: `source.actorforth`, file extension: `.a4`
- First-line match: `^#.*ActorForth|^:.*->`
- Highlighting contexts:
  - Comments: `# ...` → `comment.line`
  - Strings: `"..."` with escape sequences → `string.quoted.double`
  - Word definitions: `: name` → `keyword.control.definition` + `entity.name.function`
  - Type definitions: `type Name` → `keyword.declaration.type` + `entity.name.type`
  - Arrow `->` → `keyword.operator.arrow`
  - Semicolon `;` → `keyword.control.semicolon`
  - Dot `.` → `keyword.control.dot`
  - Built-in types: Int, Bool, String, Float, List, Map, Tuple, Atom, Any, Actor, Message → `storage.type`
  - Boolean literals: true, false → `constant.language.boolean`
  - Nil → `constant.language.nil`
  - Integer/float literals (including negative) → `constant.numeric`
  - Stack ops: dup, drop, swap, rot, over, 2dup → `support.function.stack`
  - Arithmetic: +, -, *, / → `keyword.operator.arithmetic`
  - Comparison: ==, !=, <, >, <=, >= → `keyword.operator.comparison`
  - Logic: not, and, or → `keyword.operator.logical`
  - I/O: print, stack, debug, assert, assert-eq → `support.function.io`
  - Actor words: server, supervised-server, spawn, send, receive → `keyword.control.actor`
  - Send protocol: <<, >> → `keyword.operator.send`
  - List ops: cons, head, tail, append, reverse, nth, etc. → `support.function.list`
  - Map ops: map-new, map-put, map-get, etc. → `support.function.map`
  - String ops: concat, split, contains, etc. → `support.function.string`
  - FFI: erlang-apply, erlang-call, etc. → `support.function.ffi`
  - File ops: load, read-file, write-file, file-exists? → `support.function.file`
  - Tuple ops: make-tuple, from-tuple, tuple-size, etc. → `support.function.tuple`
  - Product setters: `field!` pattern → `entity.name.function.setter`

**`LSP-ActorForth.sublime-settings`** — LSP client configuration:
- Client name: `actorforth`
- Command: `rebar3 shell --eval "af_lsp:start()."`
- Selector: `source.actorforth`
- Language ID: `actorforth`

### Test Coverage (`test/af_lsp_tests.erl`)

13 tests covering:
- Stack inference for primitives and literals
- Hover computation with markdown output
- Completion with type-aware op suggestions
- Go-to-definition for word names
- JSON-RPC request/response handling
- Protocol lifecycle (initialize, shutdown, didOpen, didChange, didClose)

---

## Phase 1: LSP Server Improvements

### Diagnostics (publish on save/change)
- Stack underflow detection (consuming more than available)
- Type mismatch warnings (op expects Int, got String on TOS)
- Undefined word warnings (token not in any dictionary, not a literal)
- Missing word terminator (`:` without matching `.`)
- Signature arity mismatch (declared sig_in/sig_out vs inferred)

### Incremental Analysis
- Cache parsed/inferred results per document URI
- Re-analyze only changed regions (line-level granularity)
- Background analysis on didChange, report on didSave

### Multi-file Support
- Track `load` dependencies between .a4 files
- Cross-file go-to-definition
- Project-wide word index for completion
- Workspace symbol search

### Signature Help
- Show `sig_in -> sig_out` when cursor is inside a word call
- Trigger on space after word name

### Document Symbols
- List all word definitions (`:` blocks) and type definitions (`type` blocks)
- Outline view in editor sidebar

### Code Actions
- "Extract word" — select body tokens, refactor into new word definition
- "Add type signature" — infer and insert sig_in/sig_out for untyped word

---

## Phase 2: Editor Plugins

### VS Code Extension
- `editor/vscode/actorforth/`
- `package.json` — extension manifest, language configuration
- `syntaxes/actorforth.tmLanguage.json` — TextMate grammar (convert from sublime-syntax)
- `language-configuration.json` — brackets, comments, auto-closing pairs
- LSP client config pointing to `rebar3 shell --eval "af_lsp:start()."`
- Marketplace publish target: `actorforth.actorforth`

### Neovim
- `editor/nvim/actorforth.lua` — ftdetect, syntax, LSP client config
- Tree-sitter grammar: `editor/nvim/tree-sitter-actorforth/grammar.js`
- LSP config for nvim-lspconfig or manual setup
- Treesitter highlights, indentation, folds

### Emacs
- `editor/emacs/actorforth-mode.el` — major mode
- Syntax highlighting via font-lock
- LSP integration via lsp-mode or eglot
- REPL integration: comint-mode wrapper for `rebar3 shell` + `af_repl:start()`

### Zed
- `editor/zed/actorforth/` — extension directory
- `extension.toml` — manifest
- Tree-sitter grammar (share with Neovim)
- LSP client config

---

## Phase 3: Developer Experience

### REPL Integration
- Editor command to send selected text to af_repl
- Inline evaluation: show stack result next to code
- REPL panel/terminal integration for each editor

### Debugger (DAP — Debug Adapter Protocol)
- Step through word execution (token by token)
- Stack inspection at each step
- Breakpoints on word entry
- Watch expressions (stack depth, TOS value)
- Requires DAP server in Erlang (new module, similar to af_lsp)

### Snippets
- Word definition template: `: name Types -> Types ; body .`
- Product type template: `type Name\n  field Type\n.`
- Actor server template: `type State ... . : init -> State ; ... . server`
- Sub-clause template: `: Pattern -> Types ; body`

### Formatting
- Auto-indent word bodies (2 spaces)
- Align `->` in signatures
- One field per line in type definitions
- Normalize whitespace around `:` `;` `.`

---

## Phase 4: Tree-sitter Grammar

A proper tree-sitter grammar enables fast, incremental parsing for
syntax highlighting, code folding, and structural navigation across
all editors that support it (VS Code, Neovim, Zed, Emacs, Helix).

### Grammar structure
```
program → (definition | type_definition | comment | token)*
definition → ':' name signature ';' body '.'
signature → type* '->' type*
type → UPPER_WORD
body → (token | sub_clause)*
sub_clause → ':' pattern '->' type* ';' body
token → WORD | NUMBER | STRING | BOOL
```

### Location
- `editor/tree-sitter-actorforth/grammar.js`
- Shared across VS Code, Neovim, Zed, Emacs

---

## Priority Order
1. LSP diagnostics (highest user value)
2. VS Code extension (largest editor market share)
3. Document symbols + signature help
4. Neovim tree-sitter + LSP
5. Incremental analysis + multi-file
6. REPL integration
7. Emacs, Zed
8. DAP debugger
9. Formatter
