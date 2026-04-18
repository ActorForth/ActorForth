# ActorForth for VS Code

Syntax highlighting and LSP client for `.a4` and `.test.a4` files.

## Features

- Syntax highlighting: comments, strings, numbers, word definitions, test-DSL vocabulary, stdlib types.
- LSP client wired to `a4c-lsp`: hover, completion, go-to-definition, diagnostics — all served by the same Erlang LSP server used by the Sublime extension.
- File associations: `.a4` and `.test.a4`.

## Install (development)

    cd editor/vscode
    npm install
    npm run compile
    # Then: VS Code → "Extensions: Install from VSIX..." after `vsce package`,
    # or press F5 in VS Code with this folder open to launch the Extension
    # Development Host.

## Config

- `actorforth.lsp.path` — path to the ActorForth LSP executable (default:
  `a4c-lsp`). Build that from the current tree with:

      rebar3 escriptize
      cp _build/default/bin/a4c-lsp ~/.local/bin/

Status: skeleton. Hover / completion / go-to-definition work once the
LSP server is on PATH. Future: richer semantic highlighting driven by
the LSP's stack-inference snapshots.
