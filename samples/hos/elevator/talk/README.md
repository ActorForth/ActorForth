# Talk deck

Markdown slide deck for the HOS elevator demo, rendered by
[presenterm](https://github.com/mfontanini/presenterm).

## Files

- `talk.md` - the deck. ~25 slides. Edit this to revise prose.
- `run.sh` - launches presenterm from the repo root with snippet
  execution enabled (the `-X` flag).
- `README.md` - this file.

## Running

```bash
samples/hos/elevator/talk/run.sh
```

Navigation:

- `space`, `n`, `right-arrow`, `enter` - next
- `p`, `left-arrow`, `backspace` - previous
- `q` - quit
- `o` - open the slide overview (jump to any slide by number)
- `?` - show all key bindings

## Live snippet execution

Several slides include `bash +exec_replace` snippets that run live
when you reach the slide. These are the audience-visible "press a
key, watch the checker reject this" moments.

- The `-X` flag in `run.sh` enables `+exec_replace` blocks. Without
  it presenterm refuses to execute snippets (a safety default).
- Each snippet runs the actual a4 checker / FAT suite from the repo,
  not a recorded transcript. The output you see is live.

## Theme

Default is the `light` built-in theme. Override with:

```bash
samples/hos/elevator/talk/run.sh --theme catppuccin-latte
samples/hos/elevator/talk/run.sh --theme dark
samples/hos/elevator/talk/run.sh --theme tokyonight-day
```

`presenterm --list-themes` shows the full list (must be run from
a real terminal).

## Editing

The deck is plain markdown. Edit prose in any text editor. Slide
breaks are `<!-- end_slide -->`. In-slide pause points (for
incremental reveals) are `<!-- pause -->`. Speaker notes are
`<!-- speaker_note: text -->`.

The Door teaching slides (slides 3-12) build the Door system one
line at a time. To revise that flow, edit each slide in turn so
the code blocks remain monotonically growing.

The defect slides (17-22) reference the bad-spec files in
`../comparison/exploits/` and run the a4 checker against them
live. If you change a bad spec the slide updates automatically.

## Hot reload

presenterm watches the source file. Edit `talk.md` while the
deck is running; the slide redraws on save. Useful for rehearsal.

## Speaker notes

The deck includes `<!-- speaker_note: ... -->` markers at the
slides where pacing or framing matters. To see them rendered:

```bash
samples/hos/elevator/talk/run.sh --listen-speaker-notes
```

(Or use `--publish-speaker-notes` to broadcast to a separate
listener.)
