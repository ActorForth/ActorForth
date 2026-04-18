# Heritage & Philosophy — Resume Notes

**Status:** working notes, not published prose. Captures decisions and
source material from the 2026-04-18 design session so the doc-writing
work can resume later without re-running the conversation.

## Scope of the work

Two documents get new content:

1. **`docs/architectural-drivers-considerations.md`** — add two
   front-matter parts before current Part I:
   - **Part 0: On Language Design — Why Engineering, Not Math.**
     The philosophical framing: formal specs are brittle at scale;
     Dijkstra and Hamilton worked at the scale where proof pays
     (algorithms; lives-at-stake systems); real systems escape
     those methods due to unbounded non-determinism; Chuck Moore's
     engineering answer; the pivot to emergent behavior of simple
     loosely-coupled components.
   - **Part 0.5: Intellectual Heritage.** Annotated credits to the
     key figures (list below), each with how their work shows up in
     a4 concretely.
   - **Appendix A**: compact bibliography pointing back to Part 0.5.

2. **`docs/IntroToActorForth.md`** — add a new preface:
   - **Preface: Why ActorForth Exists.** Reader-friendly version of
     Part 0 above. Less philosophical density, more "here's the itch
     this language scratches and the giants it stands on." Points at
     the architectural doc for depth.
   - Scattered inline citations in existing chapters (footnote-style
     call-outs) when the stack / actors / tooling are introduced.

Not yet in scope (later sessions):

- Testing chapter (lands when the test DSL is built).
- LLM chapter (lands after testing).
- Talk-slide deck.

## The author's motivation — a4 as the DSL-building tool (2026-04-18 addition)

This is the single most important framing the user gave for *why* a4
exists. Preserve this as its own section — working title: **"What
ActorForth Is For."** Goes into the preface of the intro book and
stands as its own piece in the architectural doc. In user's voice:

> Language design has always been one of my most favorite intellectual
> pastimes. I chew on a problem and think of a way of expressing it in
> a language that I wish I had — without much regard to the challenges
> of making that language — and iterate through it until I come up with
> something that expresses the solution in a really elegant nice manner
> that is enjoyable to read. Formally you might think of this as
> creating an ontology for the problem domain and then designing a
> language that expresses it well. The language is quite likely
> completely unsuitable for just about anything else not tied directly
> to the problem domain. And that's typically where the exercise ends
> — because the cost of making that language real can be quite
> complicated and difficult to justify for the limited domain in which
> it applies. So you use that as a formal specification to guide the
> development of your program in some other language like Python or
> C++ or Erlang depending on which language's strengths most align
> with your design. (I've never encountered a situation where Java,
> C#, or Javascript aligned better with my domain than some other
> cleaner language since either of those languages have been invented.
> Not once.)
>
> Forth naturally landed in this place for small systems, especially
> embedded ones, but since it had no syntax or type system to speak of
> it relied heavily on the strong discipline of the developer not to
> go off the rails — which is why it got the reputation for being a
> write-only language.
>
> ActorForth is primarily inspired by Forth but addresses the two
> core issues I kept running into with my own DSLs:
>
> 1. A strong type system that greatly reduces the need to pay
>    attention to what's on the stack at run time.
> 2. A concurrency model that lets me deal with the unbounded
>    non-determinism that not only uniquely allowed me to deal with
>    real-world embedded control systems, but also greatly reduces
>    the cognitive overload that every other concurrency model imposes
>    when you're dealing with multiple simultaneous things
>    interoperating.
>
> ActorForth is designed to make programming language creation a fun
> creative iterative process that encourages you to explore and try
> things — learning about your domain while expressing your solution
> at the same time. That's what it's for.
>
> You may not like how ActorForth or its RPN notation (which gives it
> a great deal of its simplicity) look — but you don't have to. Just
> use it to quickly build your own language and leave native a4
> behind. Just like the best C++ applications rarely use the native
> types, the best a4 programs rarely use a4 primitives.
>
> Since you can create everything right in its own REPL, you can just
> explore your domain and solution right at the command line without
> having to bring in all kinds of other tooling like build systems and
> linkers. a4 should encourage you to explore and try weird things
> just to see what would happen even if you plan on tossing it away
> minutes later. This is literally how I discovered you can build a
> full compiler in three words with ActorForth.

### Key claims to preserve in the drafted prose

- **Language design is an intellectual pastime, not a chore.** Phrase
  it that way in the preface — matters for readers who think language
  design = academic formal methods.
- **The domain-specific language as ontology + expression.** This is
  the formal framing of what normally gets called "thinking through
  the problem." a4 takes this informal workflow and makes it
  something you can run.
- **The usual exercise ends because making the DSL real is too
  expensive.** a4's proposition: the cost of making the DSL real is
  near zero; you were building it in your head anyway.
- **The DSL you imagined becomes the formal spec for Python/C++/
  Erlang code.** In a4, the DSL IS the program.
- **Java / C# / JavaScript vs "some other cleaner language."** The
  user has never once found those three to be the best domain fit
  since their invention. Include this as stated — it's load-bearing
  for the argument that mainstream languages aren't doing this work.
- **Forth's strengths and its reputation as a "write-only language."**
  Acknowledge honestly. a4 fixes the two issues: types + concurrency.
- **RPN / "you may not like it."** The "leave native a4 behind"
  rhetorical move is important. a4's aesthetic is a means, not the
  point. Builds a DSL you DO like.
- **"The best a4 programs rarely use a4 primitives."** The Stroustrup
  analogy ("best C++ apps rarely use native types") is the perfect
  crystallization. Quote it directly.
- **REPL as exploration environment.** No build system, no linker —
  try things, throw them away. The 3-word-compiler anecdote is a
  concrete proof of the claim; include it (briefly).
- **Unbounded non-determinism for real-world embedded control.** The
  user uniquely points to embedded systems as where this model was
  forced on them by physics — not an academic preference.

### Where this content goes

- **Intro book preface**: this is the lead of the preface, in
  something close to the user's verbatim framing. Readers encounter
  a4's actual purpose before they encounter any syntax.
- **Architectural doc Part 0**: a condensed, more technical version.
  Bridges into the "engineering not math" argument.
- **Talk slide**: probably one or two slides with the "what's it
  for" framing. Strong candidate for a slide that just shows the
  sentence *"The best a4 programs rarely use a4 primitives."*
- **Resume checklist**: add to step 4 ("Draft Part 0") — this section
  is the hook, not a footnote.

## Core argument / narrative arc (Part 0)

In the user's own framing, to preserve voice:

> Language design is super hard because formal specifications are
> hard to scale up and they are incredibly brittle — changing your
> mind later is painful if you started with a complete BNF spec of
> your language and then have separate parsers and lexers that all
> depend on it. This is the difference between the math people and
> the engineers.
>
> Chuck Moore did it as an engineer with Forth: super easy to build
> your own language and change it quickly and evolve it.
>
> You need to be able to formally prove the correctness of small
> simple concepts, but then the realities of the unbounded
> non-determinism of complex real-world systems quickly grow
> complexity and possible execution paths geometrically as the
> capabilities increase linearly. That's why Dijkstra's work stayed
> in algorithms and Hamilton's required massive costs only
> justifiable when human lives are at stake.
>
> What if, by focusing on eliminating all unnecessary complexity and
> having a simple provable foundation, and only writing what was
> necessary to solve the problem you're looking at right now — you
> could build highly scalable complex systems that use emergent
> behavior of orchestrated interoperable simple highly coherent but
> loosely coupled systems?
>
> That's the goal of ActorForth. You can try and see very useful
> results quickly and cheaply so it's no problem to refactor or
> completely throw away core components and start over. Each
> iteration teaches you about the domain of your problem and makes
> you more capable of solving it in novel ways with increasingly
> improved ROIs.

Draft should preserve: the verb "brittle" for formal specs; the
Dijkstra/Hamilton scale contrast; "emergent behavior of simple,
coherent, loosely-coupled components" as the pivot phrase; "each
iteration teaches you about the domain" as the closing beat.

## Simplicity-as-proof sub-section

Working title: **"Simplicity as Proof — Why Reliability > Verifiability."**

This braids together three primary sources and one internal source.
Direct quotes to include:

1. **C.A.R. Hoare**, *The Emperor's Old Clothes* (1980 Turing Award
   Lecture; CACM 24(2), 1981):
   > "There are two ways of constructing a software design: one way
   > is to make it so simple that there are obviously no deficiencies,
   > and the other way is to make it so complicated that there are no
   > obvious deficiencies. The first method is far more difficult."

2. **De Millo, Lipton, Perlis**, *Social Processes and Proofs of
   Theorems and Programs* (CACM 22(5), 1979) — cite the overall
   argument: that program proofs do not scale past toy systems; that
   reliability is produced by social processes (peer review, use,
   revision) rather than by formal verification.
   PDF: https://www.cs.columbia.edu/~angelos/Misc/p271-de_millo.pdf

3. **Einstein (attributed)**:
   > "Everything should be made as simple as possible, but not simpler."
   (Popular paraphrase; the 1933 Spencer lecture has the precise form
   — flag as a paraphrase attribution in the draft.)

4. **Scherrey (2019)**, *ActorForth — Architectural Drivers*
   (`docs/ActorForth.pdf`), page 2:
   > "'Provable programs' not possible past a certain point of
   > complexity. What's more important is simplicity... It is more
   > important that software be made reliable than verifiable —
   > primarily through social processes and experience."
   Page 3:
   > "As simple as possible but no simpler..."

Section title can be the user's remembered paraphrase, framed as the
ActorForth stance crystallized:

> *"A system should be expressed so simply that its correctness is
> obvious to the reader."*

Attribute to B. Scherrey / the 2019 paper as the ActorForth position
— not to Hoare. Note in the draft that the line captures the spirit
of the Hoare quote but is the author's own compression of the stance.

## Intellectual heritage — annotated credits

Each figure gets a paragraph (5–10 lines) covering: who, what work,
how that work shows up in a4. Suggested order (chronological by
primary contribution):

### Edsger W. Dijkstra

- *Notes on Structured Programming* (EWD249, 1970).
- Primary quote (already in WhyActorForth.pdf slide 5): on the
  programmer's responsibility to demonstrate correctness in a
  convincing manner; the object produced must be usefully structured.
- Also: "Program testing can be used to show the presence of bugs,
  but never to show their absence!" (EWD249 corollary)
- How it shows up in a4: type-driven dispatch eliminates whole
  classes of defect by construction; compile-time type inference
  for provable fragments; test framework acknowledges Dijkstra's
  corollary by treating testing as a complement to, not replacement
  for, clarity-of-construction.

### Margaret Hamilton

- Apollo Guidance Computer software; coiner of "software engineering";
  DO-178B lineage; cost-justified formal correctness when lives at
  stake.
- How it shows up in a4: ActorForth accepts Hamilton's standard
  where it applies — as a target for critical subsystems
  (cryptoledger contracts, the bootstrap compiler) — not as a
  universal mandate. The "expand later" note in the user's framing:
  her work deserves fuller treatment than this first pass; placeholder
  for a dedicated sub-section.

### Chuck Moore

- Creator of Forth. Stack-based, concatenative, radically simple.
  Engineer's rather than mathematician's approach to language design.
- How it shows up in a4: the stack IS the state machine (Principle 3);
  compilation IS interpretation; "no syntax, only types" extends
  Moore's "tokens are words" model; DSL-first development as canonical
  workflow.

### Carl Hewitt

- Actor model (1973 onwards). **Unbounded non-determinism** as the
  theoretical differentiator from lambda calculus and CSP (both
  bounded-nondeterministic).
- How it shows up in a4: actors are a primitive, not a library;
  the test framework uses unbounded non-determinism as the
  correctness model for parallel test execution; "actor-as-type"
  (`server`) unifies state with dispatch; `<< ... >>` protocol.
- Critical to weave "unbounded non-determinism" explicitly into
  the narrative — not just a citation, a repeated operational
  claim. Lambda calculus / CSP cannot fully express what real
  distributed systems do; the actor model can.

### Bjarne Stroustrup

- C++; patience with misunderstanding; discipline of architectural
  drivers; never cheating on the requirements of the domain.
- User's anecdote (to edit for voice): explained to a 20-year-old
  on BIX that object-oriented was not just another fad.
- How it shows up in a4: the discipline of stating architectural
  drivers up front (this entire document); refusing to hide warts
  with sugar (no implicit casts, no context register yet, no
  operator overloading magic); patience — this language has been
  in development since 2018.

### Guy L. Steele Jr.

- *Growing a Language* (OOPSLA 1998 keynote) — user-extensible
  language design; the talk famously demonstrates the thesis by
  defining all its own vocabulary in the talk.
- *How to Think about Parallel Programming: Not!* (Strange Loop
  2010) — parallelism should be the compiler's concern, not the
  programmer's; induction as a parallel-friendly alternative to
  iteration.
- *It's Time for a New Old Language* (PPoPP 2017).
- *Four Solutions to a Trivial Problem* (Google Tech Talk, 2015).
- How it shows up in a4: the entire DSL-first workflow is *Growing
  a Language* made operational — users extend the language in a4
  rather than requesting features. Parallelism is implicit through
  actor spawn; the test framework makes this concrete by defaulting
  to maximal parallelism.

Links to confirm in draft (YouTube timed out on automated fetch,
verified others):
- https://www.youtube.com/watch?v=_ahvzDzKdB0
  (Growing a Language, OOPSLA 1998) — YT timed out, human-verified
- https://archive.org/details/GrowingALanguageByGuySteeleAhvzDzKdB0
  (same, archive.org mirror) — verified
- https://www.cs.virginia.edu/~evans/cs655/readings/steele.pdf
  (paper) — serves a 175 KB PDF; user to click-confirm
- https://www.youtube.com/watch?v=dPK6t7echuA
  (Parallel Programming: Not!) — YT timed out
- https://www.infoq.com/presentations/Thinking-Parallel-Programming/
  (same, InfoQ with synced slides) — verified
- https://www.youtube.com/watch?v=7HKbjYqqPPQ
  (New Old Language, PPoPP 2017) — YT timed out
- https://www.youtube.com/watch?v=ftcIcn8AmSY
  (Four Solutions to a Trivial Problem, Google 2015) — YT timed out

### Evan Czaplicki

- Creator of Elm. Elm as a strongly-typed DSL for the DOM / web UI.
  Tooling-as-feature; "errors as helpful messages" discipline; no
  runtime exceptions as a language guarantee.
- How it shows up in a4: the testing DSL's stack-mismatch diagnostic
  feature is an explicit Elm-influence — the language actively helps
  the developer diagnose the failure, does not merely report it.
  The LSP's hover-shows-stack-at-token feature is also in this
  tradition. "Tooling is a feature of the language, not an
  afterthought."

### Other already-in-appendix-A names to keep

- John Backus, 1977 Turing Award Lecture (von Neumann bottleneck).
- Abelson & Sussman, SICP (programs for people to read).
- Kernighan, Elements of Programming Style (debugging harder than
  writing).
- Bastiat (1850), The Law (already cited).

## Inline citations to scatter through IntroToActorForth.md

- **Chapter 1, "The Stack Is Everything"** — footnote citing
  Chuck Moore / Forth when the stack is introduced.
- **Chapter 2, "Types Drive Everything"** — brief paragraph relating
  type-driven dispatch to Dijkstra's construct-for-correctness stance.
- **(future) Actor chapter** — footnote citing Hewitt's *unbounded
  non-determinism* claim; contrast with lambda calculus / CSP.
- **(future) Testing chapter** — open with the De Millo / Hoare
  thread; simplicity-as-proof; "tests show presence, not absence."
- **(future) LLM chapter** — pick up the "no syntax errors" theme
  and the DSL-first-workflow-as-LLM-fit argument.

## Drafting rules

- Draft quality, clearly marked needs-edit at top of file.
- User's voice, not Claude's. Stance first, qualification after.
- No invented quotes. No invented dates. Flag any uncertain
  attribution inline.
- Cross-link: Part 0 / Part 0.5 / Preface / Simplicity section
  should interlink with anchor IDs (`<a id="simplicity-as-proof">`)
  so slides and other docs can deep-link.
- Keep the technical tone. No marketing.

## What's deferred

- **Quote verification for Einstein**: the "simple but not simpler"
  line is a popular paraphrase of a more verbose original from his
  1933 Herbert Spencer lecture. Before publishing, confirm the exact
  source or label the quote as the popular paraphrase.
- **Hamilton section depth**: user flagged "expand on them later."
  First pass can be short; revisit when the talk's dedicated
  correctness-proof section is drafted.
- **Stroustrup BIX anecdote**: present in user's voice in the
  session transcript; needs editing before publication to avoid the
  personal-narrative tone unless the section is explicitly
  first-person.

## Resume checklist for the next session

1. Read this file.
2. Read `docs/WhyActorForth.pdf` (14 slides) — already summarized above.
3. Read `docs/ActorForth.pdf` (3 pages) — key passages quoted above.
4. Draft Part 0 in `docs/architectural-drivers-considerations.md`
   before Part I. ~150 lines.
5. Draft Part 0.5 (intellectual heritage) in same file. ~200 lines.
6. Compact existing Appendix A to point back at Part 0.5. ~30 lines.
7. Draft "Preface: Why ActorForth Exists" for `IntroToActorForth.md`.
   ~120 lines.
8. Insert inline-citation footnotes in existing chapters per map above.
9. Hand over as draft for user editing pass.
