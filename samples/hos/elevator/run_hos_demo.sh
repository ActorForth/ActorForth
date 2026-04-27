#!/bin/bash
# run_hos_demo.sh
#
# Visual HOS demo for live presentation. Walks the audience through
# the elevator spec, then shows three failure cases as a two-phase
# reveal: first the clean source, then the same source with the
# violation highlighted and the Hamilton axiom classification on
# screen. Closes with a quick passing FAT and the line-count
# comparison vs the Python / OCaml / Rust references.
#
# This is the audience-facing demo. The audit-trail proof walk
# (test pillars + skeptic-grade narration) is run_hos_tests.sh.
#
#   ./run_hos_demo.sh           # default, manually advance
#   ./run_hos_demo.sh --auto    # auto-advance on a 4-second timer
#                                 (useful for self-rehearsal)
#
# Exit code 0 if the demo completed; non-zero on script failure.

set -uo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
REPO="$(cd "$HERE/../../.." && pwd)"
cd "$REPO"

AUTO=false
for arg in "$@"; do
    case "$arg" in
        --auto) AUTO=true ;;
    esac
done

ESC=$'\033'
RST="${ESC}[0m"
DIM="${ESC}[2m"
BOLD="${ESC}[1m"
CYAN="${ESC}[1;36m"
YEL="${ESC}[1;33m"
GRN="${ESC}[1;32m"
RED="${ESC}[1;31m"
HL="${ESC}[41;1;37m"        # red bg, bold white fg
ARROW="${ESC}[1;33m"

# ----- helpers -----------------------------------------------------

advance() {
    if [ "$AUTO" = "true" ]; then
        sleep 4
    else
        printf "\n${DIM}  [press enter to continue]${RST} "
        read -r _
    fi
}

clear_screen() {
    printf "${ESC}[2J${ESC}[H"
}

rule() {
    printf "${DIM}"
    printf -- "─%.0s" $(seq 1 70)
    printf "${RST}\n"
}

box_top() {
    printf "${DIM}"
    printf "┌"
    printf -- "─%.0s" $(seq 1 68)
    printf "┐${RST}\n"
}

box_bottom() {
    printf "${DIM}"
    printf "└"
    printf -- "─%.0s" $(seq 1 68)
    printf "┘${RST}\n"
}

scene() {
    clear_screen
    printf "\n  ${CYAN}${BOLD}%s${RST}\n\n" "$1"
}

note() {
    printf "  ${DIM}%s${RST}\n" "$1"
}

# ----- syntax highlighting ----------------------------------------
#
# Bold-cyan HOS keywords; dim comments; everything else as-is.
# Implemented with a single sed pipe that emits ANSI escapes.

highlight_keywords() {
    sed -E \
        -e "s/(#.*)$/${DIM}\\1${RST}/" \
        -e "s/\\b(system|end|on|parent|children|state|transitions|after)\\b/${CYAN}\\1${RST}/g" \
        -e "s/->/${YEL}->${RST}/g"
}

# Show source file with line numbers + syntax highlighting.
show_source() {
    local file="$1"
    local lineno=0
    box_top
    while IFS= read -r line; do
        lineno=$((lineno + 1))
        local prefix
        prefix=$(printf '%3d │ ' "$lineno")
        local body
        body=$(printf '%s' "$line" | highlight_keywords)
        printf "${DIM}│${RST} ${DIM}%s${RST}%s\n" "$prefix" "$body"
    done < "$file"
    box_bottom
}

# Show source with one line highlighted + an arrow annotation.
# $1 file, $2 line number to highlight, $3 annotation text
show_source_with_violation() {
    local file="$1"
    local hl="$2"
    local annot="$3"
    local lineno=0
    box_top
    while IFS= read -r line; do
        lineno=$((lineno + 1))
        local prefix
        prefix=$(printf '%3d │ ' "$lineno")
        if [ "$lineno" = "$hl" ]; then
            printf "${DIM}│${RST} ${RED}%s${RST}${HL}%s${RST}  ${ARROW}← %s${RST}\n" \
                "$prefix" "$line" "$annot"
        else
            local body
            body=$(printf '%s' "$line" | highlight_keywords)
            printf "${DIM}│${RST} ${DIM}%s${RST}%s\n" "$prefix" "$body"
        fi
    done < "$file"
    box_bottom
}

# Show source with an entire range highlighted (for "missing row"
# style violations where one line isn't enough).
show_source_with_range() {
    local file="$1"
    local from="$2"
    local to="$3"
    local annot="$4"
    local lineno=0
    box_top
    while IFS= read -r line; do
        lineno=$((lineno + 1))
        local prefix
        prefix=$(printf '%3d │ ' "$lineno")
        if [ "$lineno" -ge "$from" ] && [ "$lineno" -le "$to" ]; then
            local marker=""
            if [ "$lineno" = "$from" ]; then
                marker="  ${ARROW}← ${annot}${RST}"
            fi
            printf "${DIM}│${RST} ${RED}%s${RST}${HL}%s${RST}%b\n" \
                "$prefix" "$line" "$marker"
        else
            local body
            body=$(printf '%s' "$line" | highlight_keywords)
            printf "${DIM}│${RST} ${DIM}%s${RST}%s\n" "$prefix" "$body"
        fi
    done < "$file"
    box_bottom
}

# ----- axiom classification card ----------------------------------

axiom_card() {
    local title="$1"
    local hamilton="$2"
    local explanation="$3"

    printf "\n  ${YEL}${BOLD}%s${RST}\n" "$title"
    rule
    printf "  ${BOLD}Hamilton's discipline:${RST} %s\n" "$hamilton"
    printf "\n"
    printf "  %s\n" "$explanation"
    rule
}

# ----- run the a4 checker on a spec, show its output indented -----

run_checker() {
    local spec="$1"
    printf "\n  ${BOLD}Checker output:${RST}\n"
    printf "${DIM}"
    samples/hos/elevator/comparison/exploits/a4_check.escript "$spec" 2>&1 \
        | sed 's/^/    /'
    printf "${RST}"
}


# ==================================================================
# SCENE 1 -- Title
# ==================================================================

scene "ActorForth + HOS: A Talk Demo"

cat <<'EOF'
  We will walk through:

    1. The elevator spec, written as four nested HOS systems
       in 93 non-comment lines of a4.

    2. Three classes of design defect that the a4 checker
       refuses to compile, classified by Hamilton's axioms.

    3. The same spec running in a live FAT (functional
       acceptance test).

    4. Line counts vs the Python / OCaml / Rust reference
       implementations (~5x smaller, and the a4 version is
       the only one whose correctness is enforced at compile
       time rather than defended by hand).

EOF
advance


# ==================================================================
# SCENE 2 -- The elevator spec
# ==================================================================

scene "The elevator spec"

note "samples/hos/elevator/elevator.a4 - 233 lines (93 non-comment)."
note "We'll show one system at a time. The full file is in the repo."
echo ""

# Show only the body of elevator.a4 (skip the long comment header).
# Lines ~50 onward contain the actual `system Building ... end` etc.
SPEC_BODY="/tmp/hos_demo_elevator_body.a4"
awk 'BEGIN{showing=0}
     /^system/{showing=1}
     showing{print}' samples/hos/elevator/elevator.a4 > "$SPEC_BODY"

show_source "$SPEC_BODY"
rm -f "$SPEC_BODY"

note ""
note "Notice the structure: parent / children declarations, named"
note "states, declared events, declared transitions with effects."
note "Every name a system references is in its own scope (parent,"
note "children, own events, own states, a4 builtins). Anything else"
note "the checker refuses."
advance


# ==================================================================
# SCENE 3a -- AXIOM 1 (sibling back-channel)
# ==================================================================

scene "Defect 1: A system reaching across the tree"

note "Phase 1: the spec, as written by the author."
echo ""

show_source "samples/hos/elevator/comparison/exploits/flaw_5_bad.a4"

note ""
note "BadEmergency declares its parent as BuildingSystem."
note "Inside its handler body, it references Car."
note "Press enter to ask the checker what it thinks."
advance

# Phase 2: same source, line 17 highlighted (the "Car trigger-emergency").
scene "Defect 1: AXIOM 1 violation: sibling back-channel"

show_source_with_violation \
    "samples/hos/elevator/comparison/exploits/flaw_5_bad.a4" \
    17 \
    "AXIOM 1 (scope)"

axiom_card \
    "Hamilton AXIOM 1: Immediate Control (Scope)" \
    "a system may reference its parent, its declared children, its own state names, its own events, and a4 builtins. Nothing else." \
    "BadEmergency reaches across the tree to Car (its sibling under
  BuildingSystem). Sibling back-channels are the canonical way
  control hierarchies decay: subsystems start coordinating without
  going through their common parent, the dependency graph becomes
  a mesh, and reasoning about local behaviour stops working.

  The compiler refuses to register this system."

run_checker "samples/hos/elevator/comparison/exploits/flaw_5_bad.a4"
advance


# ==================================================================
# SCENE 3b -- AXIOM 5 (incomplete emergency coverage)
# ==================================================================

scene "Defect 2: An emergency the system can silently drop"

note "Phase 1: a stateful car with a stop event."
note "Read the transitions block carefully."
echo ""

show_source "samples/hos/elevator/comparison/exploits/flaw_3_bad.a4"

note ""
note "stop appears as an emergency trigger from Idle and Moving."
note "Look at Loading. Press enter to ask the checker what's wrong."
advance

scene "Defect 2: AXIOM 5 violation: incomplete emergency coverage"

show_source_with_range \
    "samples/hos/elevator/comparison/exploits/flaw_3_bad.a4" \
    27 \
    28 \
    "stop covers Idle and Moving but not Loading"

axiom_card \
    "Hamilton AXIOM 5: Input Rejection (Exhaustive Emergency)" \
    "every emergency trigger must be honoured from every reachable state, or the system can silently swallow the event." \
    "BadCar's transitions table declares stop from Idle and Moving
  (both reach Halted). Loading is reachable (Moving -> Loading
  done) but has no stop transition. An emergency arriving while
  the car is in Loading would be input-rejected without notice.
  Dropped fire alarms.

  The exhaustive-emergency check finds the missing row at compile
  time. The author sees the gap before the fire."

run_checker "samples/hos/elevator/comparison/exploits/flaw_3_bad.a4"
advance


# ==================================================================
# SCENE 3c -- Stateful body rule
# ==================================================================

scene "Defect 3: Stateful handler with body code"

note "Phase 1: a stateful system with an on-handler that does work."
echo ""

show_source "samples/hos/elevator/comparison/exploits/flaw_stateful_body.a4"

note ""
note "On a stateful system, what should this do? The transitions"
note "table says go fires Idle -> Running. The body says drop."
note "Press enter to ask the checker."
advance

scene "Defect 3: Stateful body rule violation"

show_source_with_violation \
    "samples/hos/elevator/comparison/exploits/flaw_stateful_body.a4" \
    21 \
    "stateful body must be empty"

axiom_card \
    "Hamilton: Stateful Body Rule" \
    "a system that declares a state is driven by its transitions table, not by handler body code. The on declaration documents that the system knows about the event; the table decides what happens." \
    "BadStatefulBody declares state BadStatefulBodyState. Its on go
  handler then carries body code (drop). The transitions table
  also declares Idle -> Running go. Two mechanisms competing for
  one event.

  The checker resolves the ambiguity by refusing the question:
  stateful handlers must declare empty bodies. Effects belong on
  transition rows; state changes belong in the transitions table.
  No handler body code on stateful systems, full stop."

run_checker "samples/hos/elevator/comparison/exploits/flaw_stateful_body.a4"
advance


# ==================================================================
# SCENE 4 -- The valid spec running
# ==================================================================

scene "The same checker, the valid spec, a live FAT"

note "samples/hos/elevator/elevator.a4 passes every axiom check."
note "We run the elevator FAT suite below. Each test exercises a"
note "different control path: door open/close, motor travel, the"
note "two-elevator dispatch, the emergency-routing scenario."
echo ""

printf "${DIM}"
rebar3 eunit --module=af_hos_elevator_tests 2>&1 \
    | sed -E 's/\x1b\[[0-9;]*m//g' \
    | sed -e '/===>/d' \
          -e '/==* EUnit =*/d' \
          -e '/^=*$/d' \
          -e '/^ *\[done in/d' \
          -e '/^module /d' \
          -e '/^$/d' \
    | sed 's/^/    /'
printf "${RST}"

echo ""
note "Same checker. Same axioms. Whether the spec is rejected or"
note "accepted is decided at compile time, not at runtime."
advance


# ==================================================================
# SCENE 5 -- Line counts
# ==================================================================

scene "Same controller, four languages, line counts"

printf "    %-40s %7s\n" "File" "Lines"
printf "    %-40s %7s\n" "----" "-----"
for f in samples/hos/elevator/elevator.a4 \
         samples/hos/elevator/comparison/elevator_naive.py \
         samples/hos/elevator/comparison/elevator_correct.py \
         samples/hos/elevator/comparison/elevator_naive.ml \
         samples/hos/elevator/comparison/elevator_correct.ml \
         samples/hos/elevator/comparison/elevator_naive.rs \
         samples/hos/elevator/comparison/elevator_correct.rs
do
    n=$(wc -l < "$f")
    printf "    %-40s %7s\n" "$(basename "$f")" "$n"
done
echo ""
nonblank_a4=$(awk 'NF && !/^[[:space:]]*#/' samples/hos/elevator/elevator.a4 | wc -l)
printf "    a4 non-comment / non-blank lines: ${GRN}%s${RST}\n" "$nonblank_a4"
echo ""
note "The a4 spec is 93 non-comment lines. The smallest correct"
note "reference (Python) is 444. Ratio ~4.7x. The a4 version is"
note "also the only version whose correctness properties are"
note "enforced at compile time rather than defended by hand."
advance


# ==================================================================
# SCENE 6 -- Closing
# ==================================================================

scene "Three defects, three Hamilton axioms"
# (no em-dashes; using hyphens elsewhere in this script per repo convention)

cat <<EOF
  We saw three minimal specs, each containing exactly one defect
  in the body of an otherwise-valid system:

    AXIOM 1 (scope)              : sibling back-channel
    AXIOM 5 (exhaustive trigger) : emergency coverage gap
    Stateful body rule           : state machine + handler code

  Every one of these is a class of bug in real systems-of-systems
  code. Every one of these the a4 checker refuses to compile,
  by construction.

  ${BOLD}The structural claim:${RST} declarations tell you what the
  system can do. The transitions table tells you when. The
  axioms tell you what cannot be expressed. There is no fourth
  thing.

EOF
ok_msg="Demo complete."
printf "  ${GRN}${BOLD}%s${RST}\n\n" "$ok_msg"
