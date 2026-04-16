#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")"
rebar3 eunit --cover
rebar3 cover
