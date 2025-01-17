actorforth
=====

ActorForth is Open Source software covered under BSD license (see
LICENSE.txt).  If you contribute to this project then you guarantee
that you have the right to contribute your content & IP, will abide by
said license and do assign an unlimited, universal, non-exclusive
license to the copyright holder and anyone else who agrees to abide by
the license.

ActorForth was originally written in Python 3.x but hit some technical 
limits in Python's ability to do low level processing and the alternative 
implementation stressed the "functional" iterative aspects of the Python interpreter.
Then we went forward with a modern C++ version but life got in the way and it paused in 2020
having not gotten nearly as far as the Python version. Now I have a practical need for a 
DSL inside some of our BEAM systems software so am attempting a higher level implementation 
in Erlang for 2025. Cross your fingers and toes! Here we go...

See [ActorForthDefinition](docs/ActorForthDefinition.md) for a quick
overview of how the language works.

Experiments implementing an Actor-style programming language on top of
a stack-based processor will be forthcoming as the system is
developed.


Build
-----

    $ rebar3 compile
