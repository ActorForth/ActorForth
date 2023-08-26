# ActorForth

NOTE: When checking out this repo use --recursive to automatically get the external submodule dependencies.

[![Join the chat at https://gitter.im/ActorForth/community](https://badges.gitter.im/ActorForth/community.svg)](https://gitter.im/ActorForth/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

ActorForth is Open Source software covered under BSD license (see
LICENSE.txt).  If you contribute to this project then you guarantee
that you have the right to contribute your content & IP, will abide by
said license and do assign an unlimited, universal, non-exclusive
license to the copyright holder and anyone else who agrees to abide by
the license.

ActorForth was originally written in Python 3.x but hit some technical limits in Python's ability to do low level processing and the alternative implementation stressed the "functional" iterative aspects of the Python interpreter so we have now switched to modern C++ and are back on track.

See [ActorForthDefinition](docs/ActorForthDefinition.md) for a quick
overview of how the language works.

Experiments implementing an Actor-style programming language on top of
a stack-based processor will be forthcoming as the system is
developed.

To build you need clang++ installed. On a Debian derivative you should be able to install with this:

`
sudo apt install cmake ninja-build clang-15 lld-15 libc++-15-dev libc++abi-15-dev llvm-15-dev clang-15-doc llvm-15-doc
`

Now get linux to use your version of clang++. Can adjust that '150' number to whatever is the highest of your current installs.

`
sudo update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-15 150
`

To see the other installs or pick one manually do this:

`
sudo update-alternatives --config clang++
`

