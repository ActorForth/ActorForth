project('parser','cpp', 
		version: '1.0.0',
		default_options: ['cpp_std=c++2a'])

add_global_arguments('-fcoroutines-ts', language : 'cpp')
compiler = meson.get_compiler('cpp')

source = []
subdir('src')
incdir = include_directories('include', 'coro/include', 'coro/include/felspar/coro') # , '/usr/lib/llvm-10/bin/../include/c++/v1')
library('libparser', source, include_directories : incdir)
