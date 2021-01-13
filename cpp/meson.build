project('libparser','cpp', 
		version: '1.0.0',
		default_options: ['cpp_std=c++2a'])

add_global_arguments(language : 'cpp')
compiler = meson.get_compiler('cpp')

source = []
subdir('src')
incdir = include_directories('include', 'makham/include', 'makham/include/felspar/makham')
library('libparser', source, include_directories : incdir)