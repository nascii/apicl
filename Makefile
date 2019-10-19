.DEFAULT_GOAL := all

all: .quicklisp/setup.lisp

.quicklisp/setup.lisp:
	quicklisp init
