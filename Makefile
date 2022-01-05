.PHONY: all bench

all:
	dune build

bench:
	dune build @bench
