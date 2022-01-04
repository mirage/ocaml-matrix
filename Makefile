.PHONY: all bench

all:
	dune build

bench:
	@dune exec -- ./bench/bench.exe --json --minimal --nb-exec 3
