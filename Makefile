
DATAFILE=../ngrams.tsv

all:
	echo "PYTHON"
	make python
	echo "PYPY"
	make pypy
	echo "D (LDC)"
	make d-ldc
	echo "D (DMD)"
	make d-dmd
	echo "Nim"
	make nim
	echo "Golang"
	make golang
	echo "Haskell"
	make haskell

python:
	cd python && ../repeat.rb python ./max_column_sum_by_key.py $(DATAFILE) 1 2

pypy:
	cd python && ../repeat.rb pypy ./max_column_sum_by_key.py $(DATAFILE) 1 2

d-ldc:
	-rm d/max_column_sum_by_key d/max_column_sum_by_key.o
	cd d && ldc2 -release -O max_column_sum_by_key.d
	cd d && ../repeat.rb ./max_column_sum_by_key $(DATAFILE) 1 2

d-dmd:
	-rm d/max_column_sum_by_key d/max_column_sum_by_key.o
	cd d && dmd -release -O -inline -boundscheck=off -of=./max_column_sum_by_key max_column_sum_by_key.d
	cd d && ../repeat.rb ./max_column_sum_by_key $(DATAFILE) 1 2

nim:
	cd nim && nim c -d:release max_column_sum_by_key.nim
	cd nim && ../repeat.rb ./max_column_sum_by_key $(DATAFILE) 1 2

golang:
	-rm go/csvtest
	cd go && go build csvtest.go
	cd go && ../repeat.rb ./csvtest $(DATAFILE) 1 2

haskell:
	cd faster-hs && \
		stack clean && \
		stack build && \
		../repeat.rb stack exec -- faster-hs $(DATAFILE) 1 2

rust:
	cd faster-rs && \
		cargo clean && \
		cargo build --release && \
		../repeat.rb ./target/release/faster-rs $(DATAFILE) 1 2

rust-profile:
	-rm -f ./faster-rs/faster-rs.stacks
	cd faster-rs && \
		cargo clean && \
		cargo build --release
	cd faster-rs && sudo dtrace -c './target/release/faster-rs ../ngrams.tsv 1 2' -o faster-rs.stacks -n 'profile-997 /execname == "faster-rs"/ { @[ustack(100)] = count(); }'
	~/s/FlameGraph/stackcollapse.pl ./faster-rs/faster-rs.stacks | ~/s/FlameGraph/flamegraph.pl > ./faster-rs/faster-rs.svg

.PHONY: python pypy d-ldc d-dmd nim golang haskell rust rust-profile
