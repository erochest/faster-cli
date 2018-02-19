
python:
	time python ./python/max_column_sum_by_key.py ./ngrams.tsv 1 2

pypy:
	time pypy ./python/max_column_sum_by_key.py ./ngrams.tsv 1 2

d-ldc:
	-rm d/max_column_sum_by_key d/max_column_sum_by_key.o
	cd d && ldc2 -release -O max_column_sum_by_key.d
	time ./d/max_column_sum_by_key ./ngrams.tsv 1 2

d-dmd:
	-rm d/max_column_sum_by_key d/max_column_sum_by_key.o
	cd d && dmd -release -O -inline -boundscheck=off -of=./max_column_sum_by_key max_column_sum_by_key.d
	time ./d/max_column_sum_by_key ./ngrams.tsv 1 2

nim:
	cd nim && nim c -d:release max_column_sum_by_key.nim
	time ./nim/max_column_sum_by_key ./ngrams.tsv 1 2

golang:
	cd go && go build csvtest.go
	time ./go/csvtest ./ngrams.tsv 1 2

.PHONY: python pypy d-ldc d-dmd nim golang
