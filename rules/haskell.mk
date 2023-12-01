answer: main ../../input
	./$< > $@

main: Main.hs
	ghc -Wall -Wno-missing-signatures -Wno-name-shadowing -o $@ $^

.PHONY: lint
lint:
	hlint -q Main.hs
	ormolu -m check Main.hs

.PHONY: clean
clean:
	rm -f answer main *.o *.hi
