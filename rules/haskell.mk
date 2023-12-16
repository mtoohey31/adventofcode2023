HASKELL_FLAGS := -Wall -Werror -Wno-incomplete-uni-patterns -Wno-missing-signatures

answer: main ../../input
	./$< > $@

main: Main.hs
	ghc $(HASKELL_FLAGS) -o $@ $^

.PHONY: lint
lint:
	hlint -q Main.hs
	ormolu -m check Main.hs

.PHONY: clean
clean:
	rm -f answer main *.o *.hi
