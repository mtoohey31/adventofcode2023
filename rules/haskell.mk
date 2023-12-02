HASKELL_FLAGS := \
-Wall \
-Wno-incomplete-patterns \
-Wno-incomplete-uni-patterns \
-Wno-missing-signatures \
-Wno-name-shadowing

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
