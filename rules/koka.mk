answer: main ../../input
	./$< > $@

main: main.kk
	koka -v0 -o $@ $<
	chmod +x $@

.PHONY: lint
lint:

.PHONY: clean
clean:
	rm -rf answer main .koka
