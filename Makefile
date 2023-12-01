MAKEFLAGS := --silent

.PHONY: all
all:

.PHONY: lint
lint::

.PHONY: clean
clean::

define ANSWER_template =
all: $(1)/answer

.PHONY: $(1)/answer
$(1)/answer: $(1)/../.envrc
	cd $(1); nix develop ../../..#$(shell grep -oP '(?<=^use flake \.\./\.\.#).*' $(1)/../.envrc) --keep HOME -ic $$(MAKE) --silent answer

lint::
	cd $(1); nix develop ../../..#$(shell grep -oP '(?<=^use flake \.\./\.\.#).*' $(1)/../.envrc) -ic $$(MAKE) --silent lint

clean::
	cd $(1); make clean
endef

$(foreach part,$(foreach lang,$(foreach day,$(wildcard day??),$(wildcard $(day)/*)),$(wildcard $(lang)/part?)),$(eval $(call ANSWER_template,$(part))))

# Diff gets checked after answer builds.
.PHONY: diff
all: diff
diff:
	for day in day??; do for part in 1 2; do diff -u --from-file "$$day"/*/"part$$part/answer"; done; done

# Lint gets checked last.
all: lint
