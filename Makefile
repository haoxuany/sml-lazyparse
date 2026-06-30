BINDIR = bin
HEAP = $(BINDIR)/lazyparse-heap
WRAPPER = $(BINDIR)/lazyparse

SMLNJ = sml
SOURCES = src/main/sources.cm

.PHONY: all clean

all: $(WRAPPER)

$(HEAP): $(shell find src lib -name '*.sml' -o -name '*.cm' -o -name '*.sig' -o -name '*.lex' -o -name '*.grm')
	@mkdir -p $(BINDIR)
	echo 'CM.make "$(SOURCES)"; SMLofNJ.exportFn ("$(HEAP)", fn (_, args) => (Main.main (); OS.Process.success));' | $(SMLNJ)

$(WRAPPER): $(HEAP)
	@printf '#!/bin/sh\nexec sml @SMLload="$$(dirname "$$0")/lazyparse-heap" @SMLdebug=/dev/null "$$@"\n' > $@
	@chmod +x $@
	@echo "built $@"

clean:
	rm -f $(HEAP).* $(WRAPPER)
	find src -name .cm -type d -exec rm -rf {} + 2>/dev/null || true
