EMACS=emacs
EMFLAGS=-Q -batch -L .
BYTECOMPFLAGS=--eval '(setq byte-compile-error-on-warn t)'
ERTFLAGS=--eval '(setq ert-batch-backtrace-right-margin nil ert-batch-print-length 100 ert-batch-print-level 12)'

EL = xr.el xr-test.el
ELC=$(EL:.el=.elc)

.PHONY: build check clean

build: $(ELC)

clean:
	rm -f $(ELC)

check:
	$(EMACS) $(EMFLAGS) $(ERTFLAGS) -l xr-test -f ert-run-tests-batch-and-exit

%.elc: %.el
	$(EMACS) $(EMFLAGS) $(BYTECOMPFLAGS) -f batch-byte-compile $^
