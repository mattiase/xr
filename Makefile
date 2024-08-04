EMACS=emacs
EMFLAGS=-Q -batch -L .
BYTECOMPFLAGS=--eval '(setq byte-compile-error-on-warn t)'

EL = xr.el xr-test.el
ELC=$(EL:.el=.elc)

.PHONY: build check clean

build: $(ELC)

clean:
	rm -f $(ELC)

check:
	$(EMACS) $(EMFLAGS) -l xr-test -f ert-run-tests-batch-and-exit

%.elc: %.el
	$(EMACS) $(EMFLAGS) $(BYTECOMPFLAGS) -f batch-byte-compile $^
