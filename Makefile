EMACS?=	emacs

ELCS=	nspawn-tramp.elc

.PHONY:	all clean

all:	$(ELCS)

clean:
	rm -f $(ELCS)

%.elc:	%.el
	$(EMACS) -Q --batch -f batch-byte-compile $<
