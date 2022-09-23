EMACS?=	emacs

ELCS=	tramp-nspawn.elc

.PHONY:	all clean

all:	$(ELCS)

clean:
	rm -f $(ELCS)

%.elc:	%.el
	$(EMACS) -Q --batch -f batch-byte-compile $<
