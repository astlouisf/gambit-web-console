GSC=~/gambit/gsc/gsc
UNIVLIB=~/gambit/univ-lib/lib/lib.scm

all:
	$(GSC) -c -target js -prelude '(include "$(UNIVLIB)")' console.scm

clean:
	rm console.js
