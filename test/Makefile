# Configuration
OCAMLC=$(CAMLBIN)ocamlc
COQC=$(COQBIN)coqc

##############################################################################
# usage:
# make          launch the tests
# make clean    clean temp files
##############################################################################

TESTS=add.v imp.v fibo.v so.v odd.v tuples.v
ML_TESTS=$(subst .v,.ml,$(TESTS))
ML_TESTS_OBJ=$(subst .v,.cmo,$(TESTS))

all: test

%.ml: %.v
	$(COQC) $< > $@

%.cmo: %.ml
	$(OCAMLC) -c $<

tester.cmo: tester.ml $(ML_TESTS_OBJ)
	$(OCAMLC) -c $<

tester: $(ML_TESTS_OBJ) tester.cmo
	$(OCAMLC) -o $@ $^

.PHONY: clean test

test: tester
	./tester

clean:
	rm -f $(ML_TESTS) *.cmo *.cmi *.vo *.glob tester
