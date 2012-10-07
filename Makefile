#############################################################################
##  v      #                   The Coq Proof Assistant                     ##
## <O___,, #                INRIA - CNRS - LIX - LRI - PPS                 ##
##   \VV/  #                                                               ##
##    //   #  Makefile automagically generated by coq_makefile Vtrunk      ##
#############################################################################

# WARNING
#
# This Makefile has been automagically generated
# Edit at your own risks !
#
# END OF WARNING

#
# This Makefile was generated by the command line :
# coq_makefile -f Make -o Makefile 
#

.DEFAULT_GOAL := all

# 
# This Makefile may take arguments passed as environment variables:
# COQBIN to specify the directory where Coq binaries resides;
# ZDEBUG/COQDEBUG to specify debug flags for ocamlc&ocamlopt/coqc;
# DSTROOT to specify a prefix to install path.

# Here is a hack to make $(eval $(shell works:
define donewline


endef
includecmdwithout@ = $(eval $(subst @,$(donewline),$(shell { $(1) | tr -d '\r' | tr '\n' '@'; })))
$(call includecmdwithout@,$(COQBIN)coqtop -config)

##########################
#                        #
# Libraries definitions. #
#                        #
##########################

OCAMLLIBS?=
COQLIBS?= -R . RelationExtraction
COQDOCLIBS?=-R . RelationExtraction

##########################
#                        #
# Variables definitions. #
#                        #
##########################

RELEXTR_PLUGIN=relation_extraction_plugin

OPT?=
COQDEP?=$(COQBIN)coqdep -c
COQSRCLIBS?=-I $(COQLIB)kernel -I $(COQLIB)lib \
  -I $(COQLIB)library -I $(COQLIB)parsing -I $(COQLIB)pretyping \
  -I $(COQLIB)interp -I $(COQLIB)printing -I $(COQLIB)intf \
  -I $(COQLIB)proofs -I $(COQLIB)tactics -I $(COQLIB)tools \
  -I $(COQLIB)toplevel -I $(COQLIB)grammar \
  -I $(COQLIB)plugins/btauto \
  -I $(COQLIB)plugins/cc \
  -I $(COQLIB)plugins/decl_mode \
  -I $(COQLIB)plugins/extraction \
  -I $(COQLIB)plugins/firstorder \
  -I $(COQLIB)plugins/fourier \
  -I $(COQLIB)plugins/funind \
  -I $(COQLIB)plugins/micromega \
  -I $(COQLIB)plugins/nsatz \
  -I $(COQLIB)plugins/omega \
  -I $(COQLIB)plugins/quote \
  -I $(COQLIB)plugins/romega \
  -I $(COQLIB)plugins/rtauto \
  -I $(COQLIB)plugins/setoid_ring \
  -I $(COQLIB)plugins/syntax \
  -I $(COQLIB)plugins/xml
ZFLAGS=$(OCAMLLIBS) $(COQSRCLIBS) -I $(CAMLP4LIB)

CAMLC?=$(OCAMLC) -c
CAMLOPTC?=$(OCAMLOPT) -c
CAMLLINK?=$(OCAMLC)
CAMLOPTLINK?=$(OCAMLOPT)
GRAMMARS?=grammar.cma
ifeq ($(CAMLP4),camlp5)
CAMLP4EXTEND=pa_extend.cmo q_MLast.cmo pa_macro.cmo
else
CAMLP4EXTEND=
endif
PP?=-pp "$(CAMLP4O) -I $(CAMLLIB) -I . $(COQSRCLIBS) compat5.cmo \
  $(CAMLP4EXTEND) $(GRAMMARS) $(CAMLP4OPTIONS) -impl"

##################
#                #
# Install Paths. #
#                #
##################

ifdef USERINSTALL
XDG_DATA_HOME?=$(HOME)/.local/share
COQLIBINSTALL=$(XDG_DATA_HOME)/coq
COQDOCINSTALL=$(XDG_DATA_HOME)/doc/coq
else
COQLIBINSTALL=${COQLIB}user-contrib
COQDOCINSTALL=${DOCDIR}user-contrib
endif

######################
#                    #
# Files dispatching. #
#                    #
######################

ML4FILES:=g_relation_extraction.ml4

-include $(addsuffix .d,$(ML4FILES))
.SECONDARY: $(addsuffix .d,$(ML4FILES))

MLFILES:=relation_extraction.ml\
  minimlgen.ml\
  host2spec.ml\
  coq_stuff.ml\
  pred.ml

-include $(addsuffix .d,$(MLFILES))
.SECONDARY: $(addsuffix .d,$(MLFILES))

MLLIBFILES:=relation_extraction_plugin.mllib

-include $(addsuffix .d,$(MLLIBFILES))
.SECONDARY: $(addsuffix .d,$(MLLIBFILES))

MLIFILES:=relation_extraction.mli\
  minimlgen.mli\
  host2spec.mli\
  coq_stuff.mli\
  pred.mli\
  host_stuff.mli

-include $(addsuffix .d,$(MLIFILES))
.SECONDARY: $(addsuffix .d,$(MLIFILES))

ALLCMOFILES:=$(ML4FILES:.ml4=.cmo) $(MLFILES:.ml=.cmo)
CMOFILES=$(filter-out $(addsuffix .cmo,$(foreach lib,$(MLLIBFILES:.mllib=_MLLIB_DEPENDENCIES) $(MLPACKFILES:.mlpack=_MLPACK_DEPENDENCIES),$($(lib)))),$(ALLCMOFILES))
CMXFILES=$(CMOFILES:.cmo=.cmx)
OFILES=$(CMXFILES:.cmx=.o)
CMAFILES:=$(MLLIBFILES:.mllib=.cma)
CMXAFILES:=$(CMAFILES:.cma=.cmxa)
CMIFILES=$(sort $(ALLCMOFILES:.cmo=.cmi) $(MLIFILES:.mli=.cmi))
CMXSFILES=$(CMXFILES:.cmx=.cmxs) $(CMXAFILES:.cmxa=.cmxs)
ifeq '$(HASNATDYNLINK)' 'true'
HASNATDYNLINK_OR_EMPTY := yes
else
HASNATDYNLINK_OR_EMPTY :=
endif

#######################################
#                                     #
# Definition of the toplevel targets. #
#                                     #
#######################################

all: $(CMOFILES) $(CMAFILES) $(if $(HASNATDYNLINK_OR_EMPTY),$(CMXSFILES)) \
  ./test

mlihtml: $(MLIFILES:.mli=.cmi)
	 mkdir $@ || rm -rf $@/*
	$(OCAMLDOC) -html -d $@ -m A $(ZDEBUG) $(ZFLAGS) $(^:.cmi=.mli)

all-mli.tex: $(MLIFILES:.mli=.cmi)
	$(OCAMLDOC) -latex -o $@ -m A $(ZDEBUG) $(ZFLAGS) $(^:.cmi=.mli)

.PHONY: all opt byte archclean clean install uninstall_me.sh uninstall userinstall depend html validate ./test

###################
#                 #
# Custom targets. #
#                 #
###################

: 
	
#the following is inserted verbatim

all: $(RELEXTR_PLUGIN).cma $(RELEXTR_PLUGIN).cmxs

clean: clean-plugin

clean-plugin:
	rm -f $(RELEXTR_PLUGIN).a

test: $(RELEXTR_PLUGIN).cma $(RELEXTR_PLUGIN).cmxs

#end verbatim


###################
#                 #
# Subdirectories. #
#                 #
###################

./test:
	cd ./test ; $(MAKE) all

####################
#                  #
# Special targets. #
#                  #
####################

byte:
	$(MAKE) all "OPT:=-byte"

opt:
	$(MAKE) all "OPT:=-opt"

userinstall:
	+$(MAKE) USERINSTALL=true install

install-natdynlink:
	cd . && for i in $(CMXSFILES); do \
	 install -d `dirname $(DSTROOT)$(COQLIBINSTALL)/RelationExtraction/$$i`; \
	 install -m 0644 $$i $(DSTROOT)$(COQLIBINSTALL)/RelationExtraction/$$i; \
	done

install:$(if $(HASNATDYNLINK_OR_EMPTY),install-natdynlink)
	cd . && for i in $(VOFILES) $(CMOFILES) $(CMIFILES) $(CMAFILES); do \
	 install -d `dirname $(DSTROOT)$(COQLIBINSTALL)/RelationExtraction/$$i`; \
	 install -m 0644 $$i $(DSTROOT)$(COQLIBINSTALL)/RelationExtraction/$$i; \
	done
	(cd ./test; $(MAKE) DSTROOT=$(DSTROOT) INSTALLDEFAULTROOT=$(INSTALLDEFAULTROOT)/./test install)

install-doc:
	install -d $(DSTROOT)$(COQDOCINSTALL)/RelationExtraction/mlihtml
	for i in mlihtml/*; do \
	 install -m 0644 $$i $(DSTROOT)$(COQDOCINSTALL)/RelationExtraction/$$i;\
	done

uninstall_me.sh:
	echo '#!/bin/sh' > $@ 
	printf 'cd $${DSTROOT}$(COQLIBINSTALL)/RelationExtraction && rm -f $(CMXSFILES) && find . -type d -and -empty -delete\ncd $${DSTROOT}$(COQLIBINSTALL) && find RelationExtraction -maxdepth 0 -and -empty -exec rmdir -p \{\} \;\n' >> "$@"
	printf 'cd $${DSTROOT}$(COQLIBINSTALL)/RelationExtraction && rm -f $(VOFILES) $(CMOFILES) $(CMIFILES) $(CMAFILES) && find . -type d -and -empty -delete\ncd $${DSTROOT}$(COQLIBINSTALL) && find RelationExtraction -maxdepth 0 -and -empty -exec rmdir -p \{\} \;\n' >> "$@"
	printf 'cd $${DSTROOT}$(COQDOCINSTALL)/RelationExtraction \\\n' >> "$@"
	printf '&& rm -f $(shell find mlihtml -maxdepth 1 -and -type f -print)\n' >> "$@"
	printf 'cd $${DSTROOT}$(COQDOCINSTALL) && find RelationExtraction/mlihtml -maxdepth 0 -and -empty -exec rmdir -p \{\} \;\n' >> "$@"
	chmod +x $@

uninstall: uninstall_me.sh
	sh $<

clean:
	rm -f $(ALLCMOFILES) $(CMIFILES) $(CMAFILES)
	rm -f $(ALLCMOFILES:.cmo=.cmx) $(CMXAFILES) $(CMXSFILES) $(ALLCMOFILES:.cmo=.o) $(CMXAFILES:.cmxa=.a)
	rm -f $(addsuffix .d,$(MLFILES) $(MLIFILES) $(ML4FILES) $(MLLIBFILES) $(MLPACKFILES))
	rm -f all.ps all-gal.ps all.pdf all-gal.pdf all.glob $(VFILES:.v=.glob) $(VFILES:.v=.tex) $(VFILES:.v=.g.tex) all-mli.tex
	- rm -rf html mlihtml uninstall_me.sh
	- rm -rf 
	(cd ./test ; $(MAKE) clean)

archclean:
	rm -f *.cmx *.o
	(cd ./test ; $(MAKE) archclean)

printenv:
	@$(COQBIN)coqtop -config
	@echo CAMLC =	$(CAMLC)
	@echo CAMLOPTC =	$(CAMLOPTC)
	@echo PP =	$(PP)
	@echo COQFLAGS =	$(COQFLAGS)
	@echo COQLIBINSTALL =	$(COQLIBINSTALL)
	@echo COQDOCINSTALL =	$(COQDOCINSTALL)

Makefile: Make
	mv -f $@ $@.bak
	$(COQBIN)coq_makefile -f $< -o $@

	(cd ./test ; $(MAKE) Makefile)

###################
#                 #
# Implicit rules. #
#                 #
###################

%.cmi: %.mli
	$(CAMLC) $(ZDEBUG) $(ZFLAGS) $<

%.mli.d: %.mli
	$(OCAMLDEP) -slash $(OCAMLLIBS) "$<" > "$@" || ( RV=$$?; rm -f "$@"; exit $${RV} )

%.cmo: %.ml4
	$(CAMLC) $(ZDEBUG) $(ZFLAGS) $(PP) -impl $<

%.cmx: %.ml4
	$(CAMLOPTC) $(ZDEBUG) $(ZFLAGS) $(PP) -impl $<

%.ml4.d: %.ml4
	$(OCAMLDEP) -slash $(OCAMLLIBS) $(PP) -impl "$<" > "$@" || ( RV=$$?; rm -f "$@"; exit $${RV} )

%.cmo: %.ml
	$(CAMLC) $(ZDEBUG) $(ZFLAGS) $<

%.cmx: %.ml
	$(CAMLOPTC) $(ZDEBUG) $(ZFLAGS) $<

%.ml.d: %.ml
	$(OCAMLDEP) -slash $(OCAMLLIBS) "$<" > "$@" || ( RV=$$?; rm -f "$@"; exit $${RV} )

%.cmxs: %.cmxa
	$(CAMLOPTLINK) $(ZDEBUG) $(ZFLAGS) -linkall -shared -o $@ $<

%.cmxs: %.cmx
	$(CAMLOPTLINK) $(ZDEBUG) $(ZFLAGS) -shared -o $@ $<

%.cma: | %.mllib
	$(CAMLLINK) $(ZDEBUG) $(ZFLAGS) -a -o $@ $^

%.cmxa: | %.mllib
	$(CAMLOPTLINK) $(ZDEBUG) $(ZFLAGS) -a -o $@ $^

%.mllib.d: %.mllib
	$(COQDEP) -slash $(COQLIBS) -c "$<" > "$@" || ( RV=$$?; rm -f "$@"; exit $${RV} )

# WARNING
#
# This Makefile has been automagically generated
# Edit at your own risks !
#
# END OF WARNING

