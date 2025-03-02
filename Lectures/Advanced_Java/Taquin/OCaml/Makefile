export OCAMLFLAGS    = -unsafe
export OCAMLBCFLAGS  = -g
export OCAMLBLDFLAGS = -g
export LIBS          = str unix

define PROJ_taquin
  SOURCES = log.ml implicitGraph.ml astar.ml fsm.ml idastar.ml grapheTaquin.ml iO.ml trie.ml taquin.ml
  RESULT  = taquin
endef
export PROJ_taquin

define PROJ_explore
  SOURCES = log.ml trie.ml implicitGraph.ml grapheTaquin.ml iO.ml explore.ml
  RESULT  = explore
endef
export PROJ_explore

define PROJ_generator
  SOURCES = log.ml grapheTaquin.ml iO.ml permutations.ml generator.ml
  RESULT  = generator
endef
export PROJ_generator

define PROJ_validator
  SOURCES = log.ml grapheTaquin.ml iO.ml validator.ml
  RESULT  = validator
endef
export PROJ_validator

ifndef SUBPROJS
  export SUBPROJS = taquin explore generator validator
endif

all: native-code

export:
	tar -cvz --exclude CVS -f test.tar.gz test
	rsync -e ssh -vrR test.tar.gz pottier@kelen.polytechnique.fr:www/pi/2005

validate: all
	./validator 4 2>log & tail -f log

%:
	@$(MAKE) -f OCamlMakefile -r -s subprojs SUBTARGET=$@
