#COMP = byte
COMP = native

ifeq (${COMP},native)
OCAMLC = ocamlfind ocamlopt
EXT    = cmx
else
OCAMLC = ocamlfind ocamlc -g
EXT    = cmo
endif

SRCS  = pqueue.ml boardrep.ml boardmetric.ml board.ml astar.ml npuzzle.ml
OBJS  = ${patsubst %.ml, %.${EXT}, ${SRCS}}
PKGS  = batteries,ounit2
PROG  = npuzzle
TESTS = tests_final

compile:
	${OCAMLC} -c pqueue.mli
	${OCAMLC} -c pqueue.ml -package ${PKGS}
	${OCAMLC} -c boardrep.mli
	${OCAMLC} -c boardrep.ml
	${OCAMLC} -c boardmetric.mli
	${OCAMLC} -c boardmetric.ml
	${OCAMLC} -c board.mli
	${OCAMLC} -c board.ml
	${OCAMLC} -c astar.mli
	${OCAMLC} -c astar.ml
	${OCAMLC} -c npuzzle.mli
	${OCAMLC} -c npuzzle.ml
	${OCAMLC} -c main.ml
	${OCAMLC} -package ${PKGS} -linkpkg ${OBJS} main.${EXT} -o ${PROG}

test: ${TESTS}.ml ${OBJS}
	${OCAMLC} -o ${TESTS} -package ${PKGS} -linkpkg ${OBJS} ${TESTS}.ml
	./${TESTS}

all: compile test

clean:
	rm -f *.cmi *.cmo *.cmx *.o ${PROG}
	rm -f npuzzle.out
	rm -f ${TESTS} *.log *.cache

