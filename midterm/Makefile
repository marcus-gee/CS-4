default: compile

NAME = midterm
OCAMLC = ocamlfind ocamlc
UTIL = tree_utils
DRAW = tree_draw

all: compile test

compile: clean
	${OCAMLC} -c ${NAME}.mli ${NAME}.ml

test:
	${OCAMLC} -o tests_${NAME} \
	 -package ounit2 -linkpkg \
	  ${NAME}.cmo tests_${NAME}.ml
	./tests_${NAME}

draw: compile
	${OCAMLC} -c ${UTIL}.ml
	${OCAMLC} -c ${DRAW}.ml
	${OCAMLC} -o make_drawings \
	 ${NAME}.cmo ${UTIL}.cmo ${DRAW}.cmo
	./make_drawings
	for n in 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16; do \
	  dot -Tpng t$$n.dot -o t$$n.png; \
	done
	for m in 4 7 11 12 16; do \
	  dot -Tpng t$$m\_n.dot -o t$$m\_n.png; \
	done

clean:
	rm -f *.cm* *.log *.cache tests_${NAME}

clean_draw:
	rm -f *.dot *.png make_drawings

clean_all: clean clean_draw
