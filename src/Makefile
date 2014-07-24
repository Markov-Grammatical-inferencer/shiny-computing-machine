OC=ocamlopt
OCOPT=-annot -g 
OCC=-annot -g
OCBLIB=str.cma
OCLIB=str.cmxa
GRAPHICS_DIRS=-I +lablgl -I +camlimages
GRAPHICS_LIBS_COMPILED=lablgl.cmxa lablglut.cmxa graphics.cmxa camlimages_core.cmxa camlimages_freetype.cmxa 
GRAPHICS_LIBS_BYTECODE=lablgl.cma lablglut.cma graphics.cma camlimages_core.cma camlimages_freetype.cma 

EXECUTABLES=../SCM ../parse_tree_drawer ../set_map_multi_timing_test

.PHONY: all libs clean
all: libs $(EXECUTABLES)

libs: scm_util.cmo font_wrapper.cmo glr_parser.cmo opengl_visualization.cmo sexprparse.cmo sexprlex.cmo machine_learning_util.cmo markov_chain_generator.cmo

test: file_reader.cmo string_splitter.cmo slice.cmo
	ocaml tests.ml

ngramdisplay.cmx: disambiguator.cmx ngramdisplay.ml
	$(OC) $(OCLIB) graphics.cmxa slice.cmx disambiguator.cmx ngramdisplay.ml


../SCM: spell_checking.cmx disambiguator.cmx file_reader.cmx string_splitter.cmx main.ml 
	$(OC) $(OCOPT) $(OCLIB) -I . slice.cmx spell_checking.cmx disambiguator.cmx file_reader.cmx string_splitter.cmx main.ml -o ../SCM

%.cmo: %.ml
	ocamlc $(OCC) $(OCBLIB) -c $^
%.cmx: %.ml
	ocamlopt $(OCOPT) $(OCLIB) -c $^

disambiguator.cmx: sexprparse.cmx slice.cmx
disambiguator.cmo: sexprparse.cmo slice.cmo

spell_checking.cmx: slice.cmx
spell_checking.cmo: spell_checking.ml
	ocamlc $(OCC) -c str.cma spell_checking.ml

glr_parser.cmx: scm_util.cmx
glr_parser.cmo: scm_util.cmo

machine_learning_util.cmx: scm_util.cmx
machine_learning_util.cmo: scm_util.cmo

markov_chain_generator.cmx: scm_util.cmx glr_parser.cmx
markov_chain_generator.cmo: scm_util.cmo glr_parser.cmo

#produce the editor
editor.cmx: editor.ml
	$(OC) $(OCOPT) -I ../ocaml-curses/ curses.cmxa editor.ml
editor.cmo: editor.ml
	ocamlc $(OCC) -I ../ocaml-curses/ curses.cma editor.ml -o editor

../parse_tree_drawer: scm_util.cmx glr_parser.cmx font_wrapper.cmx opengl_visualization.cmx parse_tree_drawer.ml
	$(OC) $(OCOPT) $(GRAPHICS_DIRS) $(OCLIB) $(GRAPHICS_LIBS_COMPILED) $^ -o ../parse_tree_drawer

opengl_visualization.cmx: scm_util.cmx font_wrapper.cmx opengl_visualization.ml
	$(OC) $(OCOPT) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_COMPILED) $^ -c
opengl_visualization.cmo: scm_util.cmo font_wrapper.cmo opengl_visualization.ml
	ocamlc $(OCC) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_BYTECODE) $^ -c

font_wrapper.cmx: scm_util.cmx font_wrapper.ml
	$(OC) $(OCOPT) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_COMPILED) $^ -c
font_wrapper.cmo: scm_util.cmo font_wrapper.ml
	ocamlc $(OCC) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_BYTECODE) $^ -c


sexpressions.cmx: sexprparse.cmx sexprlex.cmx sexpressions.ml
	$(OC) $(OCOPT) -c $(OCLIB)  sexprparse.cmx sexprlex.cmx sexpressions.ml
sexpressions.cmo: sexprparse.cmo sexprlex.cmo sexpressions.ml
	ocamlc $(OCC) -c sexprparse.cmo sexprlex.cmo sexpressions.ml

sexprparse.cmi sexprparse.mli sexprparse.ml: sexprparse.mly
	ocamlyacc sexprparse.mly
	ocamlc -c sexprparse.mli sexprparse.ml

sexprparse.cmx: sexprparse.ml
	$(OC) $(OCOPT) -c $^

sexprparse.cmo: sexprparse.ml
	ocamlc $(OCC) -c $^

sexprlex.cmi sexprlex.mli sexprlex.ml: sexprlex.mll
	ocamllex sexprlex.mll
	ocamlc -c sexprlex.mli sexprlex.ml

sexprlex.cmx: sexprlex.ml
	$(OC) $(OCOPT) -c sexprlex.ml

../set_map_multi_timing_test: scm_util.cmx set_map_multi_timing_test.ml
	$(OC) $(OCOPT) $^ -o $@

clean:
	rm *.cmi *.cmx *.cmo *.annot *.o $(EXECUTABLES)
