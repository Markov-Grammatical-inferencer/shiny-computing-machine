OC=ocamlopt
OCOPT=-annot -g -p
OCC=-annot -g
OCLIB=str.cmxa
GRAPHICS_DIRS=-I +lablgl -I +camlimages
GRAPHICS_LIBS_COMPILED=lablgl.cmxa lablglut.cmxa graphics.cmxa camlimages_core.cmxa camlimages_freetype.cmxa 
GRAPHICS_LIBS_BYTECODE=lablgl.cma lablglut.cma graphics.cma camlimages_core.cma camlimages_freetype.cma 

EXECUTABLES=../SCM ../parse_tree_drawer ../set_map_multi_timing_test

.PHONY: all libs clean
all: libs $(EXECUTABLES)

libs: scm_util.cmo font_wrapper.cmo glr_parser.cmo opengl_visualization.cmo

test: file_reader.cmo string_splitter.cmo slice.cmo
	ocaml tests.ml

ngramdisplay.cmx: disambiguator.cmx ngramdisplay.ml
	$(OC) $(OCLIB) graphics.cmxa slice.cmx disambiguator.cmx ngramdisplay.ml


../SCM: spell_checking.cmx disambiguator.cmx file_reader.cmx string_splitter.cmx main.ml 
	$(OC) $(OCOPT) $(OCLIB) -I . slice.cmx spell_checking.cmx disambiguator.cmx file_reader.cmx string_splitter.cmx main.ml -o ../SCM

%.cmo: %.ml
	ocamlc $(OCC) -c $<
%.cmx: %.ml
	ocamlopt $(OCOPT) -c $<

disambiguator.cmx: slice.cmx
disambiguator.cmo: slice.cmo

spell_checking.cmx: slice.cmx
spell_checking.cmo: spell_checking.ml
	ocamlc $(OCC) -c str.cma spell_checking.ml

glr_parser.cmx: scm_util.cmx
glr_parser.cmo: scm_util.cmo

#produce the editor
editor.cmx: editor.ml
	$(OC) $(OCOPT) -I ../ocaml-curses/ curses.cmxa editor.ml
editor.cmo: editor.ml
	ocamlc $(OCC) -I ../ocaml-curses/ curses.cma editor.ml -o editor

../parse_tree_drawer: scm_util.cmx glr_parser.cmx font_wrapper.cmx opengl_visualization.cmx parse_tree_drawer.ml
	$(OC) $(OCOPT) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_COMPILED) $^ -o ../parse_tree_drawer

opengl_visualization.cmx: scm_util.cmx font_wrapper.cmx opengl_visualization.ml
	$(OC) $(OCOPT) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_COMPILED) $^ -c
opengl_visualization.cmo: scm_util.cmo font_wrapper.cmo opengl_visualization.ml
	ocamlc $(OCC) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_BYTECODE) $^ -c

font_wrapper.cmx: scm_util.cmx font_wrapper.ml
	$(OC) $(OCOPT) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_COMPILED) $^ -c
font_wrapper.cmo: scm_util.cmo font_wrapper.ml
	ocamlc $(OCC) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_BYTECODE) $^ -c


sexpressions.cmx:sexprparse.cmx sexprlex.cmx sexpressions.ml
	$(OC) $(OCOPT) -c $(OCLIB)  sexprparse.cmx sexprlex.cmx sexpressions.ml
sexpressions.cmo:sexprparse.cmo sexprlex.cmo sexpressions.ml
	ocamlc $(OCC) -c sexprparse.cmo sexprlex.cmo sexpressions.ml

sexprparse.cmx: sexprparse.mly
	ocamlyacc sexprparse.mly
	$(OC) $(OCOPT) -c sexprparse.ml

sexprparse.cmo: sexprparse.mly
	ocamlyacc sexprparse.mly
	ocamlc $(OCC) -c sexprparse.ml

sexprlex.cmx:sexprlex.mll sexprparse.cmx
	ocamllex sexprlex.mll
	$(OC) $(OCOPT) -c sexprparse.cmx sexprlex.ml

../set_map_multi_timing_test: scm_util.cmx set_map_multi_timing_test.ml
	$(OC) $(OCOPT) $^ -o $@

clean:
	rm *.cmi *.cmx *.cmo *.annot *.o $(EXECUTABLES)
