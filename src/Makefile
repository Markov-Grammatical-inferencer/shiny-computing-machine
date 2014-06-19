OC=ocamlopt
OCOPT=-annot -g -p
OCC=-annot -g
OCLIB=str.cmxa
GRAPHICS_DIRS=-I +lablgl -I +camlimages
GRAPHICS_LIBS_COMPILED=lablgl.cmxa lablglut.cmxa graphics.cmxa camlimages_core.cmxa camlimages_freetype.cmxa 
GRAPHICS_LIBS_BYTECODE=lablgl.cma lablglut.cma graphics.cma camlimages_core.cma camlimages_freetype.cma 

all: ../SCM ../parse_tree_drawer libs

libs: scm_util.cmo font_wrapper.cmo glr_parser.cmo opengl_visualization.cmo
	touch libs #dummy file so that make gets the right timestamp

test: file_reader.cmo string_splitter.cmo slice.cmo
	ocaml tests.ml

ngramdisplay.cmx: disambiguator.cmx ngramdisplay.ml
	$(OC) $(OCLIB) graphics.cmxa slice.cmx disambiguator.cmx ngramdisplay.ml


../SCM: spell_checking.cmx disambiguator.cmx file_reader.cmx string_splitter.cmx main.ml 
	$(OC) $(OCOPT) $(OCLIB) -I . slice.cmx spell_checking.cmx disambiguator.cmx file_reader.cmx string_splitter.cmx main.ml -o ../SCM


disambiguator.cmx: slice.cmx disambiguator.ml
	$(OC) $(OCOPT) -c disambiguator.ml
disambiguator.cmo: slice.cmo disambiguator.ml
	ocamlc $(OCC) -c disambiguator.ml

file_reader.cmx: file_reader.ml
	$(OC) $(OCOPT) -c file_reader.ml
file_reader.cmo: file_reader.ml
	ocamlc $(OCC) -c file_reader.ml

scm_util.cmx: scm_util.ml
	$(OC) $(OCOPT) -c scm_util.ml
scm_util.cmo: scm_util.ml
	ocamlc $(OCC) -c scm_util.ml

string_splitter.cmx: string_splitter.ml
	$(OC) $(OCOPT) -c string_splitter.ml 
string_splitter.cmo: string_splitter.ml
	ocamlc $(OCC) -c string_splitter.ml

slice.cmx: slice.ml
	$(OC) $(OCOPT) -c slice.ml
slice.cmo: slice.ml
	ocamlc $(OCC) -c  slice.ml

spell_checking.cmx: slice.cmx spell_checking.ml 
	$(OC) $(OCOPT) -c $^

spell_checking.cmo: spell_checking.ml
	ocamlc $(OCC) -c str.cma spell_checking.ml

glr_parser.cmx: scm_util.cmx glr_parser.ml
	$(OC) $(OCOPT) -c glr_parser.ml
glr_parser.cmo: scm_util.cmo glr_parser.ml
	ocamlc $(OCC) -c glr_parser.ml
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

clean:
	rm *.cmi *.cmx *.cmo *.annot *.o
