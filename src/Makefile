OC=ocamlopt
OCOPT=-annot -g -p
OCC=-annot -g
OCLIB=str.cmxa
GRAPHICS_DIRS=-I +lablgl -I +camlimages
GRAPHICS_LIBS_COMPILED=lablgl.cmxa lablglut.cmxa graphics.cmxa camlimages_core.cmxa camlimages_freetype.cmxa 
GRAPHICS_LIBS_BYTECODE=lablgl.cma lablglut.cma graphics.cma camlimages_core.cma camlimages_freetype.cma 

all: ../SCM ../glr_parser ../font_wrapper libs

libs: scm_util.cmo font_wrapper.cmo glr_parser.cmo
	touch libs #dummy file so that make gets the right timestamp

test: filereader.cmo string_splitter.cmo slice.cmo
	ocaml tests.ml


../SCM: disambiguator.cmx file_reader.cmx string_splitter.cmx main.ml
	$(OC) $(OCOPT) $(OCLIB)  -I .  slice.cmx disambiguator.cmx file_reader.cmx string_splitter.cmx main.ml -o ../SCM


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
	ocamlc $(OCC) -c slice.ml

../glr_parser: scm_util.cmx font_wrapper.cmx glr_parser.ml
	$(OC) $(OCOPT) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_COMPILED) $^ -o ../glr_parser
glr_parser.cmo: scm_util.cmo font_wrapper.cmo glr_parser.ml
	ocamlc $(OCC) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_BYTECODE) $^ -c

../font_wrapper: scm_util.cmx font_wrapper.ml
	$(OC) $(OCOPT) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_COMPILED) $^ -o ../font_wrapper
font_wrapper.cmx: scm_util.cmx font_wrapper.ml
	$(OC) $(OCOPT) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_COMPILED) $^ -c
font_wrapper.cmo: scm_util.cmo font_wrapper.ml
	ocamlc $(OCC) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_BYTECODE) $^ -c

clean:
	rm *.cmi  *.cmx *.cmo *.annot *.o
