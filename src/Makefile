OC=ocamlopt
OCOPT=-annot -g -p
OCC=-annot -g
OCLIB=str.cmxa
GRAPHICS_DIRS=-I +lablgl -I +camlimages
GRAPHICS_LIBS_COMPILED=lablgl.cmxa lablglut.cmxa graphics.cmxa camlimages_core.cmxa camlimages_freetype.cmxa 
GRAPHICS_LIBS_BYTECODE=lablgl.cma lablglut.cma graphics.cma camlimages_core.cma camlimages_freetype.cma 
all: binary glr_parser font_wrapper

test: filereader string_splitter slice
	ocaml tests.ml


binary: disambiguator filereader string_splitter
	$(OC) $(OCOPT) $(OCLIB)  -I .  slice.cmx disambiguator.cmx file_reader.cmx string_splitter.cmx  main.ml -o ../SCM
	

disambiguator: slice
	$(OC) $(OCOPT) -c disambiguator.ml 
	ocamlc $(OCC) -c disambiguator.ml

filereader:
	$(OC) $(OCOPT) -c file_reader.ml
	ocamlc $(OCC) -c file_reader.ml

string_splitter:
	$(OC) $(OCOPT) -c string_splitter.ml 
	ocamlc $(OCC) -c string_splitter.ml

slice:
	$(OC) $(OCOPT) -c slice.ml
	ocamlc $(OCC) -c slice.ml

glr_parser:
	$(OC) $(OCOPT) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_COMPILED) glr_parser.ml -o ../glr_parser
	ocamlc $(OCC) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_BYTECODE) -c glr_parser.ml
#	$(OC) $(OCOPT) $(OPENGL_DIR) $(OPENGL_LIB) glr_parser.ml -o ../glr_parser
#	ocamlc $(OCC) $(OPENGL_DIR) $(OPENGL_LIB_BYTECODE) glr_parser.ml

font_wrapper:
	$(OC) $(OCOPT) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_COMPILED) font_wrapper.ml -o ../font_wrapper
	ocamlc $(OCC) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_BYTECODE) -c font_wrapper.ml

clean:
	rm *.cmi  *.cmx *.cmo
