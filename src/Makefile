OC=ocamlopt
OCOPT=-annot -g -p
OCC=-annot -g
OCLIB=str.cmxa
GRAPHICS_DIRS=-I +lablgl -I +camlimages
GRAPHICS_LIBS_COMPILED=lablgl.cmxa lablglut.cmxa graphics.cmxa camlimages_core.cmxa camlimages_freetype.cmxa 
GRAPHICS_LIBS_BYTECODE=lablgl.cma lablglut.cma graphics.cma camlimages_core.cma camlimages_freetype.cma 

all: ../SCM ../glr_parser ../font_wrapper

test: filereader.cmo string_splitter.cmo slice.cmo
	ocaml tests.ml


../SCM: disambiguator.cmx file_reader.cmx string_splitter.cmx main.ml
	$(OC) $(OCOPT) $(OCLIB)  -I .  slice.cmx disambiguator.cmx file_reader.cmx string_splitter.cmx main.ml -o ../SCM

disambiguator.cmx: slice.cmx disambiguator.ml
	$(OC) $(OCOPT) -c disambiguator.ml
disambiguator.cmo: slice.cmo disambiguator.ml
	ocamlc $(OCC) -c disambiguator.ml

filereader.cmx: file_reader.ml
	$(OC) $(OCOPT) -c file_reader.ml
filereader.cmo: file_reader.ml
	ocamlc $(OCC) -c file_reader.ml

string_splitter.cmx: string_splitter.ml
	$(OC) $(OCOPT) -c string_splitter.ml 
string_splitter.cmo: string_splitter.ml
	ocamlc $(OCC) -c string_splitter.ml

slice.cmx: slice.ml
	$(OC) $(OCOPT) -c slice.ml
slice.cmo: slice.ml
	ocamlc $(OCC) -c slice.ml

../glr_parser: glr_parser.ml
	$(OC) $(OCOPT) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_COMPILED) glr_parser.ml -o ../glr_parser
	ocamlc $(OCC) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_BYTECODE) -c glr_parser.ml

../font_wrapper: font_wrapper.ml
	$(OC) $(OCOPT) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_COMPILED) font_wrapper.ml -o ../font_wrapper
	ocamlc $(OCC) $(GRAPHICS_DIRS) $(GRAPHICS_LIBS_BYTECODE) -c font_wrapper.ml

clean:
	rm *.cmi  *.cmx *.cmo *.annot *.o
