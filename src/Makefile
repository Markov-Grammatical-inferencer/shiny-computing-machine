OC=ocamlopt
OCOPT=-annot -g -p
OCC=-annot -g
OCLIB=str.cmxa
all: binary

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

clean:
	rm *.cmi  *.cmx *.cmo
