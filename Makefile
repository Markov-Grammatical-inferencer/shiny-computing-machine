OC=ocamlopt
OCOPT=-annot -g
OCLIB=str.cmxa
all:  binary

test: filereader string_splitter slice
	ocaml tests.ml

binary: disambiguator filereader string_splitter

	$(OC) $(OCOPT) $(OCLIB)  -I .  slice.cmx disambiguator.cmx file_reader.cmx string_splitter.cmx  main.ml -o SCM

disambiguator: slice
	$(OC) $(OCOPT) -c disambiguator.ml 
	ocamlc $(OCOPT) -c disambiguator.ml

filereader:
	$(OC) $(OCOPT) -c file_reader.ml
	ocamlc $(OCOPT) -c file_reader.ml

string_splitter:
	$(OC) $(OCOPT) -c string_splitter.ml 
	ocamlc $(OCOPT) -c string_splitter.ml

slice:
	$(OC) $(OCOPT) -c slice.ml
	ocamlc $(OCOPT) -c slice.ml

clean:
	rm *.cmi *.cma *.cmx *.cmo
