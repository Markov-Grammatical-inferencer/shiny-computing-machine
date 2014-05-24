OC=ocamlc
OCOPT=-annot
OCLIB=str.cma
all: disambiguator filereader string_splitter binary

binary:
	$(OC) $(OCOPT) $(OCLIB) disambiguator.cmo file_reader.cmo string_splitter.cmo

disambiguator:
	$(OC) $(OCOPT) -c disambiguator.ml 

filereader:
	$(OC) $(OCOPT) -c file_reader.ml

string_splitter:
	$(OC) $(OCOPT) -c string_splitter.ml 
