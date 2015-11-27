all: 2 

2: commonGrade.cmo hw7_2.cmo 
	ocaml commonGrade.cmo hw7_2.cmo hw7_2_selfgrader.ml 

	ocamlc commonGrade.ml 
commonGrade.cmo: 
	ocamlc commonGrade.ml 

hw7_2.cmo: hw7_2.ml 
	ocamlc hw7_2.ml

clean: 
	rm -f *.cmo *.exe *.cmi
