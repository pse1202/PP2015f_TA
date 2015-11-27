all: 1 2 3 4

1: commonGrade.cmo hw8_1.cmo 
	ocaml commonGrade.cmo hw8_1.cmo hw8_1_selfgrader.ml 

	ocamlc commonGrade.ml 


2: commonGrade.cmo hw8_2.cmo 
	ocaml commonGrade.cmo hw8_2.cmo hw8_2_selfgrader.ml 

	ocamlc commonGrade.ml 

3: commonGrade.cmo hw8_3.cmo 
	ocaml commonGrade.cmo hw8_3.cmo hw8_3_selfgrader.ml 

	ocamlc commonGrade.ml 

4: commonGrade.cmo hw8_4.cmo 
	ocaml commonGrade.cmo hw8_4.cmo hw8_4_selfgrader.ml 

	ocamlc commonGrade.ml 

commonGrade.cmo: 
	ocamlc commonGrade.ml 

hw8_1.cmo: hw8_1.ml 
	ocamlc hw8_1.ml 

hw8_2.cmo: hw8_2.ml 
	ocamlc hw8_2.ml

hw8_3.cmo: hw8_3.ml 
	ocamlc hw8_3.ml 

hw8_4.cmo: hw8_4.ml 
	ocamlc hw8_4.ml 

clean: 
	rm -f *.cmo *.exe *.cmi
