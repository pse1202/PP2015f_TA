all: 2 3 4 5 6 7 

2: commonGrade.cmo hw6_2.cmo 
	ocaml commonGrade.cmo hw6_2.cmo hw6_2_selfgrader.ml 

3: commonGrade.cmo hw6_3.cmo 
	ocaml commonGrade.cmo hw6_3.cmo hw6_3_selfgrader.ml 

4: commonGrade.cmo hw6_4.cmo 
	ocaml commonGrade.cmo hw6_4.cmo hw6_4_selfgrader.ml 

5: commonGrade.cmo hw6_5.cmo 
	ocaml commonGrade.cmo hw6_5.cmo hw6_5_selfgrader.ml 

6: commonGrade.cmo hw6_6.cmo 
	ocaml commonGrade.cmo hw6_6.cmo hw6_6_selfgrader.ml 

7: commonGrade.cmo hw6_7.cmo 
	ocaml commonGrade.cmo hw6_7.cmo hw6_7_selfgrader.ml 

commonGrade.cmo: 
	ocamlc commonGrade.ml 

hw6_2.cmo: hw6_2.ml 
	ocamlc hw6_2.ml 

hw6_3.cmo: hw6_3.ml 
	ocamlc hw6_3.ml 

hw6_4.cmo: hw6_4.ml 
	ocamlc hw6_4.ml 

hw6_5.cmo: hw6_5.ml 
	ocamlc hw6_5.ml 

hw6_6.cmo: hw6_6.ml 
	ocamlc hw6_6.ml 

hw6_7.cmo: hw6_7.ml 
	ocamlc hw6_7.ml 

clean: 
	rm -f *.cmo *.exe *.cmi
