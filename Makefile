OBJS = ast.cmo table.cmo sast.cmo parser.cmo scanner.cmo compile.cmo marmalade.cmo

TESTS = \

# Choose one
YACC = ocamlyacc
#YACC = menhir --explain

marmalade : $(OBJS)
	ocamlc -o marmalade $(OBJS)

# 	.PHONY : test
# 	test : microc testall.sh
# 		./testall.sh

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	$(YACC) parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

.PHONY : clean
clean :
	rm -f marmalade parser.ml parser.mli scanner.ml \
		*.cmo *.cmi *.out *.diff


ast.cmo: 
ast.cmx: 

sast.cmo: ast.cmo
sast.cmx: ast.cmx

compile.cmo: ast.cmo 
compile.cmx: bytecode.cmx ast.cmx 
marmalade.cmo: scanner.cmo parser.cmi compile.cmo 
marmalade.cmx: scanner.cmx parser.cmx compile.cmx
parser.cmo: ast.cmo parser.cmi 
parser.cmx: ast.cmx parser.cmi 
scanner.cmo: parser.cmi 
scanner.cmx: parser.cmx 
parser.cmi: ast.cmo 
