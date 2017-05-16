all: parser.js 

CalcSyntax.cmo: CalcSyntax.ml
	ocamlfind ocamlc -package js_of_ocaml.ppx -package js_of_ocaml -o $@ -c $<

CalcParser.cmo: CalcParser.ml
	ocamlfind ocamlc -pp camlp4o -package js_of_ocaml.ppx -package js_of_ocaml -o $@ -c $<

CalcSyntaxVisitor.cmo: CalcSyntaxVisitor.ml CalcParser.cmo
	ocamlfind ocamlc -package js_of_ocaml.ppx -package js_of_ocaml -o $@ -c $<

CalcInterpreter.cmo: CalcInterpreter.ml CalcSyntaxVisitor.cmo
	ocamlfind ocamlc -package js_of_ocaml.ppx -package js_of_ocaml -o $@ -c $<

parser.byte: CalcSyntax.cmo CalcParser.cmo CalcSyntaxVisitor.cmo
	ocamlfind ocamlc -package js_of_ocaml.ppx -package js_of_ocaml -linkpkg -o $@ $^

%.unlog.js: %.ml ../../generator/main.byte ../../generator/monad_ppx.byte
	../../generator/main.byte -mode unlog -ppx ../../generator/monad_ppx.byte -I ../../generator/stdlib_ml $<

CalcInterpreter.log.js: CalcInterpreter.ml ../../generator/main.byte ../../generator/monad_ppx.byte
	../../generator/main.byte -mode log -ppx ../../generator/monad_ppx.byte -I ../../generator/stdlib_ml $<

parser.js: parser.byte CalcSyntax.unlog.js CalcInterpreter.unlog.js CalcInterpreter.log.js
	js_of_ocaml parser.byte

calc: CalcSyntax.cmo CalcParser.cmo CalcInterpreter.cmo CalcMain.ml
	ocamlfind ocamlc -o $@ $^

clean:
	rm *.cmo
	rm *.cmi
	# rm parser.map
	rm parser.byte
	rm *.js
	# rm *.o
#	rm *.cmx
