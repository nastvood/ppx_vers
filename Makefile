.PHONY: ppx_vers

ppx_vers: src/ppx_vers.ml
	dune build src/ppx_vers.a 

test_run: 
ifeq (, $(shell which ocamlfind))
	 $(error "could not find ocamlfind, consider 'opam install ocamlfind'")
endif
	@ dune build test/test_run.exe
	@ _build/default/test/test_run.exe

clean:
	dune clean	
