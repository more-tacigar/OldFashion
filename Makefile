SETUP=ocaml setup.ml

build: setup.data
	$(SETUP) -build -use-menhir
setup.data:
	$(SETUP) -configure
