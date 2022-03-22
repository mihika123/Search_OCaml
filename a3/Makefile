MODULES=dictionary dictionarySet listDictionary treeDictionary engine \
	listEngine treeEngine authors
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind \
	-plugin-tag 'package(bisect_ppx-ocamlbuild)'
PKGS=unix,ounit2,str,qcheck

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

bisect-test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TEST) \
		&& ./$(TEST)

check:
	@bash check.sh
	
finalcheck:
	@bash check.sh final

bisect: clean bisect-test
	bisect-ppx-report html

zip:
	zip -r search.zip *.ml* engine_test *.sh \
		_tags .merlin .ocamlformat .ocamlinit LICENSE Makefile 

docs: docs-public docs-private

docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf search.zip _doc.public _doc.private _coverage bisect*.coverage
