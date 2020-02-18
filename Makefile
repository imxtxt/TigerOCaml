INCLUDES= util,x86,grading,ll
LIBS = unix,str,nums
SUBMIT := lexer.mll parser.mly frontend.ml studenttests.ml

HWNAME := hw04
TIMESTAMP := $(shell /bin/date "+%Y-%m-%d-%H:%M:%S")
ZIPNAME := $(HWNAME)-submit($(TIMESTAMP)).zip


all: main.native

.PHONY: test
test: main.native
	./main.native --test

.PHONY: main.native
main.native: 
	ocamlbuild -cflag -bin-annot -cflag -g -lflag -g -libs $(LIBS) main.native -use-menhir -yaccflag --explain

.PHONY: main.byte
main.byte: 
	ocamlbuild -cflag -bin-annot -cflag -g -lflag -g -libs $(LIBS) main.byte -use-menhir -yaccflag --explain

zip: $(SUBMIT)
	zip '$(ZIPNAME)' $(SUBMIT)

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -rf output a.out
