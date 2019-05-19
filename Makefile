#
# Makefile
#

SRC= sqlsyntax.ml sqlparser.ml sqllexer.ml
COMPONENT= sqlsyntax.ml sqlparser.mli sqlparser.ml sqllexer.ml sqlcompile.ml
#COMPONENT= sqlsyntax.ml sqlparser.mli sqlparser.ml sqllexer.ml
TARGET= sql
STRCMA= str.cmxa

all:	$(TARGET)

$(TARGET): 	$(COMPONENT) 
	ocamlmktop $(COMPONENT) -o $(TARGET)
#	ocamlc $(COMPONENT) -o $(TARGET)

sqlparser.mli:	sqlparser.mly
	ocamlyacc sqlparser.mly

sqlparser.ml:	sqlparser.mly
	ocamlyacc sqlparser.mly

sqllexer.ml:	sqllexer.mll
	ocamllex sqllexer.mll


backup:
	/bin/cp -f Makefile $(SRC) back

clean:
	/bin/rm -f sqlparser.ml sqlparser.mli sqllexer.ml  $(TARGET) *.cmi *.cmo *.mli

