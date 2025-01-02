# Compiler and flags
OCAMLC = ocamlc
OCAMLYACC = ocamlyacc
OCAMLLEX = ocamllex
OCAMLFLAGS = -w A

# Source files
SOURCES = ast.ml parser.ml lexer.ml semantics.ml main.ml
INTERFACES = ast.mli parser.mli lexer.mli semantics.mli
GENERATED = parser.ml parser.mli lexer.ml
OBJECTS = ast.cmo parser.cmo lexer.cmo semantics.cmo main.cmo

# Executable name
EXEC = tpscript

# Default target
all: $(EXEC)

# Generate parser from mly
parser.ml parser.mli: parser.mly ast.cmi
	$(OCAMLYACC) parser.mly

# Generate lexer from mll
lexer.ml: lexer.mll
	$(OCAMLLEX) lexer.mll

# Compile interfaces
%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# Compile implementations
%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# Special case for generated files
parser.cmo: parser.ml parser.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c parser.ml

lexer.cmo: lexer.ml lexer.cmi parser.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c lexer.ml

# Dependencies
ast.cmi: ast.mli
	$(OCAMLC) $(OCAMLFLAGS) -c ast.mli

ast.cmo: ast.ml ast.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c ast.ml

parser.cmi: parser.mli ast.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c parser.mli

lexer.cmi: lexer.mli parser.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c lexer.mli

semantics.cmi: semantics.mli ast.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c semantics.mli

semantics.cmo: semantics.ml semantics.cmi ast.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c semantics.ml

main.cmo: main.ml ast.cmi parser.cmi lexer.cmi semantics.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c main.ml

# Link everything together
$(EXEC): ast.cmo parser.cmo lexer.cmo semantics.cmo main.cmo
	$(OCAMLC) $(OCAMLFLAGS) -o $(EXEC) $^

# Clean generated files
clean:
	rm -f *.cmo *.cmi $(GENERATED) $(EXEC)

# Don't remove intermediate files
.PRECIOUS: parser.ml parser.mli lexer.ml

# Phony targets
.PHONY: all clean test