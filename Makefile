# Compiler and flags
OCAMLC = ocamlc
OCAMLYACC = ocamlyacc
OCAMLLEX = ocamllex
OCAMLFLAGS = -w A

# Source files
SOURCES = ast.ml parser.ml lexer.ml semantics.ml main.ml
INTERFACES = ast.mli parser.mli lexer.mli
GENERATED = parser.ml parser.mli lexer.ml
OBJECTS = ast.cmo parser.cmo lexer.cmo semantics.cmo main.cmo

# Executable name
EXEC = tpscript

# Default target
all: $(EXEC)

# Generate parser from mly
parser.ml parser.mli: parser.mly
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
ast.cmo: ast.ml ast.cmi
parser.cmi: parser.mli ast.cmi
parser.cmo: parser.ml parser.cmi ast.cmi
lexer.cmi: lexer.mli parser.cmi
lexer.cmo: lexer.ml lexer.cmi parser.cmi
semantics.cmo: semantics.ml ast.cmi
main.cmo: main.ml ast.cmi parser.cmi lexer.cmi semantics.cmi

# Link everything together
$(EXEC): $(OBJECTS)
	$(OCAMLC) $(OCAMLFLAGS) -o $(EXEC) $(OBJECTS)

# Test target
test: $(EXEC)
	@echo "Running tests..."
	@for f in tests/*.tps; do \
		if [ -f "$$f" ]; then \
			echo "\nTesting $$f:"; \
			./$(EXEC) < "$$f"; \
		fi \
	done

# Clean generated files
clean:
	rm -f *.cmo *.cmi $(GENERATED) $(EXEC)

# Don't remove intermediate files
.PRECIOUS: parser.ml parser.mli lexer.ml

# Phony targets
.PHONY: all clean test