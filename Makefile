# Variable
CC=ocamlopt


# Cible
TARGET = projet
OBJ = main.cmx assert.cmx

# Compilation
%.cmx: %.ml
	$(CC) -c $<

$(TARGET): $(OBJ)
	$(CC) -o $@ $^


.PHONY: clean

clean:
	rm -f *.cmx
	rm -f *.cmi
	rm -f *.o