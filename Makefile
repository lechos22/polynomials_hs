MAIN = ./target/main

all: ${MAIN}
run: run(${MAIN})
run(%): %
	$%
clean: clean(${MAIN})
clean(%):
	rm -rf $% $%.o $%.hi

./target/%: %.hs
	mkdir -p target
	ghc $^ -o $@

