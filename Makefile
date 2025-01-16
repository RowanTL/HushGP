.PHONY: help run test clean format

help: # Show help for each of the commented Makefile recipes.
	@grep -E '^[a-zA-Z0-9 -]+:.*#'  Makefile | sort | while read -r l; \
	do printf "\033[1;32m$$(echo $$l | cut -f 1 -d':')\033[00m:$$(echo $$l | cut -f 2- -d'#')\n"; done

run: target/Main.out # Runs your compiled main code.
	./target/Main.out

target/Main.out: src/*
	ghc -g -fprof-auto -prof -Wall src/*.hs -o target/Main.out
	@rm -f src/*.o src/*.hi

test: tests/*.hs # Runs unit tests.
	runghc -i./src/ tests/Main.hs

format: src/* # Formats code using ormolu.
	ormolu --mode inplace src/*.hs tests/*.hs

hlint: src/*.hs # HLint for lint suggestions.
	hlint src/*.hs

stan: src/*.hs # Stan for more optimization suggestions.
	ghc -fwrite-ide-info src/*.hs -o target/temp.out
	stan --hiedir src/
	rm -f target/temp.out src/*.hi src/*.o src/*.hie

clean: # Cleans up all the generated logfiles and outfiles.
	@rm -rf *.out *.o *.hi
	@rm -rf target/*
	@rm -rf */*.out */*.o */*.hi
	@rm -rf */*/*.out */*/*.o */*/*.hi
