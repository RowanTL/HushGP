.PHONY: help run test clean format

help: # Show help for each of the commented Makefile recipes.
	@grep -E '^[a-zA-Z0-9 -]+:.*#'  Makefile | sort | while read -r l; \
	do printf "\033[1;32m$$(echo $$l | cut -f 1 -d':')\033[00m:$$(echo $$l | cut -f 2- -d'#')\n"; done

run: target/Main.out # Runs your compiled main code.
	./target/Main.out

target/Main.out: src/* app/*
	ghc -g -fprof-auto -prof -Wall app/*.hs src/*.hs -o target/Main.out
	@rm -f src/*.o src/*.hi

test: # Runs unit tests.
	runghc -i./src/ test/Main.hs

format: # Formats code using ormolu.
	ormolu --mode inplace app/*.hs src/HushGP/*.hs test/*.hs

hlint: # HLint for lint suggestions.
	hlint src/*.hs

stan: # Stan for more optimization suggestions.
	ghc -fwrite-ide-info app/*.hs src/*.hs -o target/temp.out
	stan --hiedir src/
	rm -f target/temp.out src/*.hi src/*.o src/*.hie app/*.o app/*.hi app/*.hie

clean: # Cleans up all the generated logfiles and outfiles.
	@rm -rf *.out *.o *.hi
	@rm -rf target/*
	@rm -rf */*.out */*.o */*.hi */*.hie
	@rm -rf */*/*.out */*/*.o */*/*.hi */*.hie
	@rm -rf dist-*
