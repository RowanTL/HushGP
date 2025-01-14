code := $(wildcard src/*)
test_directories := $(wildcard tests/*)
test_targets := $(addsuffix /pass,$(test_directories))
existing_passes := $(wildcard tests/*/pass)
out_files := $(wildcard tests/*/*.out)
out_files += $(wildcard tests/*/out.*)

.PHONY: help build run test clean

help: # Show help for each of the commented Makefile recipes.
	@grep -E '^[a-zA-Z0-9 -]+:.*#'  Makefile | sort | while read -r l; \
	do printf "\033[1;32m$$(echo $$l | cut -f 1 -d':')\033[00m:$$(echo $$l | cut -f 2- -d'#')\n"; done

tests/%/pass: $(code)
	cd $(patsubst %/pass,%/,$@) && $(MAKE)

build: $(code) # Builds your code alone (does not build not any unit tests).
	cd src && $(MAKE)

run: build # Runs your compiled main code (does not pass any args or std-io).
	./target/main.out

test: $(test_targets) # Runs test code, generating score logfiles. To run past failing tests, use: `make test --keep-going`
	@#$(MAKE) grade

clean: # Cleans up all the generated logfiles and outfiles.
	@rm -f $(test_targets)
	@rm -f $(out_files)
	@rm -rf .mypy_cache __pycache__ *.out *.o *.hi .gdb_history
	@rm -rf build/*
	@rm -rf target/*
	@rm -rf .admin_files/.mypy_cache .admin_files/__pycache__ .admin_files/*.hi .admin_files/*.o
	@rm -rf */.mypy_cache */__pycache__ */*.out */*.o */*.hi
	@rm -rf */*/.mypy_cache */*/__pycache__ */*/*.out */*/*.o */*/*.hi */*/.gdb_history
