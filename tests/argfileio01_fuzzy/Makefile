pass: ../../src/*
	cd ../../src/ && $(MAKE)
	./../../target/main.out argfile.in argfile.out
	python3 ../../.admin_files/fuzzydiffer.py argfile.goal argfile.out >pass \
	|| ([[ "$$IS_PIPELINE" ]] && exit 1 \
	|| (echo -e "$$(delta argfile.goal argfile.out)\nPress enter to debug:" && read && $(MAKE) debug && exit 1))

.PHONY: debug
debug:
	ghci
