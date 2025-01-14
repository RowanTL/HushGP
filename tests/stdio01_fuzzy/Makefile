pass: ../../src/*
	cd ../../src/ && $(MAKE)
	./../../target/main.out <std.in >std.out
	python3 ../../.admin_files/fuzzydiffer.py std.goal std.out >pass \
	|| ([[ "$$IS_PIPELINE" ]] && exit 1 \
	|| (echo -e "$$(delta std.goal std.out)\nPress enter to debug (type std.in yourself):" && read && $(MAKE) debug && exit 1))

.PHONY: debug
debug:
	ghci
