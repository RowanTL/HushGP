pass: ../../src/*
	cd ../../src/ && $(MAKE)
	./../../target/main.out <std.in >std.out
	delta std.goal std.out && echo 100 >pass \
	|| ([[ "$$IS_PIPELINE" ]] && exit 1 \
	|| (echo "Press enter to debug (type std.in in yourself):" && read && $(MAKE) debug && exit 1))

.PHONY: debug
debug:
	ghci
