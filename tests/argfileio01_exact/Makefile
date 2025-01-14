pass: ../../src/*
	cd ../../src/ && $(MAKE)
	./../../target/main.out argfile.in argfile.out
	delta argfile.goal argfile.out && echo 100 >pass \
	|| ([[ "$$IS_PIPELINE" ]] && exit 1 \
	|| (echo "Press enter to debug:" && read && $(MAKE) debug && exit 1))

.PHONY: debug
debug:
	ghci
