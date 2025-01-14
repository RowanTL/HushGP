pass: ../../src/*
	ghc -g -fprof-auto -prof -Wall unit.hs ../../src/BinaryConversions.hs -o unit.out
	@rm -f *.o *.hi
	./unit.out | grep "OK" && echo 100 >pass \
	|| ([[ "$$IS_PIPELINE" ]] && exit 1 \
	|| (echo "Press enter to debug:" && read && $(MAKE) debug && exit 1))

.PHONY: debug
debug:
	ghci
