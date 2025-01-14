pass: ../../src/*.hs
	ghc -fwrite-ide-info $^ -o main.out
	stan --hiedir ../../src/ | grep "Total found observations     ┃ 0" && echo 100 >pass \
	|| ([[ "$$IS_PIPELINE" ]] && exit 1 \
	|| (echo -e "\nPress enter to debug:" && read && $(MAKE) debug && exit 1))
	@rm -f ../../src/*.hie ../../src/*.hi ../../src/*.o main.out

.PHONY: debug
debug:
	ghc -fwrite-ide-info ../../src/*.hs -o main.out
	stan --hiedir ../../src/
	@rm -f ../../src/*.hie ../../src/*.hi ../../src/*.o main.out
	@echo "To get points, fix the above 'observations'."
