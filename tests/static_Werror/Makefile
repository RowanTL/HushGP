pass: ../../src/*.hs
	ghc -Wall -Werror $^ -o main.out && echo 100 >pass || (rm main.out && exit 1)
	@rm main.out
