all:
	rm -fr *.js* *.o *.hi
	ghcjs Main.hs
	cp test.html Main.jsexe/index.html

stack:
	rm -fr *.js* *.o *.hi
	stack exec -- ghcjs Main.hs
	cp test.html Main.jsexe/index.html
