all:
	rm -fr *.js* *.o *.hi
	ghcjs Main.hs
	cp test.html Main.jsexe/index.html

stack:
	rm -fr *.js* *.o *.hi
	STACK_YAML=../stack.yaml stack exec -- ghcjs Main.hs
	cp test.html Main.jsexe/index.html
