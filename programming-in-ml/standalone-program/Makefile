PHONY: smlnj-test mlton-test test clean

test: smlnj-test mlton-test
smlnj-test: hello.sml hello.cm
	ml-build hello.cm Hello.main hello-image
	sml @SMLload hello-image.* foo bar biz

mlton-test: hello
	./hello foo bar biz

hello: hello.mlb hello.sml main.sml
	mlton hello.mlb
clean:
	rm -f hello hello-image.*
