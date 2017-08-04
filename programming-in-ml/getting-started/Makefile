PHONY: test clean

test: tutorial.sml tutorial.cm
	ml-build tutorial.cm Tutorial.main tutorial-image
	sml @SMLload tutorial-image.* foo bar biz
clean:
	rm -f tutorial-image.*
