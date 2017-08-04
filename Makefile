SOURCE_DIRS=programming-in-ml/standalone-program \
	  programming-in-ml/getting-started    \
	  reverse-polish


.PHONY: test clean

test:
	$(foreach dir, ${SOURCE_DIRS}, make -C ${dir} test; )
clean:
	$(foreach dir, ${SOURCE_DIRS}, make -C ${dir} test; )
