# Makefile to build all standalone programs.
#

SOURCES=ast.sml machine.sml rp.sml


%.lex.sml: %.lex
	mllex $<
all: rp
PHONY: all clean test
clean:
	rm -f *.lex.sml rp

rp: rp.lex.sml rp.mlb ${SOURCE}
	mlton rp.mlb

test: all
	${CURDIR}/rp test.inp
