
.PHONY: all

all: tester
	ghc -o sat-solver app/solver/sat-solver.hs lib/*.hs

tester: Tester.hs
	ghc -o tester Tester.hs

clean:
	stack clean
	rm -f sat-solver tester *.hi *.o
