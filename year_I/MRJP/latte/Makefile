all: latc_llvm
	echo "DONE"

latc_llvm: common
	cabal new-build latc_llvm
	cp dist-newstyle/build/*/*/latte-0.1.0.0/x/latc_llvm/build/latc_llvm/latc_llvm .

common:
	cabal sandbox init
	cabal new-install

clean:
	rm -rf dist latc_llvm
