all: insc_jvm insc_llvm
	echo "DONE"

insc_jvm: common
	cabal new-build insc_jvm
	cp dist-newstyle/build/*/*/instant-0.1.0.0/x/insc_jvm/build/insc_jvm/insc_jvm .

insc_llvm: common
	cabal new-build insc_llvm
	cp dist-newstyle/build/*/*/instant-0.1.0.0/x/insc_llvm/build/insc_llvm/insc_llvm .

insc_interpreter: common
	cabal new-build insc_interpreter
	cp dist-newstyle/build/*/*/instant-0.1.0.0/x/insc_interpreter/build/insc_interpreter/insc_interpreter .

common:
	cabal sandbox init
	cabal new-install

clean:
	rm -rf dist insc_jvm insc_llvm insc_interpreter
