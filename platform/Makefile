.phony: all clean

all:
	scons -C ../../m5/m5 CPU_MODELS=AtomicSimpleCPU `pwd`/m5/build/ALPHA_SE/m5.debug
	scons -C ../../m5/m5 CPU_MODELS=AtomicSimpleCPU `pwd`/m5/build/ALPHA_SE/m5.opt
	@echo Done

clean:
	@echo Cleaning
	rm -rf m5/build
