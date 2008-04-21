.phony: all clean

all:
	scons -C ../../m5/m5 CPU_MODELS=AtomicSimpleCPU `pwd`/m5/build/ALPHA_SE/m5.debug
	scons -C ../../m5/m5 CPU_MODELS=AtomicSimpleCPU `pwd`/m5/build/ALPHA_SE/m5.opt
	# Add some links for building.  libelf links are to make sure awb-resolver
	# always gets a unique path
	ln -fs ../libelf/libelf.a m5/build/ALPHA_SE/libelf.a
	rm -f m5/build/ALPHA_SE/include
	@echo Done

clean:
	@echo Cleaning
	rm -rf m5/build
