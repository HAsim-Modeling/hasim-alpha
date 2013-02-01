.phony: all clean

all:
	scons -C ../../m5/m5 CPU_MODELS=AtomicSimpleCPU `pwd`/m5/build/ALPHA/m5.debug --no-colors
	scons -C ../../m5/m5 CPU_MODELS=AtomicSimpleCPU `pwd`/m5/build/ALPHA/m5.opt --no-colors
	# Add some links for building.  libelf links are to make sure awb-resolver
	# always gets a unique path
	ln -fs ../libelf/libelf.a m5/build/ALPHA/libelf.a
	ln -fs ../gzstream/libgzstream.a m5/build/ALPHA/libgzstream.a
	rm -f m5/build/ALPHA/include
	@echo Done

clean:
	@echo Cleaning
	rm -rf m5/build
