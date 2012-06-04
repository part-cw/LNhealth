
all:
	@sh ../build.sh
	make -f makefile.payload
	@- rm makefile.payload
	
