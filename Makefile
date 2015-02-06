# Automatically generated. Do not edit
current_dir = $(shell pwd)

all:
	@cd ../lambdanative; SYS_PATH=$(current_dir) make; cd $(current_dir)

clean:
	@cd ../lambdanative; SYS_PATH=$(current_dir) make clean; cd $(current_dir)

scrub:
	@cd ../lambdanative; SYS_PATH=$(current_dir) make scrub; cd $(current_dir)

install:
	@cd ../lambdanative; SYS_PATH=$(current_dir) make install; cd $(current_dir)

#eof
