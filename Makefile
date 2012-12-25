DIRS = src examples

compile:
	for i in $(DIRS); do make -C $$i; done
