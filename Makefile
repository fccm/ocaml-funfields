all: all
%::
	$(MAKE) -C src $@
.PHONY: test
test:
	$(MAKE) -C test $@
