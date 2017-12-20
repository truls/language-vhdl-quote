GHDL_VERSION=0.34
GHDL_ARCHIVE=v$(GHDL_VERSION).tar.gz

installghdl:ghdl/ghdl_mcode

$(GHDL_ARCHIVE):
	wget https://github.com/tgingold/ghdl/archive/$(GHDL_ARCHIVE)
	tar xzf $(GHDL_ARCHIVE)
	mv ghdl-$(GHDL_VERSION) ghdl

unpack:
	tar xJf suite.tar.xz

ghdl/ghdl_mcode: $(GHDL_ARCHIVE)
	cd ghdl; ./configure
	cd ghdl; make

clean:
	rm -r ghdl
	rm $(GHDL_ARCHIVE)
	rm -r vhdl-93-transformed
	rm -r vhdl-93

# TODO: Do this with makefiles instead
transform_files: unpack
	./transform_files.sh

test: transform_files ghdl/ghdl_mcode
	./testsuite.sh
