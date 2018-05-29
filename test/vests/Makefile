#GHDL_VERSION=0.35
#GHDL_VERSION=master
#GHDL_ARCHIVE=v$(GHDL_VERSION).tar.gz
GHDL_ARCHIVE=ghdl

installghdl:ghdl/ghdl_mcode

# $(GHDL_ARCHIVE):
# 	wget https://github.com/tgingold/ghdl/archive/	$(GHDL_ARCHIVE)
# 	tar xzf $(GHDL_ARCHIVE)
# 	mv ghdl-$(GHDL_VERSION) ghdl

$(GHDL_ARCHIVE):
	git clone https://github.com/truls/ghdl


vhdl-93: suite.tar.xz
	tar xJf suite.tar.xz

ghdl/ghdl_mcode: $(GHDL_ARCHIVE)
	cd ghdl; ./configure
	cd ghdl; make

clean:
	rm -rf ghdl
#	rm $(GHDL_ARCHIVE)
	rm -r vhdl-93-transformed
	rm -r vhdl-93

# TODO: Do this with makefiles instead
transform_files: vhdl-93
	./transform_files.sh

test: transform_files ghdl/ghdl_mcode
	./testsuite.sh
