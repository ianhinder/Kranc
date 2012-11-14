thornscripts = $(wildcard *.m)

# thorns = ${thornscripts:.m=}

thorns = $(shell cat make.thorns)

kranc = ../Bin/kranc

all: $(thorns)

$(thorns): % : %.m
	$(kranc) $<
	@if [ -r tests/$@ ]; then ln -sfh ../tests/$@ $@/test; fi

clean:
	rm -rf $(thorns)
