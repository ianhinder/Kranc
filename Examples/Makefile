thornscripts = $(wildcard *.m)
thorns = ${thornscripts:.m=}

kranc = ../Bin/kranc

all: $(thorns)

$(thorns): % : %.m
	$(kranc) $<
	@if [ -r tests/$@ ]; then ln -sfh ../tests/$@ $@/test; fi

clean:
	rm -rf $(thorns)
