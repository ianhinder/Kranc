thorns = $(shell cat make.thorns)
scriptthorns = $(shell cat make.scriptthorns)

kranc = ../Bin/kranc

all: $(thorns) $(scriptthorns)

$(thorns): % : %.m
	$(kranc) $<
	@if [ -r tests/$@ ]; then ln -sfh ../tests/$@ $@/test; fi

$(scriptthorns): % : %.kranc
	$(kranc) $<
	@if [ -r tests/$@ ]; then ln -sfh ../tests/$@ $@/test; fi

clean:
	rm -rf $(thorns) $(scriptthorns)
