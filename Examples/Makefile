thornscripts = $(wildcard *.m)
thorns = ${thornscripts:.m=}

kranc = ../Bin/kranc

all: $(thorns)

$(thorns): % : %.m
	$(kranc) $<

clean:
	rm -rf $(thorns)
