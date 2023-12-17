.PHONY: all clean

all: docs

clean:
	rm -rf docs

docs: reverse-polish/ makeapp.R
	Rscript makeapp.R
