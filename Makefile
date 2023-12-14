.PHONY: all clean

all: docs

clean:
	rm -rf docs

docs: myapp makeapp.R
	Rscript makeapp.R
