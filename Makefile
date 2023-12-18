.PHONY: all clean shinylive shinyapps

all:
	# Specify 'shinylive' or 'shinyapps'

clean:
	rm -rf docs

shinylive: reverse-polish/ makeapp.R
	cd reverse-polish; make ; cd ..
	Rscript makeapp.R $@

shinyapps: reverse-polish/ makeapp.R
	cd reverse-polish; make ; cd ..
	Rscript makeapp.R $@
