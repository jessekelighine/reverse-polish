.PHONY: all clean

all: www/body.html

www/body.html: www/body.md
	pandoc www/body.md -o www/body.html

clean: 
	rm -rf www/*.html
