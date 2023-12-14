.PHONY: all clean

all: operators.html

operators.html: operators.md
	pandoc operators.md -o operators.html

clean: 
	rm -rf *.html
