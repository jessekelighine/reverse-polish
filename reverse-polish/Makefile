.PHONY: all clean

all: www/body.html

www/body.html: www/body.md www/style.css
	pandoc www/body.md \
		--mathjax \
		--output www/body.html

clean:
	rm -rf www/*.html
