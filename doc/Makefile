all: README.html ANALYSIS.html FAQ.html

clean:
	-$(RM) *.html

%.html: %.mkd
	markdown $^ > $@
