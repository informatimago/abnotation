all:help

NOTES=notes
STYLES=rst/freetype-sans.json

help:
	@echo "make documentation"

clean: 
	-rm -f $(NOTES).html $(NOTES).pdf

documentation:$(NOTES).html $(NOTES).pdf

%.html:%.txt Makefile
	LC_TYPE=en_US.utf-8 rst2html             $< $@
%.pdf:%.txt Makefile $(STYLES:,= )
	LC_TYPE=en_US.utf-8 rst2pdf -s $(STYLES) $< -o $@


