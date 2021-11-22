# Makefile for the XML Jargon File

# How to prepare a release:
#
#  1. Run patch-sizes on a write-enabled copy of jargon.xml 
#     Add a release explanation to the log.  Run patch-sizes again.
#  2. Check the pagemaker file to see if the announcement string needs to 
#     be updated.
#  3. Do `make'.  This will recreate all distributed formats
#     and a tarball.  Watch for cross-reference errors turned up by HTML
#     generation.
#  4. If the preceding steps reveal problems, fix and return to step 1.
#  5. RCS-commit all the masters and put a snapshot label on them.
#  6. Upload (by running `make release').
#  7. Bump the version number in jargon.xml.

VERSION=$(shell getversion)

# Distribution files
HTMLFILES = index.html jargbook.html alternates.html download.html \
		jargtxt.html awards.html quoting.html mirroring.html \
		mirrorlist.html submissions.html related.html news.html \
		jargoogle.html
DISTFILES = html graphics

MISC      = READ.ME Makefile 
XSLFILES  = jargon.xsl jargon-chunk.xsl jargon-chunk-base.xsl jargon-text.xsl
LEXIFILES = jargon.xml chaff.xml
ELISP     = jargon-mode.el
OLDFILES  = oldversions/{README,jarg*.txt,Steele-words,Makefile,\
		sizereport,diffreport,fixcase,getwords,xmlconvert.el}
TOOLS     = replace overwrite pagemaker nbreak.c pagemaker getversion \
		name-extract wraptext
XSLDIR    = docbook-xsl-1.62.4
SOURCES   = $(HTMLFILES) graphics $(MISC) $(XSLFILES) $(LEXIFILES) \
		$(ELISP) $(XSLDIR)

all: html/index.html chaff.html jargon.txt jargon-$(VERSION).tar.gz jargsrc.tar.gz
	pagemaker

release: all
	cp jargon.txt oldversions/jarg`echo $(VERSION) | tr -d '.'`.txt
	upload $(HTMLFILES) chaff.html changes.html

html/index.html: jargon-web.xml $(XSLFILES)
	if [ ! -d html ]; then mkdir html; fi; cp -f jargon.css html;
	if [ ! -d html/graphics ]; then cp -r graphics html; fi
	time xmlto -o html -x jargon-chunk.xsl xhtml jargon-web.xml
	# These kluges should be replaced by stylesheet stuff
	./replace 'jargon.css' '../jargon.css' html/*/*
	./replace 'src="graphics' 'src="../graphics' html/*/*

# One big file.  Not presently shipped, as the internationalization is hosed.
jargon.html: jargon-web.xml $(XSLFILES)
	xmlto -m jargon.xsl html-nochunks jargon-web.xml
	mv jargon-web.html jargon.html

jargon.txt: jargon-web.xml $(XSLFILES)
	xmlto -p "-width=79" -m jargon-text.xsl txt jargon-web.xml
	wraptext jargon-web.txt >jargon.txt
	rm -f jargon-web.html

# Make viewable XHTML from the chaff file
chaff.html: chaff.xml chaff.xsl
	xmlto html-nochunks -x chaff.xsl chaff.xml

# Get an entries/lines/words/chars count,
# Don't mess casually, jargon-mode.el parses the output from this.
size: jargon.txt
	@grep "^<glossentry" jargon.xml | wc -l
	@wc jargon.txt 2>/dev/null

# Conditionalize the master for Web output
jargon-web.xml: names.xml jargon.xml
	xmlif condition='web' <jargon.xml >jargon-web.xml

# Conditionalize the master file for book condition
jargon-book.xml: jargon.xml
	xmlif condition="book" <jargon.xml >jargon-book.xml

# This is needed for the book version
names.xml: jargon.xml
	name-extract <jargon.xml >names.xml

# Make a tools distribution for people to hack on
jargsrc.tar.gz: $(SOURCES) jargon.css BUGS $(XSL)
	tar -h -czf jargsrc.tar.gz $(SOURCES) jargon.css BUGS $(XSL)

jargon-$(VERSION).tar.gz: $(DISTFILES) jargon.css
	@echo $(DISTFILES) jargon.css | tr ' ' "\012" | sed s:^:jargon-$(VERSION)/: >MANIFEST
	@(cd ..; ln -s jargon jargon-$(VERSION))
	(cd ..;tar -czf jargon/jargon-$(VERSION).tar.gz `cat jargon/MANIFEST`)
	@(cd ..; rm jargon-$(VERSION))
	ls -l jargon-$(VERSION).tar.gz

clean:
	rm -f jargon-book.xml jargon-web.xml names.xml 
	rm -fr html/* jargon-web.html chaff.html jargon.txt
	rm -f jargon-$(VERSION).tar.gz jargsrc.tar.gz 
