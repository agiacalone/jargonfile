# Make and analyze wordlists from old versions of the Jargon File.

# Missing versions: 212, 213, 214, 29[1-57-9]
# This is the only master list of versions.  All the tools are driven by it.
VERSIONS=110 120 130 140 150 \
	211 215 221 231 241 243 244 251 261 262 263 271 281 282 283 296 \
	2910 2911 2912 \
	300 310 320 330 331 332 333 \
	400 410 411 412 413 414 415 420 421 422 423 431 432 433

WORDLISTS=$(shell for n in $(VERSIONS); do echo wordlist$${n}; done)

all: $(WORDLISTS)
	@diffreport -l $(VERSIONS)

wordlist110: jarg1-81-MM-DD.txt
	getwords -g -n jarg1-81-MM-DD.txt | fixcase >wordlist110
wordlist120: jarg1-81-MM-DD.txt
	getwords -g -n jarg1-82-11-14.txt | fixcase >wordlist120
wordlist130: jarg1-87-01-25.txt
	getwords -g -n jarg1-87-01-25.txt | fixcase >wordlist130
wordlist140: jarg1-88-10-04.txt
	getwords -g -n jarg1-88-10-04.txt | fixcase >wordlist140
wordlist150: Steele-1983
	# This is actually the list of words I had marked "Steele-1983"
	# in my TeX masters.  Not all of thsse were headwords in Steele-1983,
	# but they first occurred there. 
	cp Steele-words wordlist150
wordlist211: jarg211.txt
	getwords -g -h "THE JARGON ITSELF" jarg211.txt | fixcase >wordlist211
wordlist215: jarg215.txt
	getwords -g -h "THE JARGON ITSELF" -a "APPENDIX C" \
		-s "BONDAGE-AND-DISCIPLINE LANGUAGE A/BONDAGE-AND-DISCIPLINE LANGUAGE" \
		-s "CHURCH OF THE SUB-GENIUS A/Church of the Sub-Genius" \
		jarg215.txt | fixcase >wordlist215
wordlist221: jarg221.txt
	getwords -g -h "The Jargon (Low Moby, Current Terminology)" \
		-a "Appendix A" -a "Appendix C" -i "V" \
		-s "BONDAGE-AND-DISCIPLINE LANGUAGE A/BONDAGE-AND-DISCIPLINE LANGUAGE" \
		jarg221.txt | fixcase >wordlist221
# Keywords stopped being caseblind here, and keywords got wrapped in angle
# brackets.  There were some errors in case handling during the transition.
# We keep filtering with fixcase angway because topic entries remained in caps.
wordlist231: jarg231.txt
	sed <jarg231.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/^Appendix A: Hacker Folklore/q" \
		-e "/^<\([^>][^>]*\)>.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist231
wordlist241: jarg241.txt
	sed <jarg241.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/^<\([^>][^>]*\)>.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist241
wordlist243: jarg243.txt
	sed <jarg243.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/^<\([^>][^>]*\)>.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist243
wordlist244: jarg244.txt
	sed <jarg244.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/^<\([^>][^>]*\)>.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist244
# For some inexplicable reason, I took the angle brackets off the
# topic keywords in this one.
wordlist251: jarg251.txt
	sed <jarg251.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/Hacker Folklore/q" \
		-e "/\(^[A-Z][A-Z][A-Z ,-]*\) [a-z].*/s//\1/p" \
		-e "/^<\([^>][^>]*\)>.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist251
# Headword delimiters went from angle brackets to colons,
# and topic entries stopped being all-caps.
wordlist261: jarg261.txt
	sed <jarg261.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/Hacker Folklore/q" \
		-e "/^ /d" \
		-e "/^\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist261
wordlist262: jarg262.txt
	sed <jarg262.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/Hacker Folklore/q" \
		-e "/^ /d" \
		-e "/^\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist262
wordlist263: jarg263.txt
	sed <jarg263.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/Hacker Folklore/q" \
		-e "/^ /d" \
		-e "/^\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist263
wordlist271: jarg271.txt
	sed <jarg271.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/Hacker Folklore/q" \
		-e "/^ /d" \
		-e "/^\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist271
# Change logs were complete from about here
wordlist281: jarg281.txt
	sed <jarg281.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/Hacker Folklore/q" \
		-e "/^ /d" \
		-e "/^\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist281
wordlist282: jarg282.txt
	sed <jarg282.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/Hacker Folklore/q" \
		-e "/^ /d" \
		-e "/^\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist282
wordlist283: jarg283.txt
	sed <jarg283.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/Hacker Folklore/q" \
		-e "/^ /d" \
		-e "/^\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist283
wordlist296: jarg296.txt
	sed <jarg296.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/Hacker Folklore/q" \
		-e "/^ /d" \
		-e "/^\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist296
wordlist2910: jarg2910.txt
	sed <jarg2910.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^ /d" \
		-e "/^:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist2910
wordlist2911: jarg2911.txt
	sed <jarg2911.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^ /d" \
		-e "/^:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist2911
wordlist2912: jarg2912.txt
	sed <jarg2912.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^ /d" \
		-e "/^:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist2912
wordlist300: jarg300.txt
	sed <jarg300.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^ /d" \
		-e "/^:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist300
wordlist310: jarg310.txt
	sed <jarg310.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^ /d" \
		-e "/^:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist310
wordlist320: jarg320.txt
	sed <jarg320.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^ /d" \
		-e "/^:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist320
wordlist330: jarg330.txt
	sed <jarg330.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^ /d" \
		-e "/^:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist330
wordlist331: jarg331.txt
	sed <jarg331.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^ /d" \
		-e "/^:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist331
wordlist332: jarg332.txt
	sed <jarg332.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^ /d" \
		-e "/^:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist332
wordlist333: jarg333.txt
	sed <jarg333.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^ /d" \
		-e "/^:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist333
wordlist400: jarg400.txt
	sed <jarg400.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^ /d" \
		-e "/^:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist400
# At this point we started generating with makeinfo.
# The text output format changed one again.
# There was a spurious blank 'fandango' keyword in 4.1.0.
wordlist410: jarg410.txt
	sed <jarg410.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^  *:fandango:$$/d" \
		-e "/^  *:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^ /d" \
		-e "/^$$/d" | fixcase >wordlist410
wordlist411: jarg411.txt
	sed <jarg411.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^  *:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^ /d" \
		-e "/^$$/d" | fixcase >wordlist411
wordlist412: jarg412.txt
	sed <jarg412.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^  *:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^ /d" \
		-e "/^$$/d" | fixcase >wordlist412
wordlist413: jarg413.txt
	sed <jarg413.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^  *:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^ /d" \
		-e "/^$$/d" | fixcase >wordlist413
wordlist414: jarg414.txt
	sed <jarg414.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^  *:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^ /d" \
		-e "/^$$/d" | fixcase >wordlist414
wordlist415: jarg415.txt
	sed <jarg415.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^  *:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^ /d" \
		-e "/^$$/d" | fixcase >wordlist415
wordlist420: jarg420.txt
	sed <jarg420.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^  *:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^ /d" \
		-e "/^$$/d" | fixcase >wordlist420
wordlist421: jarg421.txt
	sed <jarg421.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^  *:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^ /d" \
		-e "/^$$/d" | fixcase >wordlist421
wordlist422: jarg422.txt
	sed <jarg422.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^  *:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^ /d" \
		-e "/^$$/d" | fixcase >wordlist422
wordlist423: jarg423.txt
	sed <jarg423.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^  *:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^ /d" \
		-e "/^$$/d" | fixcase >wordlist423
wordlist430: jarg430.txt
	sed <jarg430.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^  *:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^ /d" \
		-e "/^$$/d" | fixcase >wordlist430
# At this point we started using fixjargon.pl, which gave us back a close
# approximation of the old text format.
wordlist431: jarg431.txt
	sed <jarg431.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^ /d" \
		-e "/^:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist431
wordlist432: jarg432.txt
	sed <jarg432.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^ /d" \
		-e "/^:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist432
wordlist433: jarg433.txt
	sed <jarg433.txt -n -e "1,/The Jargon Lexicon/d" \
		-e "/(Lexicon Entries End Here)/q" \
		-e "/^ /d" \
		-e "/^:\([^:][^:]*\):.*/s//\1/p" \
		-e "/^$$/d" | fixcase >wordlist433
# Text versions after this will all be generated from HTML

regress: clean $(WORDLISTS)
	for x in $(WORDLISTS); do echo "Checking $${x}"; diff -u $${x}-regress $${x}; done

rebuild-regress: $(WORDLISTS)
	for x in $(WORDLISTS); do cp $${x} $${x}-regress; done

sizes: $(WORDLISTS)
	sizereport $(VERSIONS)

firstlines:
	for x in $(VERSIONS); do head -1 jarg$${x}.txt; done

clean:
	rm -f $(WORDLISTS) *~

