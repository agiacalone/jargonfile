#!/usr/bin/env python
#
# getwords -- extract a word list from a jargon file

import os, getopt, re, sys

if __name__ == '__main__': 
    # Process options
    (options, arguments) = getopt.getopt(sys.argv[1:], "a:gh:i:ns:v")
    greatrunes = nodivider = verbose = 0
    skip_appendices = []
    sublist = []
    ignorelist = []
    header = None
    for (switch, val) in options:
        if switch == "-a":
            skip_appendices.append(val)
        elif switch == "-i":
            ignorelist.append(val)
	elif switch == '-g':
	    greatrunes = 1
        elif switch == '-h':
            header = val
	elif switch == '-n':
	    nodivider = 1
	elif switch == '-s':
	    sublist.append(val.split("/"))
	elif switch == '-v':
	    verbose += 1

    fp = open(arguments[0], "r")

    if nodivider:
        # Old-style file -- no header, all-caps keywords
        state = 0
        while 1:
            line = fp.readline()
            if not line:
                break
            if state == 0:
                if line in ("\n", "\r\n"):
                    state = 1
                    continue
                else:
                    state = 0
                    continue
            elif re.match("^[A-Z@\\][A-Z][A-Z]", line):
                # Assumes the first headword will be a single word.
                print line.split()[0]
                break
            else:
                state = 0
    elif header:
        # We specified a header
        while 1:
            line = fp.readline()
            if not line:
                break
            if line.startswith(header):
                break
    else:
        sys.stderr.write("No header mode specified.\n")
        sys.exit(1)

    if greatrunes:
        # Assume all-capsed keywords.
        key_re = re.compile("^[A-Z0-9]|^[@/\\\\][A-Z-]")
        small_re = re.compile(" *[A-Z]?[a-z]")
        while 1:
            line = fp.readline()
            if not line:
                break
            if filter(lambda x: line.startswith(x), skip_appendices):
                while 1:
                    line = fp.readline()
                    if not line:
                        break
                    # Note: first line of following appendix is consumed,
                    # so we can't skip two appendices in a row!
                    if line.startswith("Appendix"):
                        break
            if line.startswith("APPENDIX "):
                continue
            if not key_re.match(line):
                continue
            line = line.rstrip()
            if line in ignorelist:
                continue
            if verbose:
                print "Before: ", line
            while 1:
                if line.find("  ") > -1:
                    line = line[:line.find("  ")]
                    if verbose:
                        print "Discard text after two spaces: ", line
                    continue
                if line.find(" 1.") > -1:
                    line = line[:line.find(" 1.")+1]
                    if verbose:
                        print "Discard text after 1.: ", line
                    continue
                if line.find(" [") > -1:
                    line = line[:line.find(" [")+1]
                    if verbose:
                        print "Discard text after [: ", line
                    continue
                if line.find(" (") > -1:
                    line = line[:line.find(" (")+1]
                    if verbose:
                        print "Discard text after (: ", line
                    continue
                if line.find(" /") > 0:
                    line = line[:line.find(" /")+1]
                    if verbose:
                        print "Discard text after /: ", line
                    continue
                if line.find(" ``") > 0:
                    line = line[:line.find(" ``")]
                    if verbose:
                        print "Stripped trailing open quote:", line
                    continue
                if line[-1] == ",":
                    line = line[:-1]
                    if verbose:
                        print "Stripped trailing comma:", line
                    continue
                if line[-1] == ":":
                    line = line[:-1]
                    if verbose:
                        print "Stripped trailing colon:", line
                    continue
                if line[-1] == "\"":
                    line = line[:-1]
                    if verbose:
                        print "Stripped trailing double quote:", line
                    continue
                m = small_re.search(line)
                if m:
                    line = line[:m.start(0)]
                    if verbose:
                        print "Discard text after [a-z]: ", line
                    if line == "":
                        break
                    else:
                        continue
                stripped = line.rstrip()
                if stripped != line:
                    line = stripped
                    if verbose:
                        print "Right-strip again: ", line
                    continue
                for (s, t) in sublist:
                    if line.find(s) > -1:
                        if verbose:
                            print "Replaced %s with %s: %s" % (s, t, line)
                        line = line.replace(s, t)
                        continue
                break
            if line != "":
                print line
# End
