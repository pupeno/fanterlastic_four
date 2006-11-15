scriptdir = $(PREFIX)/bin
script_DATA = fanterlasticfour

EXTRA_DIST = $(script_DATA)
CLEANFILES = $(script_DATA)

fanterlasticfour: fanterlasticfour.sh
	sed 's|____CONFIGPREFIX____|$(FIXME:_FIND_SOME_VARIABLE_THAT_SHOULD_EVALUATE_TO_/ETC)|' < $< > $@
	chmod 0755 $@
