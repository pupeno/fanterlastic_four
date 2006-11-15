scriptdir = $(PREFIX)/bin
script_DATA = fanterlasticfour

EXTRA_DIST = $(script_DATA)
CLEANFILES = $(script_DATA)

fanterlasticfour: fanterlasticfour.sh
	sed 's|____CONFIGPREFIX____|/etc|' < $< > $@
	chmod 0755 $@
