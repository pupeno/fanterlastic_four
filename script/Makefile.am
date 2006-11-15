#scriptdir = $(PREFIX)/bin
#script_DATA = fanterlasticfour
#
#EXTRA_DIST = $(script_DATA)
#CLEANFILES = $(script_DATA)
#
#fanterlasticfour: fanterlasticfour.sh
#	sed 's|____CONFIGPREFIX____|$(FIXME:_FIND_SOME_VARIABLE_THAT_SHOULD_EVALUATE_TO_/ETC)|' < $< > $@
#	chmod 0755 $@

bin_SCRIPTS = fanterlasticfour
CLEANFILES = fanterlasticfour
EXTRA_DIST = fanterlasticfour.in

confdir = $(sysconfdir)
conf_DATA = fanterlasticfour.conf
CLEANFILES += fanterlasticfour.conf
EXTRA_DIST += fanterlasticfour.conf.in

fanterlasticfour: fanterlasticfour.in
	sed -e "s,@NAMEFILE\@,$(localstatedir)/pid/fanterlasticfour,g" -e "s,@LOGDIR\@,$(localstatedir)/log/fanterlasticfour,g" -e "s,@PIPEDIR\@,$(localstatedir)/tmp/fanterlasticfour,g" -e "s,@ERL\@,$(ERL),g" -e "s,@RUN_ERL\@,$(RUN_ERL),g" -e "s,@ERL_CALL\@,$(ERL_CALL),g" -e "s,@CONFIGFILE\@,$(confdir)/fanterlasticfour.conf,g" $< >$@
	chmod +x $@

fanterlasticfour.conf: fanterlasticfour.conf.in
	sed -e "s,@NAMEFILE\@,$(localstatedir)/pid/fanterlasticfour,g" -e "s,@LOGDIR\@,$(localstatedir)/log/fanterlasticfour,g" -e "s,@PIPEDIR\@,$(localstatedir)/tmp/fanterlasticfour,g" -e "s,@ERL\@,$(ERL),g" -e "s,@RUN_ERL\@,$(RUN_ERL),g" -e "s,@ERL_CALL\@,$(ERL_CALL),g" $< >$@

#CLEANFILES = $(bin_SCRIPTS) $(conf_DATA)