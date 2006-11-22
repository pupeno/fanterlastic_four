bin_SCRIPTS = fanterlasticfour
EXTRA_DIST = fanterlasticfour.in

confdir = $(sysconfdir)/fanterlasticfour
conf_DATA = fanterlasticfour.conf
EXTRA_DIST += fanterlasticfour.conf.in

fanterlasticfour: fanterlasticfour.in
	sed -e "s,@RUNDIR\@,$(localstatedir)/run,g" -e "s,@LOGDIR\@,$(localstatedir)/log/fanterlasticfour,g" -e "s,@PIPEDIR\@,$(localstatedir)/tmp/fanterlasticfour,g" -e "s,@ERL\@,$(ERL),g" -e "s,@RUN_ERL\@,$(RUN_ERL),g" -e "s,@ERL_CALL\@,$(ERL_CALL),g" -e "s,@CONFIGFILE\@,$(confdir)/fanterlasticfour.conf,g" $< >$@
	chmod +x $@

fanterlasticfour.conf: fanterlasticfour.conf.in
	sed -e "s,@RUNDIR\@,$(localstatedir)/run,g" -e "s,@LOGDIR\@,$(localstatedir)/log/fanterlasticfour,g" -e "s,@PIPEDIR\@,$(localstatedir)/tmp/fanterlasticfour,g" -e "s,@ERL\@,$(ERL),g" -e "s,@RUN_ERL\@,$(RUN_ERL),g" -e "s,@ERL_CALL\@,$(ERL_CALL),g" $< >$@

CLEANFILES = $(bin_SCRIPTS) $(conf_DATA)