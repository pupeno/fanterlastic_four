erlsrcdir = $(ERLANG_INSTALL_LIB_DIR_fanterlasticfour)/src
erlsrc_DATA = chargen.erl daytime.erl echo.erl time.erl fanterlasticfour_app.erl fanterlasticfour_sup.erl fanterlasticfour.rel

erlbeamdir = $(ERLANG_INSTALL_LIB_DIR_fanterlasticfour)/ebin
erlbeam_DATA = chargen.beam daytime.beam echo.beam time.beam fanterlasticfour_app.beam fanterlasticfour_sup.beam

erlappdir = $(erlbeamdir)
erlapp_DATA = fanterlasticfour.app

erlreldir = $(ERLANG_ROOT_DIR)/bin
erlrel_DATA = fanterlasticfour.script fanterlasticfour.boot

EXTRA_DIST = $(erlsrc_DATA) $(erlapp_DATA) $(erlrel_DATA)
CLEANFILES = $(erlbeam_DATA) $(erlrel_DATA)

%.beam: %.erl
	$(ERLC) $(ERLCFLAGS) -b beam $<
%.script: %.rel
	$(ERLC) $(ERLCFLAGS) -b beam $<
%.boot: %.rel
	$(ERLC) $(ERLCFLAGS) -b beam $<
