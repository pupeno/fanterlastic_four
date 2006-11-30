# -*- coding: utf-8 -*-
# Copyright (C) 2005 José Pablo Ezequiel "Pupeno" Fernández Silva
#
# This file is part of Fanterlastic Four.
#
# Fanterlastic Four is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
# Fanterlastic Four is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with Fanterlastic Four; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

# Configuration.
options = Options("options.cache")
options.AddOptions(PathOption("ERLANGPREFIX", "Erlang prefix directory (where Erlang is installed)", "/usr/local/lib/erlang/"),
                PathOption("CONFIGPREFIX", "Config prefix directory", "/usr/local/etc/"),
                PathOption("BINPREFIX", "Binaries prefix directory", "/usr/local/bin/"))

env = Environment(tools = ["default", "erlang"], options=options)

# Save the options.
options.Save(options.files[0], env)

# Use cache, erlang takes long to find dependencies.
env.SetOption('implicit_cache', 1)

# Help.
Help(options.GenerateHelpText(env))

sources = ["src/fanterlasticfour_app.erl",
           "src/fanterlasticfour_sup.erl",
           "src/echo.erl",
           "src/chargen.erl",
           "src/daytime.erl",
           "src/time.erl"]

# Compile the sources to .beams.
beams = env.Erlang(sources)

# Generate the boot and script.
bootScript = env.Erlang(["src/fanterlasticfour.rel"])

# Generate the start script.
#TODO: perform the correct replacements, using SCons variables (and not unenxisting Makefile ones).
scrBldCmd = "sed -e \"s,@RUNDIR@,$(localstatedir)/run,g\" -e \"s,@LOGDIR@,$(localstatedir)/log/fanterlasticfour,g\" -e \"s,@PIPEDIR\@,$(localstatedir)/tmp/fanterlasticfour,g\" -e \"s,@ERL\@,$(ERL),g\" -e \"s,@RUN_ERL\@,$(RUN_ERL),g\" -e \"s,@ERL_CALL\@,$(ERL_CALL),g\" -e \"s,@CONFIGFILE\@,$(confdir)/fanterlasticfour.conf,g\" < $SOURCE > $TARGET"
scr = env.Command('script/fanterlasticfour', 'script/fanterlasticfour.in',
                  [scrBldCmd, Chmod('$TARGET', 0755)])
cnf = env.Command('script/fanterlasticfour.conf', 'script/fanterlasticfour.conf.in',
                  [scrBldCmd])

# Install directories.
fanterlasticFourDir = "$ERLANGPREFIX/lib/fanterlasticfour-0.0.0/"
configDir = "$CONFIGPREFIX/fanterlasticfour/"
binDir = "$BINPREFIX"

# Install files
env.Install(fanterlasticFourDir + "ebin/", beams)
env.Install(fanterlasticFourDir + "ebin/", "ebin/fanterlasticfour.app")
env.Install(fanterlasticFourDir + "src/", sources)
env.Install("$ERLANGPREFIX/bin/", bootScript)
env.Install(configDir, "src/fanterlasticfour.config")
env.Install(configDir, cnf)
env.Install(binDir, scr)

# Alias for installing.
env.Alias("install", "$ERLANGPREFIX")
env.Alias("install", "$CONFIGPREFIX")
env.Alias("install", "$BINPREFIX")

# Documentation
env.EDoc("doc/index.html", sources)
