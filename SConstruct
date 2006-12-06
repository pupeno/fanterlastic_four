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
options.AddOptions(
    PathOption("PREFIX", "Base prefix", "/usr/local"),
    PathOption("ERLANGDIR", "Erlang directory (where Erlang is installed)", "$PREFIX/lib/erlang/"),
    PathOption("CONFIGDIR", "Config prefix directory", "$PREFIX/etc/"),
    PathOption("BINDIR", "Binaries prefix directory", "$PREFIX/bin/"),
    PathOption("STATEDIR", "State directory", "$PREFIX/var"))

env = Environment(tools = ["default", "erlang"], options=options)
env["FAN_CONFIGDIR"] = "$CONFIGDIR/fanterlasticfour"
env["FAN_ERLANGDIR"] = "$ERLANGDIR/lib/fanterlasticfour-0.2.0/"

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
scrBldCmd = """sed -e "s,@RUNDIR@,$STATEDIR/run,g" \
                   -e "s,@LOGDIR@,$STATEDIR/log/fanterlasticfour,g" \
                   -e "s,@PIPEDIR@,$STATEDIR/tmp/fanterlasticfour,g" \
                   -e "s,@ERL@,$ERL,g" \
                   -e "s,@RUN_ERL@,$RUN_ERL,g" \
                   -e "s,@ERL_CALL@,$ERL_CALL,g" \
                   -e "s,@CONFIGDIR@,$CONFIGDIR,g" \
                   -e "s,@CONFIGFILE@,$FAN_CONFIGDIR/fanterlasticfour.conf,g" \
                   < $SOURCE > $TARGET"""
scr = env.Command('script/fanterlasticfour', 'script/fanterlasticfour.in',
                  [scrBldCmd, Chmod('$TARGET', 0755)])
cnf = env.Command('script/fanterlasticfour.conf', 'script/fanterlasticfour.conf.in',
                  [scrBldCmd])

# Install files
env.Install("$FAN_ERLANGDIR/ebin/", beams)
env.Install("$FAN_ERLANGDIR/ebin/", "ebin/fanterlasticfour.app")
env.Install("$FAN_ERLANGDIR/src/", sources)
env.Install("$ERLANGDIR/bin/", bootScript)
env.Install("$FAN_CONFIGDIR", "src/fanterlasticfour.config")
env.Install("$FAN_CONFIGDIR", cnf)
env.Install("$BINDIR", scr)

# Alias for installing.
env.Alias("install", "$ERLANGDIR")
env.Alias("install", "$CONFIGDIR")
env.Alias("install", "$BINDIR")

# Documentation
env.EDocFiles(sources)
