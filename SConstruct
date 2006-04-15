# -*- coding: utf-8 -*-
# Copyright (C) 2005 José Pablo Ezequiel "Pupeno" Fernández Silva
#
# This file is part of Fanterlastic Four.
#
# Fanterlastic Four is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
# Fanterlastic Four is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with Fanterlastic Four; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

env = Environment(tools = ["default", "erlang"])

# Use cache, erlang takes long to find dependencies.
env.SetOption('implicit_cache', 1)

# Configuration.
configFile = ".fanterlasticfour.conf"
opts = Options(configFile)
opts.Add(PathOption("ERLANGPREFIX", "Erlang prefix directory (where Erlang is installed)", "/usr/local/lib/erlang/"))
opts.Add(PathOption("CONFIGPREFIX", "Config prefix directory", "/usr/local/etc/"))
opts.Add(PathOption("BINPREFIX", "Binaries prefix directory", "/usr/local/bin/"))
opts.Update(env)
opts.Save(configFile, env)

# Help.
Help(opts.GenerateHelpText(env))

sources = ["src/fanterlasticfour_app.erl",
           "src/fanterlasticfour_sup.erl",
           "src/echo.erl",
           "src/chargen.erl",
           "src/daytime.erl",
           "src/time.erl"]

# Compile the sources to .beams.
beams = env.Erlang(sources)

# Generate the boot and script.
bootScript = env.Erlang(["src/fanterlasticfour.rel"],
                        LIBPATH="src/")

# Install directories.
fanterlasticFourDir = "$ERLANGPREFIX/lib/fanterlasticfour-0.0.0/"
configDir = "$CONFIGPREFIX/fanterlasticfour/"
binDir = "$BINPREFIX"

# Install files
env.Install(fanterlasticFourDir + "ebin/", beams)
env.Install(fanterlasticFourDir + "ebin/", "src/fanterlasticfour.app")
env.Install(fanterlasticFourDir + "src/", sources)
env.Install("$ERLANGPREFIX/bin/", bootScript)
env.Install(configDir, "configs/fanterlasticfour.sh")
env.Install(configDir, "configs/fanterlasticfour.config")
env.InstallAs(binDir + "/fanterlasticfour", "scripts/fanterlasticfour.sh")

# Alias for installing.
env.Alias("install", "$ERLANGPREFIX")

# Documentation
env.EDoc("doc/index.html", sources)
