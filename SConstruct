# -*- coding: utf-8 -*-
# Copyright (C) 2005 José Pablo Ezequiel "Pupeno" Fernández Silva
#
# This file is part of Fanterlastic Four.
#
# Fanterlastic Four is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
# Fanterlastic Four is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with Fanterlastic Four; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

env = Environment(tools = ["default", "erlang"], toolpath = ["../SConsErlang/"])

# Configuration.
configFile = ".FanterlasticFour.conf"
opts = Options(configFile)
opts.Add(PathOption("PREFIX", "Prefix directory (where Erlang is installed)", "/usr/local/lib/erlang/"))
opts.Update(env)
opts.Save(configFile, env)

# Help.
Help(opts.GenerateHelpText(env))

# Compile the sources to .beams.
beams = env.Erlang(["src/fanterlasticfour_app",
                    "src/fanterlasticfour_sup",
                    "src/echo",
                    "src/chargen",
                    "src/daytime",
                    "src/time"])

# Generate the boot and script.
bootScript = env.Erlang(["src/fanterlasticfour.rel"],
                        LIBPATH="src/")

# Install directories.
fanterlasticFourDir = "$PREFIX/lib/fanterlasticfour-0.0.0/"

# chicken.py, no build needed.
env.Install(fanterlasticFourDir + "ebin/", beams)
env.Install(fanterlasticFourDir + "ebin/", "src/fanterlasticfour.app")
env.Install("$PREFIX/bin/", bootScript)

# Alias for installing.
env.Alias("install", "$PREFIX")

