##########################################################################
#                                                                        #
#  ldrgen, a generator of random C programs                              #
#  Copyright (C) 2017, Gergö Barany <gergo@tud.at>                       #
#                                                                        #
#  This program is free software: you can redistribute it and/or modify  #
#  it under the terms of the GNU General Public License as published by  #
#  the Free Software Foundation, either version 3 of the License, or     #
#  (at your option) any later version.                                   #
#                                                                        #
#  This program is distributed in the hope that it will be useful,       #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the          #
#  GNU General Public License for more details.                          #
#                                                                        #
#  You should have received a copy of the GNU General Public License     #
#  along with this program. If not, see <http://www.gnu.org/licenses/>.  #
#                                                                        #
##########################################################################

PLUGIN_NAME = Ldrgen
PLUGIN_VERSION = 0.0.1

# Hook into Frama-C's plugin build system, which does the rest.
ifndef FRAMAC_SHARE
FRAMAC_SHARE := $(shell frama-c-config -print-share-path)
endif

ifndef FRAMAC_LIBDIR
FRAMAC_LIBDIR := $(shell frama-c-config -print-libpath)
endif

# Allow Frama-C's internal compilation.
PLUGIN_DIR ?=.
PLUGIN_ENABLED:=yes
PLUGIN_HAS_MLI:=yes

# These are the names of the modules that will be compiled and linked
# together to form the plugin.
PLUGIN_CMO = options utils generate


include $(FRAMAC_SHARE)/Makefile.dynamic
