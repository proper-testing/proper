#!/usr/bin/env bash

# -*- coding: utf-8 -*-
# --------------------------------------------------------------------
# Copyright 2010-2023 Manolis Papadakis <manopapad@gmail.com>,
#                     Eirini Arvaniti <eirinibob@gmail.com>
#                 and Kostis Sagonas <kostis@cs.ntua.gr>
#
# This file is part of PropEr.
#
# PropEr is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# PropEr is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with PropEr.  If not, see <http://www.gnu.org/licenses/>.

# Author:      Manolis Papadakis
# Description: Script for testing the validity of escript files

for ESCRIPT_NAME in "$@"; do
    cd scripts || exit
    SRC_FILE="$ESCRIPT_NAME".erl
    BIN_FILE="$ESCRIPT_NAME".beam
    touch "$SRC_FILE"
    {
      echo "-module($ESCRIPT_NAME)."
      echo "-export([main/1])."
      echo -n "%"
      cat "$ESCRIPT_NAME"
    } >> "$SRC_FILE"
    erlc +debug_info "$SRC_FILE"; true
    dialyzer -Wunmatched_returns "$BIN_FILE"; true
    rm -f "$SRC_FILE" "$BIN_FILE"
    cd ..
done
