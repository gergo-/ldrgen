(**************************************************************************)
(*                                                                        *)
(*  ldrgen, a generator of random C programs                              *)
(*  Copyright (C) 2017, Gergö Barany <gergo@tud.at>                       *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the          *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program. If not, see <http://www.gnu.org/licenses/>.  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.General_services

module Run: Parameter_sig.Bool

module Seed: Parameter_sig.Int
module ExprDepth: Parameter_sig.Int
module StmtDepth: Parameter_sig.Int
module BlockLength: Parameter_sig.Int
module MaxLive: Parameter_sig.Int
module MaxArgs: Parameter_sig.Int

module Float: Parameter_sig.Bool
module FloatOnly: Parameter_sig.Bool
module IntOnly: Parameter_sig.Bool
module LongLong: Parameter_sig.Bool
module DivMod: Parameter_sig.Bool
module Bitwise: Parameter_sig.Bool
module Loops: Parameter_sig.Bool

(** Initialize global state, such as the random number generator. *)
val initialize: unit -> unit
