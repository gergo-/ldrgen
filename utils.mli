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

open Cil_datatype

(** Select a random member of the list. *)
val random_select: 'a list -> 'a

(** Select a random element of the [lval] set.
    @raises [Not_found] if the set is empty. *)
val random_select_from_set: LvalStructEq.Set.t -> Cil_types.lval

(** Compute the set of all free variables in the expression. We define "free
    variables" as non-global, non-parameter variables. Return the set of
    variables in a list of lvalues without repetitions. *)
val free_vars: Cil_types.exp -> Cil_types.lval list

(** Print the ldrgen-specific arguments that were set on the command line.
    They are printed separated by spaces, with an initial space. *)
val print_command_line_args: Format.formatter -> unit -> unit

(** Return [true] iff the expression is an infinite constant. *)
val is_infinity: Cil_types.exp -> bool
