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

(** Select a random element of the [varinfo] set.
    @raises [Not_found] if the set is empty. *)
val random_select_from_set: Varinfo.Set.t -> Cil_types.varinfo
