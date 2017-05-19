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
open Cil_types

let random_select xs =
  let i = Random.int (List.length xs) in
  List.nth xs i

let random_select_from_set set =
  try
    (* FIXME: Use a smarter selection algorithm. *)
    random_select (Varinfo.Set.elements set)
  with _ -> raise Not_found

class free_vars_visitor set = object
  inherit Visitor.frama_c_inplace
  method !vvrbl vi =
    if not vi.vglob && not vi.vformal then
      set := Varinfo.Set.add vi !set;
    Cil.SkipChildren
end

let free_vars exp =
  let set = ref Varinfo.Set.empty in
  ignore (Visitor.visitFramacExpr (new free_vars_visitor set) exp);
  Varinfo.Set.elements !set
