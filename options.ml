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

include Plugin.Register
  (struct
    let name = "Ldrgen"
    let shortname = "ldrgen"
    let help = "generate a random C program"
  end)

module Run = False
  (struct
    let option_name = "-ldrgen"
    let help = "generate a random C function"
  end)

module Seed = Zero
  (struct
    let option_name = "-ldrgen-seed"
    let arg_name = "s"
    let help = "set random seed"
  end)

module ExprDepth = Int
  (struct
    let option_name = "-ldrgen-expr-depth"
    let arg_name = "d"
    let default = 5
    let help = "set maximal expression depth"
  end)

module MaxLive = Int
  (struct
    let option_name = "-ldrgen-max-live"
    let arg_name = "l"
    let default = 32
    let help = "set maximal number of concurrently live variables"
  end)

let initialize () =
  if Seed.is_default () then begin
    Random.self_init ();
    let s = Random.bits () in
    Seed.set s
  end;
  Random.init (Seed.get ())
