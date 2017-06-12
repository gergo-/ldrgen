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

module StmtDepth = Int
  (struct
    let option_name = "-ldrgen-stmt-depth"
    let arg_name = "d"
    let default = 3
    let help = "set maximal statement nesting depth"
  end)

module BlockLength = Int
  (struct
    let option_name = "-ldrgen-block-length"
    let arg_name = "n"
    let default = 5
    let help = "set maximal number of instructions per block"
  end)

module MaxLive = Int
  (struct
    let option_name = "-ldrgen-max-live"
    let arg_name = "l"
    let default = 32
    let help = "set maximal number of concurrently live variables"
  end)

module MaxArgs = Int
  (struct
    let option_name = "-ldrgen-max-args"
    let arg_name = "a"
    let default = 8
    let help = "set maximal number of function arguments"
  end)

module Float = True
  (struct
    let option_name = "-ldrgen-float"
    let help = "allow generation of floating-point arithmetic"
  end)

module FloatOnly = False
  (struct
    let option_name = "-ldrgen-float-only"
    let help = "allow *only* generation of floating-point arithmetic but \
                no integer arithmetic"
  end)

module IntOnly = False
  (struct
    let option_name = "-ldrgen-int-only"
    let help = "allow *only* generation of int and unsigned int types"
  end)

module LongLong = True
  (struct
    let option_name = "-ldrgen-long-long"
    let help = "allow generation of long long arithmetic"
  end)

module DivMod = True
  (struct
    let option_name = "-ldrgen-div-mod"
    let help = "allow generation of division and modulo operations"
  end)

module Bitwise = True
  (struct
    let option_name = "-ldrgen-bitwise"
    let help = "allow generation of bitwise operations"
  end)

module Loops = True
  (struct
    let option_name = "-ldrgen-loops"
    let help = "allow generation of loops"
  end)

let initialize () =
  (* Fake some command line settings to keep Frama-C from complaining about
     missing input files, and about problems preprocessing [/dev/null]. *)
  Kernel.Verbose.set 0;
  Kernel.Files.set ["/dev/null"];
  Kernel.CppCommand.set "/bin/true";
  Kernel.CppGnuLike.set false;
  (* Initialize the random number generator. *)
  if Seed.is_default () then begin
    Random.self_init ();
    let s = Random.bits () in
    Seed.set s
  end;
  Random.init (Seed.get ())
