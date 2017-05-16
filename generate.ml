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

open Cil
open Cil_types
open Cil_datatype

let loc = Cil.builtinLoc

let fundec = ref (Cil.emptyFunction "fn1")

let gen_type () =
  let types = Cil.[intType; uintType; longType; ulongType;
                   longLongType; ulongLongType;
                   charType; scharType; ucharType;
                   TInt (IShort, []); TInt (IUShort, []);
                   floatType; doubleType]
  in
  Utils.random_select types

let gen_local_var ?basename typ =
  let name = match basename with Some n -> n | None -> "v" in
  Cil.makeLocalVar !fundec name typ

let gen_formal_var ?basename typ =
  let name = match basename with Some n -> n | None -> "p" in
  Cil.makeFormalVar !fundec name typ

let gen_var ?basename typ =
  let gen_funcs = [gen_local_var; gen_formal_var] in
  let f = Utils.random_select gen_funcs in
  f ?basename typ

let gen_return ?typ () =
  let typ = match typ with Some t -> t | None -> gen_type () in
  let var = gen_local_var ~basename:"result" typ in
  let live = Varinfo.Set.singleton var in
  (live, Cil.mkStmt ~valid_sid:true (Return (Some (Cil.evar var), loc)))

(* For integer or floating-point types, generate a random number (possibly
   negative) of the appropriate type. For other types, just generate a zero
   of the appropriate type. *)
let gen_const typ =
  match typ with
  | TInt (ikind, _) ->
    let i = Random.bits () - (1 lsl 15) in
    Cil.kinteger ~loc ikind i
  | TFloat (fkind, _) ->
    let max = Floating_point.max_single_precision_float in
    Cil.kfloat ~loc fkind (Random.float max -. (max /. 2.0))
  | _ ->
    Cil.mkCast ~force:false ~e:(Cil.zero ~loc) ~newt:typ

let gen_local_init vi =
  let assign = Set (Cil.var vi, gen_const vi.vtype, loc) in
  Cil.mkStmtOneInstr ~valid_sid:true assign

let gen_var_use ~num_live () =
  (* Select a known local var or parameter of the function, or generate a
     new variable. *)
  let typ = gen_type () in
  let new_var () = gen_var typ in
  let select_or_new vars =
    if vars <> [] then Utils.random_select vars else new_var ()
  in
  let use_param () = select_or_new !fundec.sformals in
  let use_local () = select_or_new !fundec.slocals in
  (* Try to generate new variables if we don't have many live ones; pick
     existing ones if enough are live. *)
  let vi =
    if Random.int (Options.MaxLive.get ()) > num_live then
      new_var ()
    else
      let f = Utils.random_select [use_param; use_local] in
      f ()
  in
  (* We never want to generate assignments to the function's formal
     parameter. This is easy: We simply never put them into the live set,
     which is where functions to assign to are selected from. *)
  let live =
    if vi.vformal then Varinfo.Set.empty else Varinfo.Set.singleton vi
  in
  (live, Cil.mkCast ~force:false ~e:(Cil.evar vi) ~newt:typ)

let gen_leaf_exp ~depth ~num_live () =
  let gen_const ~num_live () =
    let typ = gen_type () in
    (Varinfo.Set.empty, gen_const typ)
  in
  let f = Utils.random_select [gen_const; gen_var_use] in
  f ~num_live ()

(* Make sure that either both expressions are integers or both are
   floating-point values (picking randomly, if originally one is integer and
   the other floating). Return a pair of the two expressions with
   appropriate casts. *)
let gen_common_type_exprs expr1 expr2 =
  let is_int e = Cil.(isIntegralType (typeOf e)) in
  let is_float e = Cil.(isFloatingType (typeOf e)) in
  if is_int expr1 && is_int expr2 || is_float expr1 && is_float expr2 then
    (expr1, expr2)
  else
    let typ = Utils.random_select [Cil.typeOf expr1; Cil.typeOf expr2] in
    let cast_to_common_typ e = Cil.mkCast ~force:false ~e ~newt:typ in
    (cast_to_common_typ expr1, cast_to_common_typ expr2)

let rec gen_exp ~depth ~num_live () =
  let generators =
    if depth <= Options.ExprDepth.get () then
      [gen_leaf_exp; gen_binop; gen_unop]
    else
      [gen_leaf_exp]
  in
  let f = Utils.random_select generators in
  f ~depth ~num_live ()

and gen_binop ~depth ~num_live () =
  let depth = depth + 1 in
  let llive, lexp = gen_exp ~depth ~num_live () in
  let rlive, rexp = gen_exp ~depth ~num_live () in
  let lexp, rexp = gen_common_type_exprs lexp rexp in
  let binops =
    if Cil.isIntegralType (Cil.typeOf lexp) then
      [PlusA; MinusA; Mult; Div; Mod; Shiftlt; Shiftrt; BAnd; BXor; BOr]
    else
      [PlusA; MinusA; Mult; Div]
  in
  let op = Utils.random_select binops in
  let live = Varinfo.Set.union llive rlive in
  (live, Cil.mkBinOp ~loc op lexp rexp)

and gen_unop ~depth ~num_live () =
  let depth = depth + 1 in
  let live, exp = gen_exp ~depth ~num_live () in
  let typ = Cil.typeOf exp in
  let unops = if Cil.isIntegralType typ then [Neg; BNot; LNot] else [Neg] in
  let op = Utils.random_select unops in
  (live, Cil.new_exp ~loc (UnOp (op, exp, typ)))

let gen_exp ~num_live typ =
  let live, exp = gen_exp ~depth:1 ~num_live () in
  (live, Cil.mkCast ~force:false ~e:exp ~newt:typ)

let gen_stmts ~live ~tail () =
  let rec rec_gen_stmts n ~live ~tail =
    let num_live = Varinfo.Set.cardinal live in
    (* Stop when we have generated [n] statements or when we have no more
       live variables to define. *)
    if n < 1 || num_live = 0 then
      (live, tail)
    else
      let vi = Utils.random_select_from_set live in
      let (new_live_vars, exp) = gen_exp ~num_live vi.vtype in
      let live =
        Varinfo.Set.union (Varinfo.Set.remove vi live) new_live_vars
      in
      let assign = Set (Cil.var vi, exp, loc) in
      let stmt = Cil.mkStmtOneInstr ~valid_sid:true assign in
      rec_gen_stmts (n-1) ~live ~tail:(stmt :: tail)
  in
  rec_gen_stmts (5 - List.length tail) ~live ~tail

let gen_function () =
  let typ = gen_type () in
  Cil.setReturnType !fundec typ;
  let (live, return) = gen_return ~typ () in
  let (live', stmts) = gen_stmts ~live ~tail:[return] () in
  !fundec.sbody.bstmts <- stmts;
  (* Initialize all live non-formal local variables. *)
  Varinfo.Set.iter
    (fun vi ->
       if not vi.vglob && not vi.vformal then
         let init = gen_local_init vi in
         !fundec.sbody.bstmts <- init :: !fundec.sbody.bstmts)
    live';
  let f = GFun (!fundec, loc) in
  f

let gen_random_function ast =
  { ast with globals = gen_function () :: ast.globals }

let gen_header ast =
  let header = Format.asprintf
    "// Generated by ldrgen\n\
     // Seed: %d"
    (Options.Seed.get ())
  in
  { ast with globals = GText header :: ast.globals }

let run () =
  if Options.Run.get () then begin
    Options.initialize ();
    let ast = gen_random_function (Ast.get ()) in
    let ast' = gen_header ast in
    Format.printf "%a" Printer.pp_file ast'
  end

let () =
  Db.Main.extend run
