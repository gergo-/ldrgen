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
  let open Options in
  let types = [intType; uintType; longType; ulongType;
               charType; scharType; ucharType;
               TInt (IShort, []); TInt (IUShort, [])]
  in
  let types =
    if Float.get () then [floatType; doubleType] @ types else types
  in
  let types =
    if LongLong.get () then [longLongType; ulongLongType] @ types else types
  in
  let types =
    if FloatOnly.get () then [floatType; doubleType] else types
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
  let sign = if Random.bool () then 1 else -1 in
  match typ with
  | TInt (ikind, _) ->
    let i = Random.bits () in
    (* Make small-ish constants more likely. *)
    let i = if Random.bool () then i mod (1 lsl 16) else i in
    Cil.kinteger ~loc ikind (sign * i)
  | TFloat (fkind, _) ->
    let max = Floating_point.max_single_precision_float in
    (* Try to allow generation of small constants; without something like
       this, almost all floating point constants would be in the 1e37-1e38
       order of magnitude. *)
    let bound = Utils.random_select [1e3; 1e10; max] in
    Cil.kfloat ~loc fkind (float_of_int sign *. Random.float bound)
  | _ ->
    Cil.mkCast ~force:false ~e:(Cil.zero ~loc) ~newt:typ

(* Initialize a local variable to a constant or a parameter. *)
let gen_local_init vi =
  let gen_param_use typ =
    if !fundec.sformals <> [] then
      let var = Utils.random_select !fundec.sformals in
      Cil.mkCast ~force:false ~e:(Cil.evar var) ~newt:typ
    else
      (* OK, fall back to constants. *)
      gen_const typ
  in
  let generators = [gen_const; gen_param_use] in
  let f = Utils.random_select generators in
  let assign = Set (Cil.var vi, f vi.vtype, loc) in
  Cil.mkStmtOneInstr ~valid_sid:true assign

let gen_var_use ~num_live () =
  (* Select a known local var or parameter of the function, or generate a
     new variable. *)
  let new_var () = gen_var (gen_type ()) in
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
     parameters. This is easy: We simply never put them into the live set,
     which is where variables to assign to are selected from. *)
  let live =
    if vi.vformal then Varinfo.Set.empty else Varinfo.Set.singleton vi
  in
  (live, Cil.evar vi)

let gen_leaf_exp ~depth ~num_live () =
  let gen_const ~num_live () =
    let typ = gen_type () in
    (Varinfo.Set.empty, gen_const typ)
  in
  (* Prefer variables over constants to discourage constant folding and
     propagation, i.e., to avoid making the compiler's job too easy. *)
  let f = Utils.random_select [gen_const; gen_var_use; gen_var_use] in
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
      (* Prefer binary operations over all other kinds. *)
      [gen_leaf_exp; gen_binop; gen_binop; gen_unop]
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
      (* Prefer arithmetic over bitwise operations; prefer other bitwise
         operations over shifts. *)
      let shift = Utils.random_select [Shiftlt; Shiftrt] in
      let bitwise = Utils.random_select [shift; BAnd; BXor; BOr] in
      [PlusA; MinusA; Mult; bitwise]
    else
      [PlusA; MinusA; Mult]
  in
  let binops =
    if Options.DivMod.get () then
      if Cil.isIntegralType (Cil.typeOf lexp) then
        [Div; Mod] @ binops
      else
        [Div] @ binops
    else
      binops
  in
  let op = Utils.random_select binops in
  let live = Varinfo.Set.union llive rlive in
  (* For some operations, it's best to patch the RHS operand to avoid some
     common problems. *)
  let rexp' =
    match op with
    | Shiftlt | Shiftrt ->
      (* Generate a modulo expression to transform this operand into the
         legal range. It must be less than the bit size of the LHS. It must
         also be non-negative, and the modulo operation alone does not
         ensure this. We therefore cast to unsigned. *)
      let unsigned_rexp =
        let newt =
          if Options.LongLong.get () then Cil.ulongLongType else Cil.ulongType
        in
        Cil.mkCast ~force:false ~e:rexp ~newt
      in
      let lhs_bitsize = Cil.bitsSizeOf (Cil.typeOf lexp) in
      let modulus = Cil.integer ~loc lhs_bitsize in
      (* Note: This modulo operation is generated even if -no-div-mod is
         set. That's OK, any compiler should replace it by a bit mask. *)
      Cil.mkBinOp ~loc Mod unsigned_rexp modulus
    | Div | Mod ->
      let rexp = Cil.constFold true rexp in
      begin match Cil.isInteger rexp with
      | Some n when not (Integer.equal Integer.zero n) ->
        (* Division by a nonzero integer. OK, keep it. *)
        rexp
      | _ ->
        (* Something other than a nonzero integer. Add a small random
           integer, just to be on the safe side; it's unlikely that this
           produces a constant zero. *)
        let random_num = Cil.integer ~loc (Random.int 1024 + 1) in
        Cil.mkBinOp ~loc PlusA rexp random_num
      end
    | _ -> rexp
  in
  (live, Cil.mkBinOp ~loc op lexp rexp')

and gen_unop ~depth ~num_live () =
  let depth = depth + 1 in
  let live, exp = gen_exp ~depth ~num_live () in
  let typ = Cil.typeOf exp in
  let unops = if Cil.isIntegralType typ then [Neg; BNot; LNot] else [Neg] in
  let op = Utils.random_select unops in
  (live, Cil.new_exp ~loc (UnOp (op, exp, typ)))

let gen_exp ~num_live ?typ () =
  let live, exp = gen_exp ~depth:1 ~num_live () in
  match typ with
  | Some typ -> (live, Cil.mkCast ~force:false ~e:exp ~newt:typ)
  | None -> (live, exp)

let gen_cond ~num_live () =
  let llive, lexp = gen_exp ~num_live () in
  let rlive, rexp = gen_exp ~num_live () in
  let lexp, rexp = gen_common_type_exprs lexp rexp in
  let comparisons = [Lt; Gt; Le; Ge; Eq; Ne] in
  let cmp = Utils.random_select comparisons in
  let live = Varinfo.Set.union llive rlive in
  (live, Cil.mkBinOp ~loc cmp lexp rexp)

let gen_assignment_to vi ~depth ~live () =
  let num_live = Varinfo.Set.cardinal live in
  let (new_live_vars, exp) = gen_exp ~num_live ~typ:vi.vtype () in
  let live =
    Varinfo.Set.union (Varinfo.Set.remove vi live) new_live_vars
  in
  let assign = Set (Cil.var vi, exp, loc) in
  let stmt = Cil.mkStmtOneInstr ~valid_sid:true assign in
  (live, stmt)

let gen_assignment ~depth ~live () =
  let vi = Utils.random_select_from_set live in
  gen_assignment_to vi ~depth ~live ()

let rec gen_stmt ~depth ~live () =
  let generators =
    if depth < Options.StmtDepth.get () then
      let gens = [gen_assignment; gen_if_stmt] in
      if Options.Loops.get () then [gen_while_loop] @ gens else gens
    else
      [gen_assignment]
  in
  let f = Utils.random_select generators in
  f ~depth ~live ()

and gen_if_stmt ~depth ~live () =
  let depth = depth + 1 in
  let (true_live, true_stmts) = gen_stmts ~depth ~live ~tail:[] () in
  let (false_live, false_stmts) = gen_stmts ~depth ~live ~tail:[] () in
  let body_live = Varinfo.Set.union true_live false_live in
  let num_live = Varinfo.Set.cardinal body_live in
  let (cond_new_live, cond) = gen_cond ~num_live () in
  let live = Varinfo.Set.union body_live cond_new_live in
  let if_stmt =
    Cil.mkStmt ~valid_sid:true
      (If (cond, Cil.mkBlock true_stmts, Cil.mkBlock false_stmts, loc))
  in
  (live, if_stmt)

and gen_while_loop ~depth ~live () =
  let depth = depth + 1 in
  (* Save the variables that are live after the loop. They may be defined in
     the loop body, but must nevertheless still be live before the loop. *)
  let live_out = live in
  let n = Random.int (Options.BlockLength.get ()) + 1 in
  (* A loop may have circular dependences that complicate things: It may
     assign a variable on one iteration and use it during the next
     iteration. The variable is live around the loop's back edge and live
     into the loop. The use is typically physically before the redefinition
     in the loop body.
     To generate such loops, we cut the body in half and generate first the
     first half, then the second one (each of these in the normal bottom-up
     fashion) with the corresponding live variable sets at the given program
     points:
        body = <L1> first_half <L2> second_half <L3>
        where L2 contains live_out, and L3 contains L1 and the condition's
        free variables
     Some variables will be used in the first half and become live in L1;
     looping back into the second half, they may get definitions inside the
     loop.
     We then have to combine the results of the liveness analysis correctly.
     Any variable in L1 is live into the loop, and is in L3. For variables
     live into the second half that are *not* in L1:
     - If they are in live_out, they are in L2, and the first half's
       liveness analysis already treated them correctly.
     - If they are not in live_out, they must have become live in the second
       half. There are two cases for such a variable v:
       + It is is defined in the first half. As v is not in live_out, this
         definition must have been caused by a use that also appears in the
         first half. If there is such a definition, it might kill our use
         from the second half. FIXME: Check this. If we keep v live, we
         overapproximate liveness and might generate some dead code.
       + It is not defined in the first half. The use in the second half
         must therefore be live before the loop body.
     In short, we can just take the union of L1 and L2 and have a safe
     overapproximation of the correct solution that is hopefully not so
     coarse that we generate a lot of dead code. *)
  let (live_body_first, stmts_first) =
    gen_stmts ~n ~depth ~live ~tail:[] ()
  in
  (* Generate the condition. *)
  let live = live_body_first in
  let num_live = Varinfo.Set.cardinal live in
  let (cond_new_live, cond) = gen_cond ~num_live () in
  let live = Varinfo.Set.union live cond_new_live in
  (* Try to generate an assignment to one of the variables in the condition.
     This is meant to simulate some kind of progress towards termination of
     the loop. *)
  let (live, tail) =
    let cond_vars = Utils.free_vars cond in
    if cond_vars <> [] then
      let vi = Utils.random_select cond_vars in
      let (live, stmt) = gen_assignment_to vi ~depth ~live () in
      (live, [stmt])
    else
      (live, [])
  in
  (* Now generate the second half of the body. *)
  let n =
    Options.BlockLength.get () - (List.length stmts_first + List.length tail)
  in
  let (live_body_second, stmts_second) = gen_stmts ~n ~depth ~live ~tail () in
  (* Compute the new live set according to the above. *)
  let live_body = Varinfo.Set.union live_body_first cond_new_live in
  (* TODO: Remove those variables that are killed in the first half. *)
  let live_body = Varinfo.Set.union live_body live_body_second in
  let loop = Cil.mkWhile ~guard:cond ~body:(stmts_first @ stmts_second) in
  let live = Varinfo.Set.union live_body live_out in
  (live, Cil.mkStmt ~valid_sid:true (Block (Cil.mkBlock loop)))

and gen_stmts ?n ~depth ~live ~tail () =
  let rec rec_gen_stmts n ~live ~tail =
    (* Stop when we have generated [n] statements or when we have no more
       live variables to define. *)
    if n < 1 || Varinfo.Set.is_empty live then
      (live, tail)
    else
      let (live, stmt) = gen_stmt ~depth ~live () in
      rec_gen_stmts (n-1) ~live ~tail:(stmt :: tail)
  in
  let n =
    match n with
    | Some n -> n
    | None -> Options.BlockLength.get () - List.length tail
  in
  rec_gen_stmts n ~live ~tail

let gen_function () =
  let typ = gen_type () in
  Cil.setReturnType !fundec typ;
  let (live, return) = gen_return ~typ () in
  let (live', stmts) = gen_stmts ~depth:1 ~live ~tail:[return] () in
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
  let f = gen_function () in
  (* Set the function's list of formal arguments to its list of formal
     arguments. This is silly, but it ensures that we generate a proper
     prototype: If the list of formals is empty, the argument list will be
     (void) instead of empty (). *)
  begin match f with
  | GFun (fdec, _) -> Cil.setFormals fdec fdec.sformals
  | _ -> ()
  end;
  { ast with globals = f :: ast.globals }

let gen_header ast =
  let header = Format.asprintf
    "// Generated by ldrgen\n\
     // Seed: %d\n\
     // Command line arguments:%a"
    (Options.Seed.get ()) Utils.print_command_line_args ()
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
