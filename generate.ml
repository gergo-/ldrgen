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

module LvalSet = struct
  include LvalStructEq.Set

  let add_vi vi = add (Cil.var vi)
  let singleton_vi vi = singleton (Cil.var vi)
end

let loc = Location.unknown

(* The function we're building. *)
let fundec = ref (Cil.emptyFunction "fn1")

(* "Parameter lvalues" are lvalues that may be used in expressions and are
   based on function parameters. If the parameter is a pointer, the
   corresponding lvalue in this set is dereferenced appropriately. *)
let param_lvals = ref (LvalSet.empty)

(* Global variable for modeling the sizes of arrays. *)
let array_size_var = ref None

let array_size () =
  match !array_size_var with
  | Some vi -> Cil.evar vi
  | None ->
    let vi = Cil.makeGlobalVar "N" Cil.uintType in
    vi.vstorage <- Extern;
    array_size_var := Some vi;
    Cil.evar vi

let gen_type () =
  let open Options in
  let types = [intType; uintType; longType; ulongType;
               charType; scharType; ucharType;
               TInt (IShort, []); TInt (IUShort, [])]
  in
  let fp_types =
    if Options.Float.get () then [floatType; doubleType] else [doubleType]
  in
  let types =
    if FloatingPoint.get () then fp_types @ types else types
  in
  let types =
    if LongLong.get () then [longLongType; ulongLongType] @ types else types
  in
  let types =
    if FloatingPointOnly.get () then fp_types else types
  in
  let types =
    (* There is a conflict between [-int-only] and [-float-only]; the former
       wins. There's no very good way to warn about this. *)
    if IntOnly.get () then begin
      LongLong.set false;  (* don't want [long long] to appear in modulo *)
      [intType; uintType]
    end else types
  in
  Utils.random_select types

let gen_local_var ?basename typ =
  let name = match basename with Some n -> n | None -> "v" in
  Cil.makeLocalVar !fundec name typ

let gen_formal_var ?basename typ =
  (* Only actually generate a new formal if we haven't exhausted the maximum
     number of arguments. *)
  if List.length !fundec.sformals < Options.MaxArgs.get () then begin
    let name = match basename with Some n -> n | None -> "p" in
    let vi = Cil.makeFormalVar !fundec name typ in
    if Cil.isPointerType typ then
      let mem = (Mem (Cil.evar vi), NoOffset) in
      param_lvals := LvalSet.add mem !param_lvals
    else
      param_lvals := LvalSet.add_vi vi !param_lvals;
    vi
  end else
    Utils.random_select !fundec.sformals

let gen_var ?basename ?(local_only=false) typ =
  let gen_funcs =
    if local_only then [gen_local_var] else [gen_local_var; gen_formal_var]
  in
  let f = Utils.random_select gen_funcs in
  let vi = f ?basename typ in
  (* Use the [vreferenced] field to indicate whether this variable is ever
     used. The [gen_lval_use] function below sets it. It is never reset. *)
  vi.vreferenced <- false;
  vi

let gen_vars n =
  let rec loop i acc =
    if i > 0 then
      let vi = gen_var ~local_only:true (gen_type ()) in
      loop (i - 1) (LvalSet.add_vi vi acc)
    else
      acc
  in
  loop n LvalSet.empty

let gen_return ?typ () =
  let typ = match typ with Some t -> t | None -> gen_type () in
  let var = gen_local_var ~basename:"result" typ in
  let live = LvalSet.singleton_vi var in
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

let gen_array_elt (lhost, offset as lval) =
  match Cil.typeOfLval lval with
  | TArray (_, e, _, _) ->
    let offset =
      match offset with
      | NoOffset ->
        Index (Cil.(integer ~loc (Random.int (lenOfArray e))), NoOffset)
      | _ ->
        assert false
    in
    lhost, offset
  | typ ->
    let err_msg =
      Format.asprintf
        "Expected lvalue of array type, got `%a'."
        Cil_printer.pp_typ typ
    in
    raise (Invalid_argument err_msg)

let gen_lval_occurrence lval =
  if Cil.(isArrayType (typeOfLval lval)) then
    (* Arrays are generated as a single lvalue with array type. Here we need to
       actually pick an element of the array [lval]. *)
    gen_array_elt lval
  else
    lval

(* Initialize a local variable to a constant or a parameter. *)
let gen_local_init lval =
  let gen_param_use typ =
    if not (LvalSet.is_empty !param_lvals) then
      let lval = Utils.random_select_from_set !param_lvals in
      let lval = gen_lval_occurrence lval in
      let e = Cil.new_exp ~loc (Lval lval) in
      Cil.mkCast ~force:false ~e ~newt:typ
    else
      (* OK, fall back to constants. *)
      gen_const typ
  in
  let generators = [gen_const; gen_param_use] in
  let f = Utils.random_select generators in
  let assign = Set (lval, f (Cil.typeOfLval lval), loc) in
  Cil.mkStmtOneInstr ~valid_sid:true assign

let new_lval () =
  let typ = gen_type () in
  let vi =
    if List.length !fundec.sformals < Options.MaxArgs.get () &&
       (Options.PointerArgs.get () || Options.Arrays.get ()) &&
       Random.float 1.0 < 0.1
    then
      (* Make a parameter of pointer or array type. *)
      let ptr_or_array_typ =
        let types =
          if Options.PointerArgs.get () then
            [TPtr (typ, [])]
          else
            []
        in
        let types =
          if Options.Arrays.get () then
            let length = Random.int (Options.MaxArrayLength.get ()) + 1 in
            (* One-dimensional array. *)
            TArray
              (typ,
               Some (Cil.integer ~loc length),
               { scache = Not_Computed },
               [])
            :: types
          else
            types
        in
        Utils.random_select types
      in
      gen_formal_var ptr_or_array_typ
    else
      (* Make a plain variable. *)
      let vi = gen_var typ in
      vi.vreferenced <- true;
      vi
  in
  if Cil.isPointerType vi.vtype then
    (Mem (Cil.evar vi), NoOffset)
  else
    (Var vi, NoOffset)

let gen_lval_use ~num_live () =
  (* Select a known local var or parameter of the function, or generate a
     new variable. *)
  let select_or_new vars =
    if vars <> [] then begin
      let vi = Utils.random_select vars in
      vi.vreferenced <- true;
      (Var vi, NoOffset)
    end else
      new_lval ()
  in
  let use_param () =
    if LvalSet.is_empty !param_lvals then
      new_lval ()
    else
      Utils.random_select_from_set !param_lvals
  in
  let use_local () = select_or_new !fundec.slocals in
  (* Try to generate new variables if we don't have many live ones; pick
     existing ones if enough are live. *)
  let lval =
    if Random.int (Options.MaxLive.get ()) > num_live then
      new_lval ()
    else
      let f = Utils.random_select [use_param; use_local] in
      f ()
  in
  let lval = gen_lval_occurrence lval in
  (* We never want to generate assignments to the function's formal
     parameters. This is easy: We simply never put them into the live set,
     which is where variables to assign to are selected from.
     [Utils.free_vars] only selects locals, not formals. *)
  let exp = Cil.new_exp ~loc (Lval lval) in
  let live = LvalSet.of_list (Utils.free_vars exp) in
  (live, exp)

let gen_leaf_exp ~depth ~num_live () =
  ignore depth;
  let gen_const ~num_live () =
    ignore num_live;
    let typ = gen_type () in
    (LvalSet.empty, gen_const typ)
  in
  (* Prefer variables over constants to discourage constant folding and
     propagation, i.e., to avoid making the compiler's job too easy. *)
  let f = Utils.random_select [gen_const; gen_lval_use; gen_lval_use] in
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

let choose_binop typ =
  let binops =
    if Cil.isIntegralType typ then
      (* Prefer arithmetic over bitwise operations; prefer other bitwise
         operations over shifts. *)
      let bitwise =
        if Options.Bitwise.get () then
          let shift = Utils.random_select [Shiftlt; Shiftrt] in
          let bitwise = Utils.random_select [shift; BAnd; BXor; BOr] in
          [bitwise]
        else
          []
      in
      bitwise @ [PlusA; MinusA; Mult]
    else
      [PlusA; MinusA; Mult]
  in
  let binops =
    if Options.DivMod.get () then
      if Cil.isIntegralType typ then
        [Div] @ (if Options.Mod.get () then [Mod] else []) @ binops
      else
        [Div] @ binops
    else
      binops
  in
  Utils.random_select binops

(* For some operations, it's best to patch the RHS operand to avoid some
   common problems. *)
let fixup_binop_rexp op lexp rexp =
  match op with
  | Shiftlt | Shiftrt ->
    (* Generate a bit mask expression to transform this operand into the
       legal range. *)
    let lhs_bitsize = Cil.bitsSizeOf (Cil.typeOf lexp) in
    let mask = Cil.integer ~loc (lhs_bitsize - 1) in
    Cil.mkBinOp ~loc BAnd rexp mask
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

let gen_binop_for (llive, lexp) (rlive, rexp) =
  let lexp, rexp = gen_common_type_exprs lexp rexp in
  let op = choose_binop (Cil.typeOf lexp) in
  let rexp' = fixup_binop_rexp op lexp rexp in
  let live = LvalSet.union llive rlive in
  (live, Cil.mkBinOp ~loc op lexp rexp')

let rec gen_exp ~depth ~num_live () =
  let generators =
    if depth <= Options.ExprDepth.get () then
      (* Prefer binary operations over all other kinds. *)
      [gen_leaf_exp; gen_binop; gen_binop; gen_unop]
    else
      [gen_leaf_exp]
  in
  let f = Utils.random_select generators in
  let live, exp = f ~depth ~num_live () in
  (* We sometimes generate expressions that constant fold to floating point
     infinities. These would get printed as "inff" and cause compiler
     errors. In such cases, try again. *)
  if Utils.is_infinity exp then
    gen_exp ~depth ~num_live ()
  else
    live, exp

and gen_binop ~depth ~num_live () =
  let depth = depth + 1 in
  let llive, lexp = gen_exp ~depth ~num_live () in
  let rlive, rexp = gen_exp ~depth ~num_live () in
  gen_binop_for (llive, lexp) (rlive, rexp)

and gen_unop ~depth ~num_live () =
  let depth = depth + 1 in
  let live, exp = gen_exp ~depth ~num_live () in
  let typ = Cil.typeOf exp in
  let unops =
    if Cil.isIntegralType typ && Options.Bitwise.get () then
      [Neg; BNot; LNot]
    else
      [Neg]
  in
  let op = Utils.random_select unops in
  (live, Cil.new_exp ~loc (UnOp (op, exp, typ)))

let gen_exp ~num_live ?typ () =
  let live, exp = gen_exp ~depth:1 ~num_live () in
  match typ with
  | Some typ -> (live, Cil.mkCast ~force:false ~e:exp ~newt:typ)
  | None -> (live, exp)

let gen_cond ~num_live () =
  (* Make sure a condition always uses at least one variable. *)
  let rec make_nonconst_exprs () =
    let llive, lexp = gen_exp ~num_live () in
    let rlive, rexp = gen_exp ~num_live () in
    let live = LvalSet.union llive rlive in
    if LvalSet.is_empty live then
      make_nonconst_exprs ()
    else
      let lexp, rexp = gen_common_type_exprs lexp rexp in
      (live, lexp, rexp)
  in
  let live, lexp, rexp = make_nonconst_exprs () in
  let comparisons = [Lt; Gt; Le; Ge; Eq; Ne] in
  let cmp = Utils.random_select comparisons in
  (live, Cil.mkBinOp ~loc cmp lexp rexp)

let gen_assignment_to lval ~depth ~live () =
  ignore depth;
  let num_live = LvalSet.cardinal live in
  let typ = Cil.typeOfLval lval in
  let (new_live_vars, exp) = gen_exp ~num_live ~typ () in
  let live =
    LvalSet.union (LvalSet.remove lval live) new_live_vars
  in
  let assign = Set (lval, exp, loc) in
  let stmt = Cil.mkStmtOneInstr ~valid_sid:true assign in
  (live, stmt)

let gen_assignment ~depth ~live () =
  let vi = Utils.random_select_from_set live in
  gen_assignment_to vi ~depth ~live ()

let rec gen_stmt ~depth ~live () =
  let generators =
    if depth < Options.StmtDepth.get () then
      let gens = [gen_assignment; gen_if_stmt] in
      if Options.Loops.get () then [gen_loop] @ gens else gens
    else
      [gen_assignment]
  in
  let f = Utils.random_select generators in
  f ~depth ~live ()

and gen_if_stmt ~depth ~live () =
  let depth = depth + 1 in
  let (true_live, true_stmts) = gen_stmts ~depth ~live ~tail:[] () in
  let (false_live, false_stmts) = gen_stmts ~depth ~live ~tail:[] () in
  let body_live = LvalSet.union true_live false_live in
  let num_live = LvalSet.cardinal body_live in
  let (cond_new_live, cond) = gen_cond ~num_live () in
  let live = LvalSet.union body_live cond_new_live in
  let if_stmt =
    Cil.mkStmt ~valid_sid:true
      (If (cond, Cil.mkBlock true_stmts, Cil.mkBlock false_stmts, loc))
  in
  (live, if_stmt)

and gen_loop ~depth ~live () =
  let while_loop =
    if Options.WhileLoops.get () then [gen_while_loop] else []
  in
  (* We can only generate a for loop if we can generate a corresponding
     array argument. *)
  let generators =
    if Options.ForLoops.get () &&
       List.length !fundec.sformals < Options.MaxArgs.get () then
      [gen_for_loop] @ while_loop
    else
      while_loop
  in
  let generators =
    (* We may have found a combination of command line arguments and program
       state that doesn't allow us to generate a loop after all. In that
       case, try some other statement. *)
    if generators = [] then [gen_stmt] else generators
  in
  let generator = Utils.random_select generators in
  generator ~depth ~live ()

and gen_for_loop ~depth ~live () =
  ignore depth;
  (* loop counter *)
  let i = gen_local_var ~basename:"i" Cil.uintType in
  (* array parameter *)
  let elem_typ = gen_type () in
  let ptr_typ = TPtr (elem_typ, []) in
  let a = gen_formal_var ~basename:"arr" ptr_typ in
  (* Hack: [gen_formal_var] put [a] into the set of usable formals, but we
     only ever want to use this array in the context of this loop, so remove
     it from there. *)
  param_lvals := LvalSet.remove (Var a, NoOffset) !param_lvals;
  (* The loop body will be of the form
        x = arr[i];
        r = r op exp(x);
     for some random expression [exp] and a random binary operator [op].
     That is, we map [exp] over the array, then reduce (fold) with [op], and
     put the result in [r]. *)
  (* Result of the loop computation. *)
  let r = Utils.random_select_from_set live in
  let live' = LvalSet.remove r live in
  let num_live = LvalSet.cardinal live' in
  (* The last statement of the block is an assignment to this result
     variable, combining its old value with some new, nontrivial expression
     that will be made to use a reference to [a[i]]. *)
  let rec gen_nonconst_rexp () =
    let (rlive, rexp) = gen_exp ~num_live ~typ:(Cil.typeOfLval r) () in
    if Utils.free_vars rexp = [] then
      gen_nonconst_rexp ()
    else
      (rlive, rexp)
  in
  let (rlive, rexp) = gen_nonconst_rexp () in
  let (llive, lexp) = (LvalSet.empty, Cil.new_exp ~loc (Lval r)) in
  let assign_live, assign_exp = gen_binop_for (llive, lexp) (rlive, rexp) in
  let assign_stmt =
    Cil.mkStmtOneInstr ~valid_sid:true (Set (r, assign_exp, loc))
  in
  let use_var = Utils.random_select (Utils.free_vars rexp) in
  let use_stmt =
    let array_elem =
      Cil.mkMem
        ~addr:(Cil.mkBinOp ~loc PlusPI (Cil.evar a) (Cil.evar i))
        ~off:NoOffset
    in
    let array_elem_exp = Cil.new_exp ~loc (Lval array_elem) in
    Cil.mkStmtOneInstr ~valid_sid:true (Set (use_var, array_elem_exp, loc))
  in
  (* The body consists of these two assignments. *)
  let use_live = LvalSet.remove r assign_live in
  let (body_live, body) = (use_live, [use_stmt; assign_stmt]) in
  (* Finally, make a loop for i from 0 to N over this body. *)
  let for_stmt_list =
    Cil.mkForIncr
      ~iter:i
      ~first:(Cil.mkCast ~force:true ~e:(Cil.zero ~loc) ~newt:Cil.uintType)
      ~stopat:(array_size ())
      ~incr:(Cil.one ~loc)
      ~body
  in
  let for_stmt =
    Cil.mkStmt ~valid_sid:true (Block (Cil.mkBlock for_stmt_list))
  in
  (* Now, the variables live before the loop are the variables live into the
     body, as well as [r], since it must be initialized. And, of course,
     whatever is live after the body. *)
  let live = LvalSet.union (LvalSet.add r body_live) live' in
  (live, for_stmt)

and gen_while_loop ~depth ~live () =
  let depth = depth + 1 in
  (* Save the variables that are live after the loop. They may be defined in
     the loop body, but must nevertheless still be live before the loop. *)
  let live_out = live in
  let num_live = LvalSet.cardinal live in
  let (cond_new_live, cond) = gen_cond ~num_live () in
  (* Generate a set of newly used variables in the loop. Some of these will
     be loop-carried dependences. *)
  let num_live = num_live + LvalSet.cardinal cond_new_live in
  let n =
    if num_live >= Options.MaxLive.get () then
      Random.int 2 + 1
    else
      Random.int ((Options.MaxLive.get () + 1 - num_live) / 2) + 1
  in
  let body_new_live = gen_vars n in
  let body_live_out =
    LvalSet.union live_out
      (LvalSet.union cond_new_live body_new_live)
  in
  let live = body_live_out in
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
  let (body_live_in, stmts) = gen_stmts ~depth ~live ~tail () in
  (* Now we must ensure that every variable in [body_new_live] does actually
     have a use before a possible redefinition in the loop. If there is a
     redefinition but no use before it, the variable is not live into the
     body. Otherwise, the variable is not used at all. Both conditions are
     checked here. *)
  let referenced_vi = function
    | Var vi, _ -> vi.vreferenced
    | _ -> false
  in
  let need_use =
    LvalSet.filter
      (fun b -> not (LvalSet.mem b body_live_in) || not (referenced_vi b))
      body_new_live
  in
  let (body_live_in', stmts') =
    if LvalSet.is_empty need_use then
      (body_live_in, stmts)
    else begin
      (* Make an assignment using all the variables in [need_use]. *)
      let (newly_live, exp) =
        LvalSet.fold
          (fun lval acc ->
             let exp = Cil.new_exp ~loc (Lval lval) in
             gen_binop_for (LvalSet.singleton lval, exp) acc)
          need_use
          (LvalSet.empty, gen_const (gen_type ()))
      in
      assert (LvalSet.equal newly_live need_use);
      (* For the LHS choose a currently live variable. If there is none,
         things get a bit more annoying. *)
      let (stmts, lval) =
        if LvalSet.is_empty body_live_in then begin
          (* The only way everything could have been killed is through an
             assignment to some variable. Pick that variable and remove the
             assignment. *)
          match stmts with
          | { skind = Instr (Set (lval, _exp, _)) } :: stmts ->
            (stmts, lval)
          | _ -> assert false
        end else
          (stmts, Utils.random_select_from_set body_live_in)
      in
      let body_live_in' =
        LvalSet.union (LvalSet.remove lval body_live_in) newly_live
      in
      let assign = Set (lval, exp, loc) in
      let assign_stmt = Cil.mkStmtOneInstr ~valid_sid:true assign in
      (body_live_in', assign_stmt :: stmts)
    end
  in
  let loop = Cil.mkLoop ~sattr:[] ~guard:cond ~body:stmts' in
  let live =
    LvalSet.union
      (LvalSet.union cond_new_live body_live_in')
      live_out
  in
  (live, Cil.mkStmt ~valid_sid:true (Block (Cil.mkBlock loop)))

and gen_stmts ?n ~depth ~live ~tail () =
  let rec rec_gen_stmts n ~live ~tail =
    (* Stop when we have generated [n] statements or when we have no more
       live variables to define. *)
    if n < 1 || LvalSet.is_empty live then
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
  let glob_or_formal_vi = function
    | Var vi, _ -> vi.vglob || vi.vformal
    | _ -> false
  in
  LvalSet.iter
    (fun vi ->
       if not (glob_or_formal_vi vi) then
         let init = gen_local_init vi in
         !fundec.sbody.bstmts <- init :: !fundec.sbody.bstmts)
    live';
  (* Register the function definition as global. *)
  !fundec.svar.vdefined <- true;
  Globals.Functions.replace_by_definition !fundec.sspec !fundec loc;
  (* Compute the control-flow graph of the function to ensure a well-formed AST.
     This is typically needed when several analyses are chained in Frama-C. *)
  Cfg.prepareCFG !fundec;
  Cfg.cfgFun !fundec;
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
  let global_array_size_var =
    match !array_size_var with
    | Some vi ->
      Globals.Vars.add vi { init = None };
      [GVarDecl (vi, loc)]
    | None -> []
  in
  ast.globals <- global_array_size_var @ f :: ast.globals

let gen_header ast =
  let header = Format.asprintf
    "// Generated by ldrgen\n\
     // Seed: %d\n\
     // Command line arguments:%a"
    (Options.Seed.get ()) Utils.print_command_line_args ()
  in
  ast.globals <- GText header :: ast.globals

let run () =
  if Options.Run.get () then begin
    Options.initialize ();
    let prj = Project.create_by_copy ~last:true "ldrgen" in
    Project.on
      prj
      (fun () ->
         let ast = Ast.get () in
         gen_random_function ast;
         gen_header ast;
         if Options.CheckAst.get () then
           Filecheck.check_ast ~is_normalized:true ~ast "ldrgen";
         Ast.mark_as_grown ();
         Format.printf "%a" Printer.pp_file ast;
         let selection = State_selection.with_dependencies Options.Run.self in
         Project.clear ~selection ~project:prj ())
      ()
  end

let () =
  Db.Main.extend run
