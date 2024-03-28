(*
 * Call-by-value reduction   
 *)

exception NotImplemented
exception Stuck

let freshVarCounter = ref 0

(*   getFreshVariable : string -> string 
 *   use this function if you need to generate a fresh variable from s. 
 *)
let getFreshVariable s =
  let _ = freshVarCounter := !freshVarCounter + 1 in
  s ^ "__" ^ string_of_int !freshVarCounter

(*
 * get the list of free variables in the expression e.
 *)
let rec fV (e : Uml.exp) =
  match e with
  | Uml.Var x -> [ x ]
  | Uml.Lam (x, e) -> List.filter (fun y -> y <> x) (fV e)
  | Uml.App (e1, e2) -> fV e1 @ fV e2

(*
 * swap the variable x with the variable y in the expression e.
 *)
let rec variableSwap (x : Uml.var) (y : Uml.var) (e : Uml.exp) =
  match e with
  | Uml.Var z -> if z = x then Uml.Var y else Uml.Var z
  | Uml.Lam (z, e) ->
      if z = x then Uml.Lam (z, e)
      else if z = y then Uml.Lam (z, e)
      else Uml.Lam (z, variableSwap x y e)
  | Uml.App (e1, e2) -> Uml.App (variableSwap x y e1, variableSwap x y e2)

(*
 * get a new variable that is not in the list lst.
 *)
let getNewVariableNotInList lst =
  let rec getUniqueVariable s =
    let str = getFreshVariable s in
    if List.mem str lst then getUniqueVariable str else str
  in
  getUniqueVariable "s"

(*
 * implement the substitution function [e'/x]e.
 *)
let rec substitute (e' : Uml.exp) (x : string) (e : Uml.exp) =
  (* Printf.printf "===sub: %s | %s | %s\n" (Inout.exp2string e') x
     (Inout.exp2string e); *)
  match (e', x, e) with
  | e', x, Uml.Var y -> if x = y then e' else Uml.Var y
  | e', x, Uml.App (e1, e2) -> Uml.App (substitute e' x e1, substitute e' x e2)
  | e', x, Uml.Lam (y, e) ->
      if x = y then Uml.Lam (y, e)
      else if List.mem y (fV e') then
        let z = getNewVariableNotInList ([ x ] @ [ y ] @ fV e' @ fV e) in
        Uml.Lam (z, substitute (variableSwap z y e') x e)
      else Uml.Lam (y, substitute e' x e)

(*
 * implement a single step with reduction using the call-by-value strategy.
 *)
let rec stepv e : Uml.exp =
  (* Printf.printf "===Value of e: %s\n" (Inout.exp2string e); *)
  match e with
  | Uml.Var x -> raise Stuck
  | Uml.Lam (x, e) -> raise Stuck
  | Uml.App (Uml.Var x, e2) -> raise Stuck
  | Uml.App (Uml.Lam (x, e1), Uml.Var y) -> substitute (Uml.Var y) x e1
  | Uml.App (Uml.Lam (x, e1), Uml.Lam (y, e2)) ->
      substitute (Uml.Lam (y, e2)) x e1
  | Uml.App (Uml.Lam (x, e1), e2) -> Uml.App (Uml.Lam (x, e1), stepv e2)
  | Uml.App (e1, e2) -> Uml.App (stepv e1, e2)

let stepOpt stepf e = try Some (stepf e) with Stuck -> None

let rec multiStep stepf e = try multiStep stepf (stepf e) with Stuck -> e

let stepStream stepf e =
  let rec steps e =
    match stepOpt stepf e with
    | None -> Stream.from (fun _ -> None)
    | Some e' -> Stream.icons e' (steps e')
  in
  Stream.icons e (steps e)
