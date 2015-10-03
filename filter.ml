open Asttypes
open Parsetree
open Longident
open Ast_mapper
open Ast_helper
open Location
open Batteries

let rec string_of_expr = function
  | Pexp_ident ident -> "Pexp_ident: " ^ (Longident.flatten ident.txt |> String.concat " ")
  | Pexp_constant const ->
      "Pexp_constant: " ^
      begin match const with
      | Const_int i -> string_of_int i
      | Const_char c -> Printf.sprintf "'%s'" (Char.escaped c)
      | Const_string (s, opt) ->
          begin match opt with
          | None -> Printf.sprintf "\"%s\"" s
          | Some ss -> ss ^ s ^ s
          end
      | Const_float s -> s
      | Const_int32 i32 -> Int32.to_string i32
      | Const_int64 i64 -> Int64.to_string i64
      | Const_nativeint i -> Nativeint.to_string i
      end
  | _ -> "not implement yet"

let rec string_of_expr_desc = function
  | Pexp_ident ident -> (Longident.flatten ident.txt |> String.concat ".")
  | Pexp_constant const ->
      begin match const with
      | Const_int i -> string_of_int i
      | Const_char c -> Printf.sprintf "'%s'" (Char.escaped c)
      | Const_string (s, opt) ->
          begin match opt with
          | None -> Printf.sprintf "\"%s\"" s
          | Some ss -> Printf.sprintf "\"%s%s%s\"" ss s s
          end
      | Const_float s -> s
      | Const_int32 i32 -> Int32.to_string i32
      | Const_int64 i64 -> Int64.to_string i64
      | Const_nativeint i -> Nativeint.to_string i
      end
  | _ -> "not implement yet"

(* ppx_tools/ast_convenience.ml *)
let may_tuple tup = function
  | [] -> None
  | [x] -> Some x
  | l -> Some (tup ?loc:None ?attrs:None l)

let lid s = mkloc (Longident.parse s) !default_loc
let const_string str = Exp.constant @@ Const_string (str, None)
let constr s args = Exp.construct (lid s) (may_tuple Exp.tuple args)
let nil () = constr "[]" []
let unit () = constr "()" []

let gensym =
  let counter = ref 0 in
  fun () ->
    incr counter;
    Printf.sprintf "ower_assert_gensym_%d" !counter

let expr_of_eprintf ~loc format args =
  Exp.apply ~loc
    (Exp.ident ~loc @@ lid "Printf.eprintf")
    (("", const_string format) :: args)

let expr_of_apply_arg expr =
  ("", const_string @@ string_of_expr_desc expr.pexp_desc)

let expr_puts_syms ~loc syms args =
  List.map2 (fun sym (_, expr) ->
    expr_of_eprintf ~loc "%s -> %s\n"
        [expr_of_apply_arg expr;
         ("", (Exp.apply ~loc (Exp.ident ~loc @@ lid "Batteries.dump") ["", Exp.ident ~loc @@ lid sym]))])
  syms args

let rec buid_seq_expr ~loc = function
  | [] -> Exp.assert_ ~loc @@ constr "false" []
  | expr::exprs -> buid_seq_expr ~loc exprs |> Exp.sequence ~loc expr

let put_assert_expr ~loc f args =
  let assert_expr =
    expr_of_eprintf ~loc "assert %s" [expr_of_apply_arg f]
  in
  let args_exprs =
    List.map (fun (_, expr)->
      expr_of_eprintf ~loc " %s" [expr_of_apply_arg expr])
      args
  in
  assert_expr::args_exprs

let rewrite_assert f args loc =
  let syms = List.map (fun _ -> gensym ()) args in
  let vars = List.map2 (fun (_, expr) sym ->
        Vb.mk ~loc (Pat.var ~loc {txt = sym; loc}) expr)
        args syms
  in
  let assert_expr = put_assert_expr ~loc f args in
  Exp.let_ ~loc Nonrecursive
    vars
    (Exp.ifthenelse ~loc
      (Exp.apply ~loc
        f
        (List.map (fun sym ->
          ("", (Exp.ident ~loc {txt = Lident sym; loc})))
          syms))
      (unit ())
      (Some (buid_seq_expr ~loc
        (List.concat [
          assert_expr;
          [expr_of_eprintf ~loc "\n" []];
          expr_puts_syms ~loc syms args]))))

let filter argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | {pexp_desc = Pexp_assert e; pexp_loc} ->
          begin match e with
            | {pexp_desc = Pexp_apply (f, args)} ->
                rewrite_assert f args pexp_loc
            | x -> default_mapper.expr mapper x
          end
      | x -> default_mapper.expr mapper x
  }

let () = register "ower_assert" filter
