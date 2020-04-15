exception Error of string
let error fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt
let bind env name value f =
  Hashtbl.add env name value;
  let result = f value in
  Hashtbl.remove env name;
  result

(* Locations *)
type loc = { line: int; col: int }
type buf = { data: string; pos: int; loc: loc; }

let buf_of_string data =
  { data; pos = 0; loc = { line = 1; col = 1 } }
let buf_sub buf count =
  String.sub buf.data buf.pos count
let advance buf =
  { buf with
    pos = buf.pos + 1;
    loc = if buf.data.[buf.pos] = '\n'
          then { line = buf.loc.line + 1; col = 1 }
          else { buf.loc with col = buf.loc.col + 1 } }
let error_loc loc fmt = error ("%d:%d: " ^^ fmt) loc.line loc.col

(* Lexer *)
type token =
| LInt of int
| LId  of string
| LKw  of string
| LEof

let show_token token =
  match token with
  | LInt n -> Printf.sprintf "int %d"  n
  | LId id -> Printf.sprintf "id '%s'" (String.escaped id)
  | LKw kw -> Printf.sprintf "kw '%s'" (String.escaped kw)
  | LEof   -> "EOF"
let show_loc_token (loc, token) =
  Printf.sprintf "%s @ %d:%d" (show_token token) loc.line loc.col
let show_loc_tokens loc_tokens =
  String.concat "; " (List.map show_loc_token loc_tokens)

let classify buf =
  if buf.pos = String.length buf.data then `Eof
  else match buf.data.[buf.pos] with
  | '0'..'9' -> `Digit
  | '_' | 'a'..'z' | 'A'..'Z' -> `Alpha
  | '=' | '<' | '>' | '+' | '-' | '*' | '/' | '%' -> `Punct
  | '(' | ')' -> `Delim
  | ' ' | '\t' | '\r' | '\n' -> `Space
  | c -> error_loc buf.loc "unexpected symbol '%c'" c

let keywords = ["let"; "fun"; "in"; "true"; "false"]

let lex buf =
  let rec chunk classes accum buf f =
    if List.mem (classify buf) classes then
      chunk classes (accum ^ buf_sub buf 1) (advance buf) f
    else f accum buf
  and token rest buf =
    match classify buf with
    | `Digit -> chunk [`Digit] "" buf (fun sub ->
        token ((buf.loc, LInt (int_of_string sub)) :: rest))
    | `Alpha -> chunk [`Alpha;`Digit] "" buf (fun sub ->
        token ((buf.loc, if List.mem sub keywords then LKw sub else LId sub) :: rest))
    | `Punct -> chunk [`Punct] "" buf (fun sub ->
        token ((buf.loc, LKw sub) :: rest))
    | `Delim -> token ((buf.loc, LKw (buf_sub buf 1)) :: rest) (advance buf)
    | `Space -> token rest (advance buf)
    | `Eof -> ((buf.loc, LEof) :: rest)
  in List.rev (token [] buf)

(* Parser *)
type parsenode =
| PLit of [ `Int of int | `Bool of bool ]
| PVar of string
| PFun of string * parsetree
| PApp of parsetree * parsetree
| PLet of string * parsetree * parsetree
and parsetree = { loc: loc; desc: parsenode }

let rec show_parsetree parsetree =
  match parsetree.desc with
  | PLit (`Int n) -> string_of_int n
  | PLit (`Bool b) -> string_of_bool b
  | PVar var -> var
  | PFun (arg, body) -> Printf.sprintf "(fun %s -> %s)" arg (show_parsetree body)
  | PApp (func, arg) -> Printf.sprintf "(%s %s)" (show_parsetree func) (show_parsetree arg) ^ ")"
  | PLet (var, expr, body) ->
    Printf.sprintf "(let %s = %s in %s)" var (show_parsetree expr) (show_parsetree body)

let parse =
  let make ~loc desc = { loc; desc } in
  let expected what tokens =
    match tokens with
    | (loc, token) :: _ -> error "expected %s, got %s" what (show_token token)
    | [] -> assert false
  in
  let expected_token what = expected (show_token what) in
  let rec expr_highest f tokens =
    match tokens with
    | (loc, LInt n) :: tokens ->
      tokens |> f (make ~loc (PLit (`Int n)))
    | (loc, LKw "true") :: tokens ->
      tokens |> f (make ~loc (PLit (`Bool true)))
    | (loc, LKw "false") :: tokens ->
      tokens |> f (make ~loc (PLit (`Bool false)))
    | (loc, LId var) :: tokens ->
      tokens |> f (make ~loc (PVar var))
    | (loc, LKw "fun") :: (_, LId arg) :: (_, LKw "->") :: tokens ->
      tokens |> expr_lowest (fun body -> f (make ~loc (PFun (arg, body))))
    | (_, LKw "fun") :: (_, LId arg) :: tokens -> tokens |> expected_token (LKw "->")
    | (_, LKw "fun") :: tokens -> tokens |> expected "identifier"
    | (loc, LKw "let") :: (_, LId var) :: (_, LKw "=") :: tokens ->
      tokens |> expr_lowest (fun expr tokens ->
        match tokens with
        | (_, LKw "in") :: tokens ->
          tokens |> expr_lowest (fun body -> f (make ~loc (PLet (var, expr, body))))
        | _ -> tokens |> expected_token (LKw "in"))
    | (_, LKw "let") :: (_, LId var) :: tokens -> tokens |> expected_token (LKw "=")
    | (_, LKw "let") :: tokens -> tokens |> expected "identifier"
    | (_, LKw "(") :: tokens ->
      tokens |> expr_lowest (fun expr tokens ->
        match tokens with
        | (_, LKw ")") :: tokens -> tokens |> f expr
        | _ -> tokens |> expected_token (LKw ")"))
    | _ -> tokens |> expected "expression"
  and expr_apply f =
    let rec apply_lassoc lhs tokens =
      match tokens with
      | (_, (LInt _ | LId _ | LKw "(")) :: _ ->
        tokens |> expr_highest (fun rhs ->
          apply_lassoc (make ~loc:rhs.loc (PApp (lhs, rhs))))
      | _ -> tokens |> f lhs
    in expr_highest apply_lassoc
  and expr_binop_rassoc ops expr_lower f =
    expr_lower (fun lhs tokens ->
      match tokens with
      | (loc, LKw op) :: tokens when List.mem op ops ->
        tokens |> expr_binop_rassoc ops expr_lower (fun rhs ->
          f (make ~loc (PApp (make ~loc (PApp (make ~loc (PVar op), lhs)), rhs))))
      | _ -> tokens |> f lhs)
  and expr_binop_mul f =
    expr_binop_rassoc ["*"; "/"; "%"] expr_apply f
  and expr_binop_add f =
    expr_binop_rassoc ["+"; "-"] expr_binop_mul f
  and expr_binop_cmp f =
    expr_binop_rassoc ["="; "<="; ">="; "<"; ">"; "<>"] expr_binop_add f
  and expr_lowest f =
    expr_binop_cmp f
  in
  expr_lowest (fun expr tokens ->
    match tokens with
    | [(_, LEof)] -> expr
    | _ -> tokens |> expected "EOF")

(* Types *)
type typ =
| YInt
| YBool
| YFun  of typ * typ
| YVar  of tvar
| YPoly of tvar * typ
and tvar = { mutable link: typ; scope: int }

let scope = ref 0
let fresh_type () =
  let rec typ = YVar { link = typ; scope = !scope } in
  incr scope; typ
let rec find_type typ =
  match typ with (* Note the use of physical inequality (!=) here. *)
  | YVar tv when tv.link != typ ->
    tv.link <- find_type tv.link; tv.link
  | _ -> typ

let with_names f =
  let names = Hashtbl.create 16 in f names
let show_tvar names tvar =
  if not (Hashtbl.mem names tvar) then
    Hashtbl.add names tvar (string_of_int (Hashtbl.length names));
  "'" ^ (Hashtbl.find names tvar)
let rec show_type names typ =
  let show_tvar, show_type = show_tvar names, show_type names in
  match find_type typ with
  | YInt -> "int"
  | YBool -> "bool"
  | YFun (a, r) ->
    (match find_type a with
    | YFun (a', r') -> "(" ^ (show_type a') ^ " -> " ^ (show_type r') ^ ")"
    | _ -> show_type a) ^ " -> " ^ (show_type r)
  | YVar tv -> show_tvar tv
  | YPoly (tv, t) -> (*forall*) (show_tvar tv) ^ ". " ^ (show_type t)

let unify loc ua ub =
  let rec unify_one ta tb =
    match find_type ta, find_type tb with
    | YVar va, YVar vb when va.scope < vb.scope -> vb.link <- ta
    | YVar va, YVar vb when vb.scope < va.scope -> va.link <- tb
    | YVar va, _ -> va.link <- tb
    | _, YVar _ -> unify_one tb ta
    | YInt, YInt -> ()
    | YFun (aa, ar), YFun (ba, br) -> unify_one aa ba; unify_one ar br
    | _ -> with_names (fun names ->
        error_loc loc "cannot unify %s with %s" (show_type names ua) (show_type names ub))
  in unify_one ua ub

let generalize outer typ =
  let rec collect accum typ =
    match find_type typ with
    (* The occurs-free check in HM is i.e. an escape check, and we unify to the outermost tvar. *)
    | YVar tv when tv.scope < outer -> accum
    | YVar tv when List.memq tv accum -> accum
    | YVar tv -> tv :: accum
    | YInt -> accum
    | YBool -> accum
    | YFun (a, r) -> collect (collect accum a) r
    | YPoly _ -> assert false
  in List.fold_left (fun typ tv -> YPoly (tv, typ)) typ (collect [] typ)

let instantiate typ =
  let rec subst assoc typ =
    match find_type typ with
    | YVar tv when List.mem_assq tv assoc -> List.assq tv assoc
    | YVar _ -> typ
    | YPoly (tv, typ) -> subst ((tv, fresh_type ()) :: assoc) typ
    | YInt -> YInt
    | YBool -> YBool
    | YFun (a, r) -> YFun (subst assoc a, subst assoc r)
  in subst [] typ

(* Type checker *)
type typednode =
| TLit of [ `Int of int | `Bool of bool ]
| TVar of string
| TFun of string * typedtree
| TApp of typedtree * typedtree
| TLet of string * typedtree * typedtree
and typedtree = { loc: loc; typ: typ; desc: typednode }

let rec show_typedtree names typedtree =
  let show_type, show_typedtree = show_type names, show_typedtree names in
  match typedtree with
  | { desc = TLit (`Int n) } -> string_of_int n
  | { desc = TLit (`Bool n) } -> string_of_bool n
  | { desc = TVar var } -> var
  | { desc = TFun (arg, body); typ } ->
    Printf.sprintf "(fun{%s} %s -> %s)" (show_type typ) arg (show_typedtree body)
  | { desc = TApp (func, arg) } ->
    Printf.sprintf "(%s{%s} %s{%s})" (show_typedtree func) (show_type func.typ)
      (show_typedtree arg) (show_type arg.typ)
  | { desc = TLet (var, expr, body) } ->
    Printf.sprintf "(let %s = %s{%s} in %s{%s})" var (show_typedtree expr) (show_type expr.typ)
      (show_typedtree body) (show_type body.typ)

let rec typeck tenv (pexpr:parsetree) =
  let typeck = typeck tenv in
  let make ~loc desc typ = { loc; typ; desc } in
  match pexpr with
  | { loc; desc = PLit value } ->
    make ~loc (TLit value) (match value with
    | `Int _ -> YInt
    | `Bool b -> YBool)
  | { loc; desc = PVar var } ->
    (match Hashtbl.find_opt tenv var with
    | Some typ -> make ~loc (TVar var) (instantiate typ)
    | None -> error_loc pexpr.loc "unbound variable '%s'" var)
  | { loc; desc = PFun (parg, pbody) } ->
    bind tenv parg (fresh_type ()) (fun arg_typ ->
      let tbody = typeck pbody in
      make ~loc (TFun (parg, tbody)) (YFun (arg_typ, tbody.typ)))
  | { loc; desc = PApp (pfunc, parg) } ->
    let tfunc = typeck pfunc in
    let targ = typeck parg in
    let app_typ = fresh_type () in
    unify loc tfunc.typ (YFun (targ.typ, app_typ));
    make ~loc (TApp (tfunc, targ)) app_typ
  | { loc; desc = PLet (var, pexpr, pbody) } ->
    let outer = !scope in
    let tvalue = typeck pexpr in
    bind tenv var (generalize outer tvalue.typ) (fun var_typ ->
      let tbody = typeck pbody in
      make ~loc (TLet (var, tvalue, tbody)) tbody.typ)

(* Interpreter *)
type value =
| VInt  of int
| VBool of bool
| VFun  of (string, value) Hashtbl.t * string * typedtree
| VPrim of (value -> value)

let show_value value =
  match value with
  | VInt value -> Printf.sprintf "%d" value
  | VBool value -> Printf.sprintf "%s" (string_of_bool value)
  | VFun (env, arg, tbody) ->
    Printf.sprintf "<fun %s -> %s>" arg (with_names show_typedtree tbody)
  | VPrim _ -> "<prim>"

let lift1 f = VPrim (fun x -> f x)
let lift2 f = VPrim (fun x -> (VPrim (fun y -> f x y)))

let rec interp venv texpr =
  match texpr.desc with
  | TLit (`Int value) -> VInt value
  | TLit (`Bool value) -> VBool value
  | TVar var -> Hashtbl.find venv var
  | TFun (arg, tbody) -> VFun (Hashtbl.copy venv, arg, tbody)
  | TApp (tfunc, targ) ->
    (match interp venv tfunc, interp venv targ with
    | VFun (venv', arg, tbody), varg ->
      bind venv' arg varg (fun varg ->
        interp venv' tbody)
    | VPrim prim, varg -> prim varg
    | _ -> assert false)
  | TLet (var, texpr, tbody) ->
    bind venv var (interp venv texpr) (fun vexpr ->
      interp venv tbody)

(* Builtins *)
let builtins =
  let binary  typ name op =
    name, YFun (typ, YFun (typ, typ)), lift2 (fun lhs rhs ->
      match lhs, rhs with
      | VInt l, VInt r -> VInt (op l r)
      | _ -> assert false)
  and compare typ name op =
    name, YFun (typ, YFun (typ, YInt)), lift2 (fun lhs rhs ->
      match lhs, rhs with
      | VInt l, VInt r -> VInt (if op l r then 1 else 0)
      | _ -> assert false)
  in [
    binary YInt "+"  (+);  binary YInt "-" (-);
    binary YInt "*" ( * ); binary YInt "/" (/); binary YInt "%" (mod);
    compare YInt "<"  (<);  compare YInt ">"  (<);  compare YInt "="  (=);
    compare YInt "<=" (<=); compare YInt ">=" (<=); compare YInt "<>" (<>);
  ]
let builtin_tenv () =
  let env = Hashtbl.create 16 in
  List.iter (fun (name, typ, prim) -> Hashtbl.add env name typ) builtins; env
let builtin_venv () =
  let env = Hashtbl.create 16 in
  List.iter (fun (name, typ, prim) -> Hashtbl.add env name prim) builtins; env

(* Driver *)
let read_file filename =
  let file = open_in filename in
  let data = really_input_string file (in_channel_length file) in
  close_in file;
  data

let () =
  try
    let input = buf_of_string (read_file Sys.argv.(2)) in
    match Sys.argv.(1) with
    | "lex" ->
      let loc_tokens = lex input in
      print_endline (show_loc_tokens loc_tokens)
    | "parse" ->
      let parsetree = parse (lex input) in
      print_endline (show_parsetree parsetree)
    | "typeck" ->
      let typedtree = typeck (builtin_tenv ()) (parse (lex input)) in
      with_names (fun names ->
        print_endline (show_typedtree names typedtree);
        prerr_endline (show_type names typedtree.typ))
    | "interp" ->
      let value = interp (builtin_venv ()) (typeck (builtin_tenv ()) (parse (lex input))) in
      print_endline (show_value value)
    | action -> error "Unknown action '%s'" action
  with Error desc ->
    prerr_endline desc; exit 1
