open Sqlsyntax
(*
  Compile
*)
let rec compile q t = 
   match q with

   | CreateTableStmt(tbl_name, column_def) ->
      Printf.printf "\n";
      Printf.printf ";-----------------------------\n";
      Printf.printf "; Table Definition: %s \n" tbl_name ;
      Printf.printf ";-----------------------------\n";

      (* カラムの型定義 *)
      column_def_list tbl_name column_def t;

      (* カラムの制約定義 *)
      column_constraint_list  tbl_name column_def t

   | SelectStmt(select_opts, select_expr_list, table_references, opt_where, opt_groupby, opt_having, opt_orderby) ->
      Printf.printf "\n";
      Printf.printf ";-----------------------------\n";
      Printf.printf "; Select Statement\n";
      Printf.printf ";-----------------------------\n";
      
      (* From Clause *)
      Printf.printf "; From Clause:\n";
      select_stmt_from table_references t;
      
      (* Where Clause *)
      Printf.printf "\n; Where Clause:\n";
      select_stmt_where  opt_where t;
      
      (* Select Clause *)
      Printf.printf "\n; Select Clause:\n";
      select_stmt_select (List.rev select_expr_list) t;
      
      Printf.printf "\n";
      Printf.printf ";-----------------------------\n";
      Printf.printf "; Path Pattern and Get Model\n";
      Printf.printf ";-----------------------------\n";

      let path = select_stmt_path_pattern (List.rev select_expr_list) t in 
      let paths = (path_pattern path t) in
      (print_path_pattern paths t 1)
      
   | Int(x) -> if x > -1 then 
                  Printf.printf "(_ bv%d 32)" x
               else
                  Printf.printf "(bvneg (_ bv%d 32))" (x * -1)

   | Float(x) ->  Printf.printf "%f" x

   | String(x) -> Printf.printf "\"%s\"" (String.sub x 1 ((String.length x) - 2))

   | Name(x) ->   Printf.printf "%s" x

   | Column(x,y) -> Printf.printf "%s.%s" x y

   | Id(x) ->     Printf.printf "%s" x

(* | Bool(x) -> *)

   | Plus(x,y) ->  binary_ops x y "bvadd" t

   | Minus(x,y) -> binary_ops x y "bvsub" t

   | Div(x,y) ->   binary_ops x y "bvudiv" t

   | Multi(x,y) -> binary_ops x y "bvmul" t

(* | Mod(x,y) ->   *)

(* | Uminus(x) ->  *)

   | And(x,y) -> binary_ops x y "bvand" t

   | Or(x,y) ->  binary_ops x y "bvor" t

(* | Xor(x,y) -> *)

   | Equal(x,y) ->        binary_ops x y "=" t

   | GreaterEqual(x,y) -> binary_ops x y "bvuge" t

   | Greater(x,y) ->      binary_ops x y "bvugt" t

   | LessEqual(x,y) ->    binary_ops x y "bvsle" t

   | Less(x,y) ->         binary_ops x y "bvslt" t

   | NotEqual(x,y) ->     Printf.printf "(not ";
                          binary_ops x y "=" t;
                          Printf.printf ")"

   | Not(x) ->    Printf.printf "(not ";
                  compile x t;
                  Printf.printf ")"

   | IsNull(x) -> Printf.printf "(= ";
                  compile x t;
                  Printf.printf " %s)" "null"

(* | Between(x,y,z) -> *)

(* | In(x,y) -> *)

(* | Exists(x) -> *)

(* | Func(x,y) -> *)

   | Case(None,x,Some(y)) -> (* t option * t list * t option*)
      when_list (List.rev x) t;
      Printf.printf " ";
      compile y t;
      when_list_after x t

   | Case(None,x,None) -> (* t option * t list * t option*)
      when_list (List.rev x) t;
      compile (Int(0)) t;
      when_list_after x t
      
   | When(x,y) -> (* t * t*)
      compile x t;
      Printf.printf " ";
      compile y t;

(* | Like(x,y,z) -> *)

   | _ -> Printf.printf ""

and select_stmt_from table_references t = 
    match table_references with
    | [] -> Printf.printf ""
    | x :: xs -> match x.from_table.t_table  with
                 | None    -> Printf.printf "";
                 | Some(y) -> table_def y x.from_table.t_alias t;
                 match x.from_table.t_subquery with
                 | None    -> Printf.printf "";
                 | Some(y) -> compile y t;
                 match x.from_join with
                 | None    -> Printf.printf "";
                 | Some(y) -> select_stmt_from_join y t;
                 select_stmt_from xs t

and select_stmt_from_join join_clause t =
    match join_clause with
    | [] -> Printf.printf ""
    | x :: xs -> match x.join_cond with
                 | None  -> Printf.printf "";
                            select_stmt_from_join xs t
                 | Some(x) -> Printf.printf "(assert ";
                              compile x t;
                              Printf.printf ")\n";
                              select_stmt_from_join xs t

and select_stmt_where opt_where t = 
   match opt_where with
   | Some(Where(x)) -> Printf.printf "(assert ";
                       compile x t;
                       Printf.printf ")\n"
   | _ ->              Printf.printf ""

and select_stmt_select select_expr_list t = 
    match select_expr_list with
    | [] -> Printf.printf ""
    | x :: xs -> Printf.printf "(declare-const %s (_ BitVec 32))\n" x.s_alias;
                 Printf.printf "(assert (= %s " x.s_alias;
                 compile x.s_expr t;
                 Printf.printf "))\n";
                 select_stmt_select xs t

and select_stmt_path_pattern select_expr_list t = 
    match select_expr_list with
    | [] -> []
    | x :: xs -> 
       column_expr_list x.s_alias x.s_expr :: 
       select_stmt_path_pattern xs t

and column_expr_list a b = 
    let abc = ref [] in
       let rec pattern_compile a b = 
          match b with
          | Case(None,x,Some(y)) ->
             pattern_when_list a (List.rev x);
             pattern_compile a y
             
          | When(x,y) ->
             pattern_compile a y
             
          | _ -> abc := [(a,b)] :: !abc
          
       and pattern_when_list a x =
          match x with
          | [] -> []
          | z :: zs -> pattern_compile a z ::
                       pattern_when_list a zs in
       pattern_compile  a b ;
       !abc

(*------------------------------------------------*)

and path_pattern s t = 
   match s with
   | [] -> []
   | xs :: t1 ->
      match t1 with
      | [] -> xs
      | ys :: zs -> path_pattern ((cartesian xs ys) :: zs) t
      
and cartesian xs ys =
   match xs, ys with
   | _ , [] -> xs
   | [], _  -> xs
   | x :: xs', _ ->(List.map (fun y -> x@y) ys) @ (cartesian xs' ys) 


and print_path_pattern s t n =
   match s with
   | [] -> Printf.printf ""
   | x :: xs -> 
                Printf.printf "(echo \"Path:%d\")\n" n;
                Printf.printf "(push)\n";
                Printf.printf "(assert (and ";
                print_condition x t;
                Printf.printf "))\n";
                Printf.printf "(check-sat)\n";
                Printf.printf "(get-model)\n";
                Printf.printf "(pop)\n\n";
                print_path_pattern xs t (n + 1)

and print_condition s t =
   match s with
   | [] -> Printf.printf "";
   | x :: xs -> match x with
                | (a,b) -> 
                   Printf.printf "(= %s " a;
                   compile b t;
                   Printf.printf ")";
                   print_condition xs t
                | _ -> Printf.printf ""

and binary_ops expr1 expr2 op t = 
   Printf.printf "(%s " op; 
   compile expr1 t;
   Printf.printf " ";
   compile expr2 t;
   Printf.printf ")"

and when_list x t =
   match x with
   | [] -> Printf.printf ""
   | z :: zs -> Printf.printf "(ite ";
                compile z t;
                Printf.printf " ";
                when_list zs t

and when_list_after x t =
   match x with
   | [] -> Printf.printf ""
   | z :: zs -> Printf.printf ")";
                when_list_after zs t

(*ここを修正*)
and table_def x y t = 
   match t with
   | [] -> Printf.printf ""
   | CreateTableStmt(x,z) :: zs ->
      (* カラムの型定義 *)
      column_def_list x z t;
      (* カラムの制約定義 *)
      column_constraint_list x z t;
      table_def x y zs
   | _ :: zs -> Printf.printf "";
      table_def x y zs

and column_def_list tbl_name column_def t =
    match column_def with
    | [] ->  Printf.printf ""
    | x :: xs -> Printf.printf "(declare-const %s.%s (_ BitVec 32))\n" tbl_name x.c_column;
                 column_def_list tbl_name xs t

and column_datatype datatype t = 
   match datatype with
   | Char(_) -> "String"
   | Varchar(_) -> "String"
   | Number(_,None) -> "Int"
   | Number(_,Some(0)) -> "Int"
   | Number(_,_) -> "Real"

and column_constraint_list tbl_name column_def t =
    match column_def with
    | [] -> Printf.printf ""
    | x :: xs -> column_datasize tbl_name x t;
                 column_check x t;
                 column_constraint_list tbl_name xs t

and column_datasize tbl_name column_def t = 
   match column_def.c_datatype with
   | Number(Some(size),None) -> (column_datasize_int tbl_name column_def size)
   | Number(Some(size1),Some(size2))        -> Printf.printf ""
   | Char(Some(size)) | Varchar(Some(size)) -> Printf.printf ""
   | _                                      -> Printf.printf ""

and column_datasize_int tbl_name column_def size = 
   match column_def.c_notnull with
   | None ->          Printf.printf "(assert (or  (and (bvsge %s.%s (bvneg (_ bv%s 32))) (bvsle %s.%s (_ bv%s 32)) )      (= %s.%s null)))\n"  tbl_name column_def.c_column (String.make size '9') tbl_name column_def.c_column (String.make size '9') tbl_name column_def.c_column
   
   | Some(NotNull) -> Printf.printf "(assert (and (and (bvsge %s.%s (bvneg (_ bv%s 32))) (bvsle %s.%s (_ bv%s 32)) ) (not (= %s.%s null))))\n" tbl_name column_def.c_column (String.make size '9') tbl_name column_def.c_column (String.make size '9') tbl_name column_def.c_column
   | _ ->             Printf.printf ""

and column_check column_def t = 
   match column_def.c_check with
   | Some(Check(x)) -> Printf.printf "(assert ";
                       compile x t;
                       Printf.printf ")\n"
   | _ ->              Printf.printf ""


let is_table x =
   match x with
   | CreateTableStmt(_,_) -> true
   | _ -> false

let is_query x =
   match x with
   | SelectStmt(_,_,_,_,_,_,_) -> true
   | _ -> false

let rec do_compile q t =
  match q with
  | x :: xs -> compile x t;
               do_compile xs t
  | [] -> Printf.printf ""

(*
  Main 
*)
let main =
  let s =
    Sqlparser.stmt_list Sqllexer.token
      (Lexing.from_channel stdin) in
  let q = List.filter is_query s in
  let t = List.filter is_table s in

  Printf.printf ";-----------------------------\n";
  Printf.printf "; Common Definition\n";
  Printf.printf ";-----------------------------\n";
  Printf.printf "; basic setting\n";
  Printf.printf "(set-option :produce-proofs true)\n";
  Printf.printf "(set-option :pp.bv-literals false)\n";
  Printf.printf "(set-logic QF_BV)\n";
  Printf.printf "\n";
  Printf.printf "; Dummy constant for null \n";
  Printf.printf "(define-const null (_ BitVec 32) (bvneg (_ bv99 32)))\n";

  do_compile q t;

