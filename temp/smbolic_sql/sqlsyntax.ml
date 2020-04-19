type t =
| Int of int
| Float of float
| String of string

| Name of string
| Column of string * string
| Id of string
| Bool of bool
| Null of string

| Plus of t * t
| Minus of t * t
| Div of t * t
| Multi of t * t
| Mod of t * t
| Uminus of t
| And of t * t
| Or of t * t
| Xor of t * t
| Equal of t * t 
| GreaterEqual of t * t  
| Greater of t * t  
| LessEqual of t * t 
| Less of t * t  
| NotEqual of t * t 
| Not of t
| IsNull of t
| Between of t * t * t
| In of t * t list
| Exists of t
| Func of string * t list
| Case of t option * t list * t option
| When of t * t
| Like of t * t * t option

| Concat of t * t
| Cond of (t * t * t)
| Compar of string * t * t
| Is of t

| Table of table_def
| Join of join_def
| Where of where_def
| GroupBy of group_by_def
| Having of having_def
| OrderBy of order_by_def

| CreateTableStmt of string *
                     columns_def list
| SelectStmt of select_opt * 
                select_def list * 
                from_def list *
                where_def option * 
                group_by_def list option * 
                having_def option * 
                order_by_def list option

and select_opt = All | Distinct

and select_def = {
      s_expr : t;
      s_alias : string}

and from_def = {
      from_table: table_def;
      from_join : join_def list option
      }

and table_def = {
      t_schema: string;
      t_table : string option;
      t_subquery : t option;
      t_alias : string
      }

and join_type = Inner | Cross | Left | Right

and join_def = {
      join_type : join_type;
      join_table : table_def;
      join_cond : t option;
      join_using : t list option}

and where_def = t

and group_by_def = {
      group_column_ref_table : string;
      group_column_name : string}

and having_def = t

and order_by_def = {
      order_column_ref_table : string;
      order_column_name : string;
      order_column_dir : dir}

and dir = Asc | Desc

and datatype = Char of int option | Varchar of int option | Number of int option * int option
and constraint_opt = Check of t | NotNull
and columns_def = {
      c_column : string;
      c_datatype : datatype;
      c_notnull : constraint_opt option;
      c_check : constraint_opt option
     }
