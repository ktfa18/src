%{  
open Sqlsyntax
%}

//
//Names and Literal Values
//
%token <string> NAME
%token <string> STRING
%token <int> INTNUM
%token <bool> BOOL
%token <float> APPROXNUM

//
//operators and precedence levels
//
%token ADD
%token ALL
%token ALTER
%token ANALYZE
%token AND
%token ANY
%token AS
%token ASC
%token AUTO_INCREMENT
%token BEFORE
%token BETWEEN
%token BIGINT
%token BINARY
%token BIT
%token BLOB
%token BOTH
%token BY
%token CALL
%token CASCADE
%token CASE
%token CHANGE
%token CHAR
%token CHECK
%token COLLATE
%token COLUMN
%token COMMENT
%token CONDITION
%token CONSTRAINT
%token CONTINUE
%token CONVERT
%token CREATE
%token CROSS
%token CURRENT_DATE
%token CURRENT_TIME
%token CURRENT_TIMESTAMP
%token CURRENT_USER
%token CURSOR
%token DATABASE
%token DATABASES
%token DATE
%token DATETIME
%token DAY_HOUR
%token DAY_MICROSECOND
%token DAY_MINUTE
%token DAY_SECOND
%token DECIMAL
%token DECLARE
%token DEFAULT
%token DELAYED
%token DELETE
%token DESC
%token DESCRIBE
%token DETERMINISTIC
%token DISTINCT
%token DISTINCTROW
%token DIV
%token DOUBLE
%token DROP
%token DUAL
%token EACH
%token ELSE
%token ELSEIF
%token ENCLOSED
%token END
%token ENUM
%token ESCAPE
%token ESCAPED
%token EXISTS
%token EXIT
%token EXPLAIN
%token FETCH
%token FLOAT
%token FOR
%token FORCE
%token FOREIGN
%token FROM
%token FULLTEXT
%token GRANT
%token GROUP
%token HAVING
%token HIGH_PRIORITY
%token HOUR_MICROSECOND
%token HOUR_MINUTE
%token HOUR_SECOND
%token IF
%token IGNORE
%token IN
%token INDEX
%token INFILE
%token INNER
%token INOUT
%token INSENSITIVE
%token INSERT
%token INTEGER
%token INTERVAL
%token INTO
%token IS
%token ITERATE
%token JOIN
%token KEY
%token KEYS
%token KILL
%token LEADING
%token LEAVE
%token LEFT
%token LIKE
%token LIMIT
%token LINES
%token LOAD
%token LOCALTIME
%token LOCALTIMESTAMP
%token LOCK
%token LONG
%token LONGBLOB
%token LONGTEXT
%token LOOP
%token LOW_PRIORITY
%token MATCH
%token MEDIUMBLOB
%token MEDIUMINT
%token MEDIUMTEXT
%token MINUTE_MICROSECOND
%token MINUTE_SECOND
%token MOD
%token MODIFIES
%token NATURAL
%token NOT
%token NO_WRITE_TO_BINLOG
%token NULLX
%token NUMBER
%token ON
%token ONDUPLICATE
%token OPTIMIZE
%token OPTION
%token OPTIONALLY
%token OR
%token ORDER
%token OUT
%token OUTER
%token OUTFILE
%token PRECISION
%token PRIMARY
%token PROCEDURE
%token PURGE
%token QUICK
%token READ
%token READS
%token REAL
%token REFERENCES
%token REGEXP
%token RELEASE
%token RENAME
%token REPEAT
%token REPLACE
%token REQUIRE
%token RESTRICT
%token RETURN
%token REVOKE
%token RIGHT
%token ROLLUP
%token SCHEMA
%token SCHEMAS
%token SECOND_MICROSECOND
%token SELECT
%token SENSITIVE
%token SEPARATOR
%token SET
%token SHOW
%token SMALLINT
%token SOME
%token SONAME
%token SPATIAL
%token SPECIFIC
%token SQL
%token SQLEXCEPTION
%token SQLSTATE
%token SQLWARNING
%token SQL_BIG_RESULT
%token SQL_CALC_FOUND_ROWS
%token SQL_SMALL_RESULT
%token SSL
%token STARTING
%token STRAIGHT_JOIN
%token TABLE
%token TEMPORARY
%token TEXT
%token TERMINATED
%token THEN
%token TIME
%token TIMESTAMP
%token TINYBLOB
%token TINYINT
%token TINYTEXT
%token TO
%token TRAILING
%token TRIGGER
%token UNDO
%token UNION
%token UNIQUE
%token UNLOCK
%token UNSIGNED
%token UPDATE
%token USAGE
%token USE
%token USING
%token UTC_DATE
%token UTC_TIME
%token UTC_TIMESTAMP
%token VALUES
%token VARBINARY
%token VARCHAR
%token VARYING
%token WHEN
%token WHERE
%token WHILE
%token WITH
%token WRITE
%token XOR
%token YEAR
%token YEAR_MONTH
%token ZEROFILL

//
// Operators(tanaka)
//
%token PLUS
%token MINUS
%token SLASH
%token ASTERISK
%token EQUAL
%token GREATEREQUAL
%token GREATER
%token LESSEQUAL
%token LESS
%token NOTEQUAL
%token AMPERSAND
%token TILDE
%token VERTICALBAR
%token HAT
%token PERCENT
%token LPAREN
%token RPAREN
%token COMMA
%token DOT
%token COLON
%token SEMICOLON
%token EXCLAMATION
%token APOSTROPHE
%token EOF

%right ASSIGN
%left OR
%left XOR
%left AND
%token IN IS LIKE REGEXP
%nonassoc IN IS LIKE REGEXP
%left NOT EXCLAMATION
//%left BETWEEN
%token <string> COMPARISON //= <> < > <= >= <=>
%left <string> COMPARISON //= <> < > <= >= <=>
%left EQUAL GREATEREQUAL GREATER LESSEQUAL LESS NOTEQUAL
%left VERTICALBAR
%left AMPERSAND
%left <int> SHIFT //<< >>
%left PLUS MINUS
%left ASTERISK SLASH PERCENT MOD
%left HAT
%nonassoc UMINUS

//------------------------------------------------------------
// Start
//------------------------------------------------------------
%start stmt_list
%type <Sqlsyntax.t list> stmt_list 
%%

stmt_list:
| stmt           { [$1] }
| stmt_list stmt { $2 :: $1 }

//------------------------------------------------------------
// Create Table
//------------------------------------------------------------
stmt:
| create_table_stmt { $1 }

create_table_stmt:
| CREATE TABLE
     table_name_def
  LPAREN
     column_def_list
  RPAREN
  SEMICOLON
  {
       CreateTableStmt(
             $3,
             List.rev $5
       )
  }

table_name_def:
| NAME { $1 }

column_def_list:
| column_def { [$1] }
| column_def_list COMMA column_def { $3 :: $1 }

column_def:
| NAME datatype_def opt_notnull opt_check { { c_column = $1; c_datatype = $2; c_notnull = $3; c_check = $4} }

datatype_def:
| CHAR { Char(None) }
| CHAR LPAREN INTNUM RPAREN { Char(Some($3)) }
| VARCHAR { Varchar(None) }
| VARCHAR LPAREN INTNUM RPAREN { Varchar(Some($3)) }
| NUMBER { Number(None, None) }
| NUMBER LPAREN INTNUM RPAREN { Number(Some($3), None) }
| NUMBER LPAREN INTNUM COMMA INTNUM RPAREN { Number(Some($3), Some($5)) }

opt_notnull:
| { None }
| NULLX { None }
| NOT NULLX { Some(NotNull) }

opt_check:
| { None }
| CHECK LPAREN expr RPAREN { Some(Check($3)) }

//------------------------------------------------------------
// Statements: Select Statement
//------------------------------------------------------------
stmt:
| select_stmt { $1 }

select_stmt:
| SELECT
     select_opts 
     select_expr_list
  FROM
     table_references
  opt_where
  opt_groupby
  opt_having
  opt_orderby
  SEMICOLON
  {   
       SelectStmt(
             $2,
             $3,
             $5,
             $6,
             $7,
             $8,
             $9
        )
  }


//------------------------------------------------------------
// Select
//------------------------------------------------------------
select_opts:
| { All }
| ALL      { All }
| DISTINCT { Distinct }

select_expr_list:
| select_expr { [$1] }
| select_expr_list COMMA select_expr { $3 :: $1 }
//| ASTERISK { [Asterisk] }

select_expr:
| expr opt_as_alias { { s_expr = $1 ; s_alias = $2 } }


//------------------------------------------------------------
// From
//------------------------------------------------------------
table_references:
| LPAREN table_references RPAREN { $2 }

table_references:
| table_reference { [$1] }
| table_references COMMA table_reference { $3 :: $1 }

table_reference:
| LPAREN table_reference RPAREN { $2 }

table_reference:
| table_factor { { from_table = $1 ; from_join = None} }
| table_factor join_tables { { from_table = $1 ; from_join = Some($2)} }

table_factor:
| NAME opt_as_alias          { { t_schema = "";
                                 t_table  = Some($1);
                                 t_subquery = None;
                                 t_alias  = $2 } }
| NAME DOT NAME opt_as_alias { { t_schema = $1;
                                 t_table  = Some($3);
                                 t_subquery = None;
                                 t_alias  = $4 } }
| table_subquery opt_as_alias{ { t_schema = "";
                                 t_table  = None;
                                 t_subquery = Some($1);
                                 t_alias  = $2 } }

table_subquery:
| LPAREN select_stmt RPAREN { $2 }

opt_as_alias:
| { "" }
| NAME    { $1 }
| AS NAME { $2 }


join_tables:
| join_table_ { [$1] }
| join_tables join_table_ { $2 :: $1 }

join_table_:
| opt_inner_cross JOIN table_factor opt_join_condition opt_join_using
              {{ join_type  = $1;
                 join_table = $3;
                 join_cond  = $4;
                 join_using = $5 }}

| left_or_right JOIN table_factor opt_join_condition opt_join_using
              {{ join_type  = $1;
                 join_table = $3;
                 join_cond  = $4;
                 join_using = $5  }}

opt_inner_cross:
| { Inner }
| INNER { Inner }
| CROSS { Cross }

left_or_right:
| LEFT        { Left }
| LEFT OUTER  { Left }
| RIGHT       { Right }
| RIGHT OUTER { Right }

opt_join_condition:
| { None }
| join_condition { Some($1) }

join_condition:
| ON expr { $2 }

opt_join_using:
| { None }
| USING LPAREN column_list RPAREN { Some($3) }

column_list:
| column { [$1] }
| column_list COMMA column { $3 :: $1 }

column:
| NAME { Column("",$1) }


//------------------------------------------------------------
// Where
//------------------------------------------------------------
opt_where:
| { None }
| WHERE expr { Some(Where($2)) }

//------------------------------------------------------------
// Group By
//------------------------------------------------------------
opt_groupby:
| { None }
| GROUP BY groupby_list { Some($3) }

groupby_list:
| groupby_column { [$1] }
| groupby_column COMMA groupby_list { $1 :: $3 }

groupby_column:
|	NAME         { {group_column_ref_table = "" ; group_column_name = $1 } }
|	NAME DOT NAME  { {group_column_ref_table = $1 ; group_column_name = $3 } }


//------------------------------------------------------------
// Having
//------------------------------------------------------------
opt_having:
| { None } 
| HAVING expr { Some($2) }

//------------------------------------------------------------
// Order By
//------------------------------------------------------------
opt_orderby:
| { None }
| ORDER BY orderby_list { Some($3) }

orderby_list:
| ordering_column { [ $1 ] }
| ordering_column COMMA orderby_list { $1 :: $3 }

ordering_column:
| NAME opt_asc_desc { {order_column_ref_table = "" ; order_column_name = $1 ; order_column_dir = $2 } }
| NAME DOT NAME opt_asc_desc { {order_column_ref_table = $1 ; order_column_name = $3 ; order_column_dir = $4 } }

opt_asc_desc:
| { Asc }
| ASC  { Asc }
| DESC { Desc }


//------------------------------------------------------------
// expressions
//------------------------------------------------------------
expr: 
| NAME            { Name($1) }
| NAME DOT NAME   { Column($1, $3) }
| STRING        { String($1) }
| INTNUM        { Int($1) }
| APPROXNUM     { Float($1) }
| BOOL          { Bool($1) }
| NULLX         { Null("null") }

expr:
| LPAREN expr RPAREN { $2 }

expr:
| LPAREN select_stmt RPAREN { $2 }

expr: 
| expr BETWEEN expr AND expr %prec AND { Between($1, $3, $5) }

expr: 
| expr PLUS expr      { Plus($1, $3) }
| expr MINUS expr     { Minus($1, $3) }
| expr ASTERISK expr  { Multi($1, $3) }
| expr SLASH expr     { Div($1, $3) }
//| expr MOD expr       { Mod($1, $3) }
| MINUS expr %prec UMINUS { Uminus($2) }
| expr AND expr       { And($1, $3) }
| expr OR expr        { Or($1, $3) }
//| expr XOR expr       { Xor($1, $3) }
| expr EQUAL expr        { Equal($1, $3) }
| expr GREATEREQUAL expr { GreaterEqual($1, $3) }
| expr GREATER expr      { Greater($1, $3) }
| expr LESSEQUAL expr    { LessEqual($1, $3) }
| expr LESS expr         { Less($1, $3) }
| expr NOTEQUAL expr     { NotEqual($1, $3) }
//| expr COMPARISON expr                           { Compar($2, $1, $3) }
//| expr COMPARISON LPAREN select_stmt RPAREN      { Compar($2, $1, $4) }
//| expr COMPARISON ANY LPAREN select_stmt RPAREN  { Compar($2, $1, $5) }
//| expr COMPARISON SOME LPAREN select_stmt RPAREN { Compar($2, $1, $5) }
//| expr COMPARISON ALL LPAREN select_stmt RPAREN  { Compar($2, $1, $5) }
//| expr VERTICALBAR expr { Concat($1, $3) }
| NOT expr            { Not($2) }

expr:
| expr IS NULLX { IsNull($1) }
| expr IS NOT NULLX { Not(IsNull($1)) }

//expr:
//| expr IN LPAREN opt_val_list RPAREN        { In($1, $4) }
//| expr NOT IN LPAREN opt_val_list RPAREN    { Not(In($1, $5)) }
//| expr IN LPAREN select_stmt RPAREN     { In($1, $4) }
//| expr NOT IN LPAREN select_stmt RPAREN { Not(In($1, $5)) }
//| EXISTS LPAREN select_stmt RPAREN      { Exists($3) }
//
//expr:
//| NAME LPAREN opt_val_list RPAREN { Func($1, $3) }
//
//opt_val_list:
//| val_list { $1 }
//
//val_list:
//| expr { [$1] }
//| val_list COMMA expr { $3 :: $1 }

expr:
//| CASE expr case_list END           { Case(Some($2), $3, None) }
//| CASE expr case_list ELSE expr END { Case(Some($2), $3, Some($5)) }
| CASE      case_list END           { Case(None,     $2, None) }
| CASE      case_list ELSE expr END { Case(None,     $2, Some($4)) }

case_list:
| case { [$1] }
| case_list case  { $2 :: $1 }

case:
| WHEN expr THEN expr { When($2, $4) }

//expr:
//| expr LIKE expr opt_escape         { Like($1, $3, $4) }
//| expr NOT LIKE expr opt_escape { Not(Like($1, $4, $5)) }
//
//opt_escape:
//|             { None }
//| ESCAPE atom { Some($2) }
//
//atom:
//|	bind_variable { $1 }
//|	literal { $1 }
//
//literal:
//	|	STRING { String($1) }
//	|	INTNUM { Int($1) }
//	|	APPROXNUM { Float($1) }
//
//bind_variable:
//	|	COLON NAME { BindVar($2) }
//
//
//range_variable:
//| { None }
//| NAME { Some($1) }


%%
