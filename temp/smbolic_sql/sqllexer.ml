# 1 "sqllexer.mll"
 
open Sqlparser
let keyword_table = Hashtbl.create 217

(* keywords *)
let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
              [ 
                "ADD", ADD;
                "ALL", ALL;
                "ALTER", ALTER;
                "ANALYZE", ANALYZE;
                "AND", AND;
                "ANY", ANY;
                "AS", AS;
                "ASC", ASC;
                "AUTO_INCREMENT", AUTO_INCREMENT;
                "BEFORE", BEFORE;
                "BETWEEN", BETWEEN;
                "INT8|BIGINT", BIGINT;
                "BINARY", BINARY;
                "BIT", BIT;
                "BLOB", BLOB;
                "BOTH", BOTH;
                "BY", BY;
                "CALL", CALL;
                "CASCADE", CASCADE;
                "CASE", CASE;
                "CHANGE", CHANGE;
                "CHAR", CHAR;
                "CHECK", CHECK;
                "COLLATE", COLLATE;
                "COLUMN", COLUMN;
                "COMMENT", COMMENT;
                "CONDITION", CONDITION;
                "CONSTRAINT", CONSTRAINT;
                "CONTINUE", CONTINUE;
                "CONVERT", CONVERT;
                "CREATE", CREATE;
                "CROSS", CROSS;
                "CURRENT_DATE", CURRENT_DATE;
                "CURRENT_TIME", CURRENT_TIME;
                "CURRENT_TIMESTAMP", CURRENT_TIMESTAMP;
                "CURRENT_USER", CURRENT_USER;
                "CURSOR", CURSOR;
                "DATABASE", DATABASE;
                "DATABASES", DATABASES;
                "DATE", DATE;
                "DATETIME", DATETIME;
                "DAY_HOUR", DAY_HOUR;
                "DAY_MICROSECOND", DAY_MICROSECOND;
                "DAY_MINUTE", DAY_MINUTE;
                "DAY_SECOND", DAY_SECOND;
                "NUMERIC|DEC|DECIMAL", DECIMAL;
                "DECLARE", DECLARE;
                "DEFAULT", DEFAULT;
                "DELAYED", DELAYED;
                "DELETE", DELETE;
                "DESC", DESC;
                "DESCRIBE", DESCRIBE;
                "DETERMINISTIC", DETERMINISTIC;
                "DISTINCT", DISTINCT;
                "DISTINCTROW", DISTINCTROW;
                "DIV", DIV;
                "FLOAT8|DOUBLE", DOUBLE;
                "DROP", DROP;
                "DUAL", DUAL;
                "EACH", EACH;
                "ELSE", ELSE;
                "ELSEIF", ELSEIF;
                "ENCLOSED", ENCLOSED;
                "END", END;
                "ENUM", ENUM;
                "ESCAPE", ESCAPE;
                "ESCAPED", ESCAPED;
                "EXISTS", EXISTS;
                "EXIT", EXIT;
                "EXPLAIN", EXPLAIN;
                "FETCH", FETCH;
                "FLOAT4", FLOAT;
                "FOR", FOR;
                "FORCE", FORCE;
                "FOREIGN", FOREIGN;
                "FROM", FROM;
                "FULLTEXT", FULLTEXT;
                "GRANT", GRANT;
                "GROUP", GROUP;
                "HAVING", HAVING;
                "HIGH_PRIORITY", HIGH_PRIORITY;
                "HOUR_MICROSECOND", HOUR_MICROSECOND;
                "HOUR_MINUTE", HOUR_MINUTE;
                "HOUR_SECOND", HOUR_SECOND;
                "IF", IF;
                "IGNORE", IGNORE;
                "IN", IN;
                "INDEX", INDEX;
                "INFILE", INFILE;
                "INNER", INNER;
                "INOUT", INOUT;
                "INSENSITIVE", INSENSITIVE;
                "INSERT", INSERT;
                "INT4?|INTEGER", INTEGER;
                "INTERVAL", INTERVAL;
                "INTO", INTO;
                "IS", IS;
                "ITERATE", ITERATE;
                "JOIN", JOIN;
                "INDEX|KEY", KEY;
                "KEYS", KEYS;
                "KILL", KILL;
                "LEADING", LEADING;
                "LEAVE", LEAVE;
                "LEFT", LEFT;
                "LIKE", LIKE;
                "LIMIT", LIMIT;
                "LINES", LINES;
                "LOAD", LOAD;
                "LOCALTIME", LOCALTIME;
                "LOCALTIMESTAMP", LOCALTIMESTAMP;
                "LOCK", LOCK;
                "LONG", LONG;
                "LONGBLOB", LONGBLOB;
                "LONGTEXT", LONGTEXT;
                "LOOP", LOOP;
                "LOW_PRIORITY", LOW_PRIORITY;
                "MATCH", MATCH;
                "MEDIUMBLOB", MEDIUMBLOB;
                "MIDDLEINT|MEDIUMINT", MEDIUMINT;
                "MEDIUMTEXT", MEDIUMTEXT;
                "MINUTE_MICROSECOND", MINUTE_MICROSECOND;
                "MINUTE_SECOND", MINUTE_SECOND;
                "MOD", MOD;
                "MODIFIES", MODIFIES;
                "NATURAL", NATURAL;
                "NOT", NOT;
                "NO_WRITE_TO_BINLOG", NO_WRITE_TO_BINLOG;
                "NULL", NULLX;
                "NUMBER", NUMBER;
                "ON", ON;
                "ONDUPLICATE", ONDUPLICATE;
                "OPTIMIZE", OPTIMIZE;
                "OPTION", OPTION;
                "OPTIONALLY", OPTIONALLY;
                "OR", OR;
                "ORDER", ORDER;
                "OUT", OUT;
                "OUTER", OUTER;
                "OUTFILE", OUTFILE;
                "PRECISION", PRECISION;
                "PRIMARY", PRIMARY;
                "PROCEDURE", PROCEDURE;
                "PURGE", PURGE;
                "QUICK", QUICK;
                "READ", READ;
                "READS", READS;
                "REAL", REAL;
                "REFERENCES", REFERENCES;
                "REGEXP|RLIKE", REGEXP;
                "RELEASE", RELEASE;
                "RENAME", RENAME;
                "REPEAT", REPEAT;
                "REPLACE", REPLACE;
                "REQUIRE", REQUIRE;
                "RESTRICT", RESTRICT;
                "RETURN", RETURN;
                "REVOKE", REVOKE;
                "RIGHT", RIGHT;
                "ROLLUP", ROLLUP;
                "SCHEMA", SCHEMA;
                "SCHEMAS", SCHEMAS;
                "SECOND_MICROSECOND", SECOND_MICROSECOND;
                "SELECT", SELECT;
                "SENSITIVE", SENSITIVE;
                "SEPARATOR", SEPARATOR;
                "SET", SET;
                "SHOW", SHOW;
                "INT2|SMALLINT", SMALLINT;
                "SOME", SOME;
                "SONAME", SONAME;
                "SPATIAL", SPATIAL;
                "SPECIFIC", SPECIFIC;
                "SQL", SQL;
                "SQLEXCEPTION", SQLEXCEPTION;
                "SQLSTATE", SQLSTATE;
                "SQLWARNING", SQLWARNING;
                "SQL_BIG_RESULT", SQL_BIG_RESULT;
                "SQL_CALC_FOUND_ROWS", SQL_CALC_FOUND_ROWS;
                "SQL_SMALL_RESULT", SQL_SMALL_RESULT;
                "SSL", SSL;
                "STARTING", STARTING;
                "STRAIGHT_JOIN", STRAIGHT_JOIN;
                "TABLE", TABLE;
                "TEMPORARY", TEMPORARY;
                "TEXT", TEXT;
                "TERMINATED", TERMINATED;
                "THEN", THEN;
                "TIME", TIME;
                "TIMESTAMP", TIMESTAMP;
                "TINYBLOB", TINYBLOB;
                "INT1|TINYINT", TINYINT;
                "TINYTEXT", TINYTEXT;
                "TO", TO;
                "TRAILING", TRAILING;
                "TRIGGER", TRIGGER;
                "UNDO", UNDO;
                "UNION", UNION;
                "UNIQUE", UNIQUE;
                "UNLOCK", UNLOCK;
                "UNSIGNED", UNSIGNED;
                "UPDATE", UPDATE;
                "USAGE", USAGE;
                "USE", USE;
                "USING", USING;
                "UTC_DATE", UTC_DATE;
                "UTC_TIME", UTC_TIME;
                "UTC_TIMESTAMP", UTC_TIMESTAMP;
                "VALUES?", VALUES;
                "VARBINARY", VARBINARY;
                "VARCHAR", VARCHAR;
                "VARYING", VARYING;
                "WHEN", WHEN;
                "WHERE", WHERE;
                "WHILE", WHILE;
                "WITH", WITH;
                "WRITE", WRITE;
                "XOR", XOR;
                "YEAR", YEAR;
                "YEAR_MONTH", YEAR_MONTH;
                "ZEROFILL", ZEROFILL
 ]

# 234 "sqllexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\223\255\002\000\079\000\230\255\024\000\233\255\234\255\
    \235\255\236\255\237\255\238\255\239\255\240\255\013\000\242\255\
    \026\000\030\000\031\000\034\000\250\255\251\255\252\255\090\000\
    \254\255\100\000\249\255\244\255\247\255\245\255\229\255\228\255\
    \225\255\057\000\175\000\185\000";
  Lexing.lex_backtrk = 
   "\255\255\255\255\031\000\028\000\255\255\024\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\014\000\255\255\
    \012\000\023\000\009\000\007\000\255\255\255\255\255\255\002\000\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\029\000";
  Lexing.lex_default = 
   "\001\000\000\000\255\255\255\255\000\000\033\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    \255\255\255\255\255\255\255\255\000\000\000\000\000\000\255\255\
    \000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\033\000\255\255\255\255";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\002\000\002\000\002\000\002\000\002\000\000\000\002\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\017\000\002\000\000\000\000\000\012\000\016\000\005\000\
    \011\000\010\000\021\000\024\000\009\000\023\000\008\000\022\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\007\000\006\000\018\000\020\000\019\000\032\000\
    \030\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\029\000\028\000\027\000\013\000\026\000\
    \032\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\000\000\014\000\034\000\015\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\031\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\000\000\
    \000\000\000\000\000\000\025\000\000\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\002\000\002\000\000\000\255\255\002\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\002\000\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
    \016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\017\000\018\000\018\000\000\000\019\000\
    \033\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\014\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\255\255\
    \255\255\255\255\255\255\025\000\255\255\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \005\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\033\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token lexbuf =
    __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 242 "sqllexer.mll"
                                                      id
# 390 "sqllexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 243 "sqllexer.mll"
               ( let id_uppercase = String.uppercase_ascii(id) in
                 try
                   Hashtbl.find keyword_table id_uppercase
                 with Not_found ->
                   NAME id )
# 398 "sqllexer.ml"

  | 1 ->
# 248 "sqllexer.mll"
     ( PLUS )
# 403 "sqllexer.ml"

  | 2 ->
# 249 "sqllexer.mll"
     ( MINUS )
# 408 "sqllexer.ml"

  | 3 ->
# 250 "sqllexer.mll"
     ( SLASH )
# 413 "sqllexer.ml"

  | 4 ->
# 251 "sqllexer.mll"
     ( ASTERISK )
# 418 "sqllexer.ml"

  | 5 ->
# 252 "sqllexer.mll"
     ( EQUAL )
# 423 "sqllexer.ml"

  | 6 ->
# 253 "sqllexer.mll"
      ( GREATEREQUAL )
# 428 "sqllexer.ml"

  | 7 ->
# 254 "sqllexer.mll"
     ( GREATER )
# 433 "sqllexer.ml"

  | 8 ->
# 255 "sqllexer.mll"
      ( LESSEQUAL )
# 438 "sqllexer.ml"

  | 9 ->
# 256 "sqllexer.mll"
     ( LESS )
# 443 "sqllexer.ml"

  | 10 ->
# 257 "sqllexer.mll"
      ( NOTEQUAL )
# 448 "sqllexer.ml"

  | 11 ->
# 258 "sqllexer.mll"
      ( NOTEQUAL )
# 453 "sqllexer.ml"

  | 12 ->
# 259 "sqllexer.mll"
     ( AMPERSAND )
# 458 "sqllexer.ml"

  | 13 ->
# 260 "sqllexer.mll"
     ( TILDE )
# 463 "sqllexer.ml"

  | 14 ->
# 261 "sqllexer.mll"
     ( VERTICALBAR )
# 468 "sqllexer.ml"

  | 15 ->
# 262 "sqllexer.mll"
     ( HAT )
# 473 "sqllexer.ml"

  | 16 ->
# 263 "sqllexer.mll"
     ( PERCENT )
# 478 "sqllexer.ml"

  | 17 ->
# 264 "sqllexer.mll"
     ( LPAREN )
# 483 "sqllexer.ml"

  | 18 ->
# 265 "sqllexer.mll"
     ( RPAREN )
# 488 "sqllexer.ml"

  | 19 ->
# 266 "sqllexer.mll"
     ( COMMA )
# 493 "sqllexer.ml"

  | 20 ->
# 267 "sqllexer.mll"
     ( DOT )
# 498 "sqllexer.ml"

  | 21 ->
# 268 "sqllexer.mll"
     ( COLON )
# 503 "sqllexer.ml"

  | 22 ->
# 269 "sqllexer.mll"
     ( SEMICOLON )
# 508 "sqllexer.ml"

  | 23 ->
# 270 "sqllexer.mll"
     ( EXCLAMATION )
# 513 "sqllexer.ml"

  | 24 ->
# 271 "sqllexer.mll"
      ( APOSTROPHE )
# 518 "sqllexer.ml"

  | 25 ->
# 272 "sqllexer.mll"
     ( EOF )
# 523 "sqllexer.ml"

  | 26 ->
# 273 "sqllexer.mll"
      ( AND )
# 528 "sqllexer.ml"

  | 27 ->
# 274 "sqllexer.mll"
      ( OR )
# 533 "sqllexer.ml"

  | 28 ->
# 275 "sqllexer.mll"
     ( INTNUM(int_of_string (Lexing.lexeme lexbuf)) )
# 538 "sqllexer.ml"

  | 29 ->
# 276 "sqllexer.mll"
       ( APPROXNUM(float_of_string (Lexing.lexeme lexbuf)) )
# 543 "sqllexer.ml"

  | 30 ->
# 277 "sqllexer.mll"
     ( STRING(Lexing.lexeme lexbuf) )
# 548 "sqllexer.ml"

  | 31 ->
# 278 "sqllexer.mll"
             ( token lexbuf )
# 553 "sqllexer.ml"

  | 32 ->
# 280 "sqllexer.mll"
    ( failwith
        (Printf.sprintf
           "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) )
# 563 "sqllexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;

