{
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
}

let char        = ['a'-'z' 'A'-'Z']   
let digit       = ['0'-'9']   
let int         = '-'?digit+   
let float       = '-'?digit+ '.' digit+
let str         = '''[^''']*'''
let identifier  = char(char|digit|['_'])*
let whitespace  = [' ' '\t' '\n' '\r']   

rule token = parse
|['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_'] * as id
               { let id_uppercase = String.uppercase_ascii(id) in
                 try
                   Hashtbl.find keyword_table id_uppercase
                 with Not_found ->
                   NAME id }
|'+'	{ PLUS }
|'-'	{ MINUS }
|'/'	{ SLASH }
|'*'	{ ASTERISK }
|'='	{ EQUAL }
|">="	{ GREATEREQUAL }
|'>'	{ GREATER }
|"<="	{ LESSEQUAL }
|'<'	{ LESS }
|"!="	{ NOTEQUAL }
|"<>"	{ NOTEQUAL }
|'&'	{ AMPERSAND }
|'~'	{ TILDE }
|'|'	{ VERTICALBAR }
|'^'	{ HAT }
|'%'	{ PERCENT }
|'('	{ LPAREN }
|')'	{ RPAREN }
|','	{ COMMA }
|'.'	{ DOT }
|':'	{ COLON }
|';'	{ SEMICOLON }
|'!'	{ EXCLAMATION }
|'\''	{ APOSTROPHE } 
|eof	{ EOF }
|"&&"	{ AND }
|"||"	{ OR }
|int { INTNUM(int_of_string (Lexing.lexeme lexbuf)) }
|float { APPROXNUM(float_of_string (Lexing.lexeme lexbuf)) }
|str { STRING(Lexing.lexeme lexbuf) }
|whitespace+ { token lexbuf }
| _
    { failwith
        (Printf.sprintf
           "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }

