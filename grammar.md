program        → declaration* EOF ;

declaration    → funDecl
               | varDecl
               | statement ;

statement      → exprStmt
               | forStmt
               | ifStmt
               | printStmt
               | whileStmt
               | block ;

block          → "{" declaration* "}" ;

varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;

ifStmt         → "if" "(" expression ")" statement
               ( "else" statement )? ;

exprStmt       → expression ";" ;
printStmt      → "print" expression ";" ;

whileStmt      → "while" "(" expression ")" statement ;

expression     → assignment ;
assignment     → IDENTIFIER "=" assignment
               | logic_or ;
logic_or       → logic_and ( "or" logic_and )* ;
logic_and      → equality ( "and" equality )* ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary | call ;
call           → primary ( "(" arguments? ")" )* ;
primary        → "true" | "false" | "nil"
               | NUMBER | STRING
               | "(" expression ")"
               | IDENTIFIER ;

arguments      → expression ( "," expression )* ;

funDecl        → "fun" function ;
function       → IDENTIFIER "(" parameters? ")" block ;

parameters     → IDENTIFIER ( "," IDENTIFIER )* ;
