%{
  open Ast
%}

/* Token Declarations */
%token <int> NUMBER
%token <string> ID
%token INT VOID IF ELSE WHILE BREAK CONTINUE RETURN
%token LPAREN RPAREN LBRACE RBRACE SEMI COMMA
%token ASSIGN EQ NEQ LT LE GT GE AND OR
%token NOT PLUS MINUS TIMES DIV MOD
%token EOF

/* Operator Precedence and Associativity */
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT LE GT GE
%left PLUS MINUS
%left TIMES DIV MOD
%right NOT

%start <Ast.comp_unit> comp_unit

%%

/* Grammar Rules */
comp_unit:
  | f=list(func_def); EOF { CUnit f }

func_def:
  | rt=func_type; n=ID; LPAREN; p=separated_list(COMMA, param); RPAREN; b=block {
      { return_type = rt; name = n; params = p; body = b }
    }

func_type:
  | INT  { TInt }
  | VOID { TVoid }

param:
  | INT; i=ID { PInt i }

block:
  | LBRACE; stmts=list(stmt); RBRACE { SBlock stmts }

stmt:
  | SEMI                                     { SBlock [] }
  | expr=expr; SEMI                          { SExpr expr }
  | ID; ASSIGN; expr; SEMI                   { SAssign($1, $3) }
  | INT; ID; ASSIGN; expr; SEMI              { SDecl($2, $4) }
  | IF; LPAREN; cond=expr; RPAREN; s1=stmt; ELSE; s2=stmt { SIf(cond, s1, Some s2) }
  | IF; LPAREN; cond=expr; RPAREN; s=stmt    { SIf(cond, s, None) }
  | WHILE; LPAREN; cond=expr; RPAREN; s=stmt { SWhile(cond, s) }
  | BREAK; SEMI                              { SBreak }
  | CONTINUE; SEMI                           { SContinue }
  | RETURN; expr=expr; SEMI                  { SReturn (Some expr) }
  | RETURN; SEMI                             { SReturn None }
  | b=block                                  { b } 

expr:
  | or_expr                                  { $1 }

or_expr:
  | and_expr                                 { $1 }
  | e1=or_expr; OR; e2=and_expr              { EBinOp(Or, e1, e2) }

and_expr:
  | eq_expr                                  { $1 }
  | e1=and_expr; AND; e2=eq_expr             { EBinOp(And, e1, e2) }

eq_expr:
  | rel_expr                                 { $1 }
  | e1=eq_expr; op=eq_op; e2=rel_expr        { EBinOp(op, e1, e2) }

rel_expr:
  | add_expr                                 { $1 }
  | e1=rel_expr; op=rel_op; e2=add_expr      { EBinOp(op, e1, e2) }

add_expr:
  | mul_expr                                 { $1 }
  | e1=add_expr; op=add_op; e2=mul_expr      { EBinOp(op, e1, e2) }

mul_expr:
  | unary_expr                               { $1 }
  | e1=mul_expr; op=mul_op; e2=unary_expr    { EBinOp(op, e1, e2) }

unary_expr:
  | primary_expr                              { $1 }
  | MINUS; e=unary_expr %prec NOT            { EUnOp(Neg, e) }
  | PLUS; e=unary_expr %prec NOT             { e }
  | NOT; e=unary_expr                        { EUnOp(Not, e) }

primary_expr:
  | i=NUMBER                                 { EInt i }
  | i=ID                                     { EId i }
  | LPAREN; e=expr; RPAREN                   { e }
  | ID; LPAREN; args=separated_list(COMMA, expr); RPAREN { ECall($1, args) }

add_op:
  | PLUS  { Add }
  | MINUS { Sub }

mul_op:
  | TIMES { Mul }
  | DIV   { Div }
  | MOD   { Mod }

eq_op:
  | EQ    { Eq }
  | NEQ   { Neq }

rel_op:
  | LT    { Lt }
  | LE    { Le }
  | GT    { Gt }
  | GE    { Ge }