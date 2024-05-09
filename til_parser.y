%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                   i;          /* integer value */
  double               d;          /* double value */
  std::string          *s;          /* symbol name or string literal */
  cdk::basic_node      *node;       /* node pointer */
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; /* expression nodes */
  cdk::lvalue_node     *lvalue;
  std::vector<std::shared_ptr<cdk::basic_type>> *types;
  til::declaration_node *declaration;
};

%token <i> tINTLIT
%token <d> tDOUBLELIT
%token <s> tIDENTIFIER tSTRINGLIT
%token tINT tDOUBLE tSTRING tVOID
%token tEXTERNAL tFORWARD tPUBLIC tVAR
%token tBLOCK tIF tLOOP tSTOP tNEXT tRETURN tPRINT tPRINTLN
%token tREAD tNULL tSET tINDEX tOBJECTS tSIZEOF tFUNCTION
%token tPROGRAM
%token tGE tLE tEQ tNE tAND tOR

%type <node> stmt program
%type <sequence> list exprs declarations
%type <expression> expr
%type <lvalue> lval
%type <types> types
%type <type> type function_type
%type <declaration> declaration
%type <i> qualifier


%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

program : '(' tPROGRAM declarations list ')' { compiler->ast(new til::function_node(LINE, $3, $4)); }
        ;

list : stmt      { $$ = new cdk::sequence_node(LINE, $1); }
     | list stmt { $$ = new cdk::sequence_node(LINE, $2, $1); }
     ;

stmt : '(' expr ')'                     { $$ = new til::evaluation_node(LINE, $2); }
     ;

qualifier : tEXTERNAL                   { $$ = 2; }
          | tFORWARD                    { $$ = 3; }
          ; 

declarations : declaration                   { $$ = new cdk::sequence_node(LINE, $1); }
             | declarations declaration      { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;

declaration : type tIDENTIFIER                { $$ = new til::declaration_node(LINE, 0, $1, *$2, nullptr); delete $2;}
            | type tIDENTIFIER expr           { $$ = new til::declaration_node(LINE, 0, $1, *$2, $3); delete $2;}
            | qualifier type tIDENTIFIER      { $$ = new til::declaration_node(LINE, $1, $2, *$3, nullptr); delete $3;}
            | tPUBLIC type tIDENTIFIER        { $$ = new til::declaration_node(LINE, 1, $2, *$3, nullptr); delete $3;}
            | tPUBLIC type tIDENTIFIER expr   { $$ = new til::declaration_node(LINE, 1, $2, *$3, $4); delete $3;}
            | tVAR tIDENTIFIER expr           { $$ = new til::declaration_node(LINE, 0, nullptr, *$2, $3); delete $2;}
            | tPUBLIC tIDENTIFIER expr        { $$ = new til::declaration_node(LINE, 1, nullptr, *$2, $3); delete $2;}
            | tPUBLIC tVAR tIDENTIFIER expr   { $$ = new til::declaration_node(LINE, 1, nullptr, *$3, $4); delete $3;}
            ;

exprs : expr                      { $$ = new cdk::sequence_node(LINE, $1); }
      | exprs expr                { $$ = new cdk::sequence_node(LINE, $2, $1); }
      ;

expr : tINTLIT               { $$ = new cdk::integer_node(LINE, $1); }
     | tDOUBLELIT            { $$ = new cdk::double_node(LINE, $1); }
     | tSTRINGLIT            { $$ = new cdk::string_node(LINE, $1); delete $1; }
     | tNULL                 { $$ = new til::nullptr_node(LINE); }
     | '(' '-' expr ')'      { $$ = new cdk::unary_minus_node(LINE, $3); }
     | '(' '+' expr ')'            { $$ = new cdk::unary_plus_node(LINE, $3); }
     | '(' '~' expr ')'            { $$ = new cdk::not_node(LINE, $3); }
     | '(' '+' expr expr ')'       { $$ = new cdk::add_node(LINE, $3, $4); }
     | '(' '-' expr expr ')'        { $$ = new cdk::sub_node(LINE, $3, $4); }
     | '(' '*' expr expr ')'       { $$ = new cdk::mul_node(LINE, $3, $4); }
     | '(' '/' expr expr ')'       { $$ = new cdk::div_node(LINE, $3, $4); }
     | '(' '%' expr expr ')'       { $$ = new cdk::mod_node(LINE, $3, $4); }
     | '(' '<' expr expr ')'       { $$ = new cdk::lt_node(LINE, $3, $4); }
     | '(' '>' expr expr ')'       { $$ = new cdk::gt_node(LINE, $3, $4); }
     | '(' tGE expr expr ')'       { $$ = new cdk::ge_node(LINE, $3, $4); }
     | '(' tLE expr expr ')'       { $$ = new cdk::le_node(LINE, $3, $4); }
     | '(' tNE expr expr ')'       { $$ = new cdk::ne_node(LINE, $3, $4); }
     | '(' tEQ expr expr ')'       { $$ = new cdk::eq_node(LINE, $3, $4); }
     | '(' tAND expr expr ')'      { $$ = new cdk::and_node(LINE, $3, $4); }
     | '(' tOR expr expr ')'       { $$ = new cdk::or_node(LINE, $3, $4); }
     | '(' expr ')'                { $$ = $2; }
     | lval                        { $$ = new cdk::rvalue_node(LINE, $1); }
     | '(' tSET lval expr ')'      { $$ = new cdk::assignment_node(LINE, $3, $4); }
     | '(' tOBJECTS expr ')'       { $$ = new til::objects_operator_node(LINE, $3); }
     | '(' tSIZEOF expr ')'        { $$ = new til::sizeof_operator_node(LINE, $3); }
     | '(' '?' lval ')'            { $$ = new til::referencing_operator_node(LINE, $3);}
     | '(' tREAD ')'               { $$ = new til::read_node(LINE); }
     | '(' expr exprs ')'          { $$ = new til::function_call_node(LINE, $2, $3); }
     | '(' '@' exprs ')'           { $$ = new til::function_call_node(LINE, nullptr, $3); }
     ;

type : tINT                        { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
     | tDOUBLE                     { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
     | tSTRING                     { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
     | tVOID                       { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID); }
     | type '!'                    { $$ = cdk::reference_type::create(4, $1); }
     | function_type               { $$ = $1;}
     ;

types : type                       { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); $$->push_back($1); }
      | types type                 { $$ = $1; $$->push_back($2);}
      ;

function_type : '(' type ')'                 { $$ = cdk::functional_type::create($2);}
              | '(' type '(' types  ')' ')'   { $$ = cdk::functional_type::create(*$4, $2);}
              ;

lval : tIDENTIFIER             { $$ = new cdk::variable_node(LINE, $1); }
     | '(' tINDEX expr expr ')' { $$ = new til::pointer_indexing_node(LINE, $3, $4); }
     ;

%%
