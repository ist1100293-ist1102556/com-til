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
  til::block_node *block;
  til::function_node *function;
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

%type <node> stmt program if_stmt loop_stmt
%type <sequence> list exprs declarations global_declarations arg_declarations
%type <expression> expr
%type <lvalue> lval
%type <types> types
%type <type> type function_type
%type <declaration> declaration global_declaration arg_declaration
%type <i> qualifier
%type <block> block
%type <function> function

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file : global_declarations program                  {compiler->ast(new cdk::sequence_node(LINE, $2, $1));}
     | global_declarations                           {compiler->ast($1);}
     | program                                      {compiler->ast(new cdk::sequence_node(LINE, $1));}
     | /* empty */                                  {compiler->ast(new cdk::sequence_node(LINE));}
     ;

program : '(' tPROGRAM declarations list ')' { $$ = new til::function_node(LINE, $3, $4); }
        | '(' tPROGRAM declarations ')'      { $$ = new til::function_node(LINE, $3, new cdk::sequence_node(LINE)); }
        | '(' tPROGRAM list ')'              { $$ = new til::function_node(LINE, new cdk::sequence_node(LINE), $3); }
        | '(' tPROGRAM ')'                   { $$ = new til::function_node(LINE, new cdk::sequence_node(LINE), new cdk::sequence_node(LINE)); }
        ;

function : '(' tFUNCTION '(' type ')' declarations list ')'                  { $$ = new til::function_node(LINE, $4, new cdk::sequence_node(LINE), $6, $7); }
         | '(' tFUNCTION '(' type ')' declarations ')'                       { $$ = new til::function_node(LINE, $4, new cdk::sequence_node(LINE), $6, new cdk::sequence_node(LINE)); }
         | '(' tFUNCTION '(' type ')' list ')'                               { $$ = new til::function_node(LINE, $4, new cdk::sequence_node(LINE), new cdk::sequence_node(LINE), $6); }
         | '(' tFUNCTION '(' type ')' ')'                                    { $$ = new til::function_node(LINE, $4, new cdk::sequence_node(LINE), new cdk::sequence_node(LINE), new cdk::sequence_node(LINE)); }
         | '(' tFUNCTION '(' type arg_declarations ')' declarations list ')' { $$ = new til::function_node(LINE, $4, $5, $7, $8); }
         | '(' tFUNCTION '(' type arg_declarations ')' declarations ')'      { $$ = new til::function_node(LINE, $4, $5, $7, new cdk::sequence_node(LINE)); }
         | '(' tFUNCTION '(' type arg_declarations ')' list ')'              { $$ = new til::function_node(LINE, $4, $5, new cdk::sequence_node(LINE), $7); }
         | '(' tFUNCTION '(' type arg_declarations ')' ')'                   { $$ = new til::function_node(LINE, $4, $5, new cdk::sequence_node(LINE), new cdk::sequence_node(LINE)); }
         ;

list : stmt      { $$ = new cdk::sequence_node(LINE, $1); }
     | list stmt { $$ = new cdk::sequence_node(LINE, $2, $1); }
     ;

stmt : expr                           { $$ = new til::evaluation_node(LINE, $1); }
     | '(' tPRINT exprs ')'           { $$ = new til::print_node(LINE, $3, false); }
     | '(' tPRINTLN exprs ')'         { $$ = new til::print_node(LINE, $3, true); }
     | '(' tSTOP ')'                  { $$ = new til::stop_node(LINE, 1); }
     | '(' tSTOP tINTLIT ')'          { $$ = new til::stop_node(LINE, $3); }
     | '(' tNEXT ')'                  { $$ = new til::next_node(LINE, 1); }
     | '(' tNEXT tINTLIT ')'          { $$ = new til::next_node(LINE, $3); }
     | '(' tRETURN ')'                { $$ = new til::return_node(LINE, nullptr); }
     | '(' tRETURN expr ')'           { $$ = new til::return_node(LINE, $3); }
     | if_stmt                        { $$ = $1; }
     | loop_stmt                      { $$ = $1; }
     | block                          { $$ = $1; }
     ;

if_stmt : '(' tIF expr stmt ')'       { $$ = new til::if_node(LINE, $3, $4); }
        | '(' tIF expr stmt stmt ')'  { $$ = new til::if_else_node(LINE, $3, $4, $5); }
        ;

loop_stmt : '(' tLOOP expr stmt ')'       { $$ = new til::loop_node(LINE, $3, $4); }
          ;

block : '(' tBLOCK declarations list ')' { $$ = new til::block_node(LINE, $3, $4); }
      | '(' tBLOCK declarations ')'      { $$ = new til::block_node(LINE, $3, new cdk::sequence_node(LINE)); }
      | '(' tBLOCK list ')'              { $$ = new til::block_node(LINE, new cdk::sequence_node(LINE), $3); }
      | '(' tBLOCK ')'                   { $$ = new til::block_node(LINE, new cdk::sequence_node(LINE), new cdk::sequence_node(LINE)); }
      ;

qualifier : tEXTERNAL                   { $$ = 2; }
          | tFORWARD                    { $$ = 3; }
          ; 

global_declarations : global_declaration                     {$$ = new cdk::sequence_node(LINE, $1);}
                    | global_declarations global_declaration {$$ = new cdk::sequence_node(LINE, $2, $1);}
                    ;

global_declaration : declaration                             { $$ = $1;}
                   | '(' qualifier type tIDENTIFIER ')'      { $$ = new til::declaration_node(LINE, $2, $3, *$4, nullptr); delete $4;}
                   | '(' tPUBLIC type tIDENTIFIER ')'        { $$ = new til::declaration_node(LINE, 1, $3, *$4, nullptr); delete $4;}
                   | '(' tPUBLIC type tIDENTIFIER expr ')'   { $$ = new til::declaration_node(LINE, 1, $3, *$4, $5); delete $4;}
                   | '(' tPUBLIC tIDENTIFIER expr ')'        { $$ = new til::declaration_node(LINE, 1, nullptr, *$3, $4); delete $3;}
                   | '(' tPUBLIC tVAR tIDENTIFIER expr ')'   { $$ = new til::declaration_node(LINE, 1, nullptr, *$4, $5); delete $4;}
                   ;

declarations : declaration                   { $$ = new cdk::sequence_node(LINE, $1); }
             | declarations declaration      { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;

declaration : '(' type tIDENTIFIER ')'                { $$ = new til::declaration_node(LINE, 0, $2, *$3, nullptr); delete $3;}
            | '(' type tIDENTIFIER expr ')'           { $$ = new til::declaration_node(LINE, 0, $2, *$3, $4); delete $3;}
            | '(' tVAR tIDENTIFIER expr ')'           { $$ = new til::declaration_node(LINE, 0, nullptr, *$3, $4); delete $3;}
            ;

arg_declarations : arg_declaration                   { $$ = new cdk::sequence_node(LINE, $1); }
                 | arg_declarations arg_declaration  { $$ = new cdk::sequence_node(LINE, $2, $1); }
                 ;

arg_declaration : '(' type tIDENTIFIER ')'           { $$ = new til::declaration_node(LINE, 0, $2, *$3, nullptr); delete $3;}
                ; 

exprs : expr                      { $$ = new cdk::sequence_node(LINE, $1); }
      | exprs expr                { $$ = new cdk::sequence_node(LINE, $2, $1); }
      ;

expr : tINTLIT                     { $$ = new cdk::integer_node(LINE, $1); }
     | tDOUBLELIT                  { $$ = new cdk::double_node(LINE, $1); }
     | tSTRINGLIT                  { $$ = new cdk::string_node(LINE, $1); delete $1; }
     | tNULL                       { $$ = new til::nullptr_node(LINE); }
     | '(' '-' expr ')'            { $$ = new cdk::unary_minus_node(LINE, $3); }
     | '(' '+' expr ')'            { $$ = new cdk::unary_plus_node(LINE, $3); }
     | '(' '~' expr ')'            { $$ = new cdk::not_node(LINE, $3); }
     | '(' '+' expr expr ')'       { $$ = new cdk::add_node(LINE, $3, $4); }
     | '(' '-' expr expr ')'       { $$ = new cdk::sub_node(LINE, $3, $4); }
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
     | function                    { $$ = $1; }
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

function_type : '(' type ')'                  { $$ = cdk::functional_type::create($2);}
              | '(' type '(' types  ')' ')'   { $$ = cdk::functional_type::create(*$4, $2);}
              ;

lval : tIDENTIFIER              { $$ = new cdk::variable_node(LINE, $1); }
     | '(' tINDEX expr expr ')' { $$ = new til::pointer_indexing_node(LINE, $3, $4); }
     ;

%%
