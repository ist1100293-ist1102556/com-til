%option c++ prefix="til_scanner_" outfile="til_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "til_parser.tab.h"

// don't change this
#define yyerror LexerError

#define PROCESS_DOUBLE { \
  try { \
    yylval.d = std::stod(yytext, nullptr); \
    return tDOUBLELIT; \
  } catch (const std::out_of_range &e) { \
    yyerror("double out of range"); \
  } \
} \

%}
EXP [Ee][+-]?[0-9]+

%x X_STRING X_COMMENT X_STRING_END
%%
  yydebug=1; set_debug(1);

";".*                  ; /* ignore single line comments */
"/*"                   yy_push_state(X_COMMENT);
<X_COMMENT>"/*"        yy_push_state(X_COMMENT);
<X_COMMENT>"*/"        yy_pop_state();
<X_COMMENT>.|\n        ;

">="                   return tGE; 
"<="                   return tLE; 
"=="                   return tEQ; 
"!="                   return tNE;
"&&"                   return tAND;
"||"                   return tOR;

"int"                  return tINT;
"double"               return tDOUBLE;
"string"               return tSTRING;
"void"                 return tVOID;

"external"             return tEXTERNAL;
"forward"              return tFORWARD;
"public"               return tPUBLIC;
"var"                  return tVAR;

"block"                return tBLOCK;
"if"                   return tIF; 
"loop"                 return tLOOP;
"stop"                 return tSTOP;
"next"                 return tNEXT;
"return"               return tRETURN;
"print"                return tPRINT; 
"println"              return tPRINTLN;

"read"                 return tREAD;
"null"                 return tNULL;
"set"                  return tSET;
"index"                return tINDEX;
"objects"              return tOBJECTS;
"sizeof"               return tSIZEOF;
"function"             return tFUNCTION;

"program"              return tPROGRAM;


[A-Za-z][A-Za-z0-9_]*  yylval.s = new std::string(yytext); return tIDENTIFIER;

\"                     yy_push_state(X_STRING); yylval.s = new std::string("");
<X_STRING>\"           yy_pop_state(); return tSTRINGLIT;
<X_STRING>\\\"         *yylval.s += '\"';
<X_STRING>\\\\         *yylval.s += '\\';
<X_STRING>\\n          *yylval.s += '\n';
<X_STRING>\\t          *yylval.s += '\t';
<X_STRING>\\r          *yylval.s += '\r';
<X_STRING>\\0          yy_push_state(X_STRING_END);
<X_STRING>\n           yyerror("newline in string");
<X_STRING>\0           yyerror("null character in string");
<X_STRING>\\[0-7]{1,3} {
                         int n = std::stoi(yytext+1, nullptr, 8);
                         if (n > 255) yyerror("invalid 8 bit character");
                         if (n == 0)
                            yy_push_state(X_STRING_END);
                         else
                            *yylval.s += (char) n;
                       }
<X_STRING>.            *yylval.s += yytext;

<X_STRING_END>\"       yy_pop_state(); yy_pop_state(); return tSTRINGLIT;
<X_STRING_END>\\\"     ;
<X_STRING_END>\\\\     ;
<X_STRING_END>\n       yyerror("newline in string");
<X_STRING_END>\0       yyerror("null character in string");
<X_STRING_END>.        ;

0|[1-9][0-9]*          {
                        try {
                          yylval.i = std::stoi(yytext, nullptr, 10);
                          return tINTLIT;
                        } catch (const std::out_of_range &e) {
                          yyerror("integer out of range");
                        }
                       }

0x[0-9A-Fa-f]+         {
                        try {
                          yylval.i = std::stoi(yytext, nullptr, 16);
                          return tINTLIT;
                        } catch (const std::out_of_range &e) {
                          yyerror("integer out of range");
                        }
                       }

[0-9]+\.[0-9]+{EXP}?   PROCESS_DOUBLE;
[0-9]+\.{EXP}?         PROCESS_DOUBLE;
\.[0-9]+{EXP}?         PROCESS_DOUBLE;
[0-9]+{EXP}            PROCESS_DOUBLE;

[+-?*/%<>~()!]         return *yytext;

[ \t\n]+               ; /* ignore whitespace */

.                      yyerror("Unknown character");

%%
