%{

#include <stdlib.h>
#include <string.h>

//#include "scanner.hh"
// This is where you put #include directives as needed for later labs.
// include "ast.hh", parser.hh" in that order
#include "ast.hh"
#include "parser.hh"

int column = 0;

extern YYLTYPE yylloc; // Used for position information, see below.

%}

%option yylineno
%option 8bit
%option noyywrap
%option caseless
%option outfile = "scanner.cc"

/* If you want to include any flex declarations, this is where to do it. */

%x c_comment
%x c_string
DIGIT [0-9]
ID [a-z][a-z0-9]*
INTEGER {DIGIT}+
REAL1 [0-9]*"."{INTEGER}
REAL2 {INTEGER}"."[0-9]*
FLOAT ({REAL1}|{REAL2}|{SCIENTIFIC1}|{SCIENTIFIC2}|{SCIENTIFIC3}|{SCIENTIFIC4}|{SCIENTIFIC5})
SCIENTIFIC1 {DIGIT}{E_OR_e}{INTEGER}
SCIENTIFIC2 "."{DIGIT}{E_OR_e}{MORE_OR_LESS}{INTEGER}
SCIENTIFIC3 {DIGIT}{E_OR_e}{MORE_OR_LESS}{INTEGER}
SCIENTIFIC4 {DIGIT}"."{E_OR_e}{INTEGER}
SCIENTIFIC5 {DIGIT}"."{DIGIT}{E_OR_e}{INTEGER}
E_OR_e ("e"|"E")
MORE_OR_LESS ("+"|"-")

CARRIAGE \n

STRING_UNFINISHED '[^'\n]*\n
STRING '[^\n]*'
SPECIALCHAR ^[a-z0-9]
IDENTIFIER [a-z]([a-z]|{DIGIT}|[_])*
LETTERS [a-z]*
SPACE [ \t]*
COMMENT "{"
COMMENT1 "{".*"}"
/* {DIGIT}+"."{DIGIT}*   \}.*$ */

/* Position information is entered into the yylloc variable, so we will be
   able to generate good error messages later in later phases. */
/* Your code should be entered below the %%. Expressions to handle the
   following: Diesel comments, Diesel string constants, Diesel
   identifiers, integers, reals, and whitespace. */
%%

{SPACE}      {
                      yylloc.first_line = yylineno;
                      yylloc.first_column = column;
                      column += yyleng;
                            }

\.                       {
                            yylloc.first_line = yylineno;
                            yylloc.first_column = column;
                            column += yyleng;
                            return T_DOT;
                         }
;                        {
                            yylloc.first_line = yylineno;
                            yylloc.first_column = column;
                            column += yyleng;
                            return T_SEMICOLON;
                         }
=                        {
                            yylloc.first_line = yylineno;
                            yylloc.first_column = column;
                            column += yyleng;
                            return T_EQ;
                         }
\:                       {
                            yylloc.first_line = yylineno;
                            yylloc.first_column = column;
                            column += yyleng;
                            return T_COLON;
                         }
\(                       {
                            yylloc.first_line = yylineno;
                            yylloc.first_column = column;
                            column += yyleng;
                            return T_LEFTPAR;
                         }
\)                       {
                            yylloc.first_line = yylineno;
                            yylloc.first_column = column;
                            column += yyleng;
                            return T_RIGHTPAR;
                         }
\[                       {
                            yylloc.first_line = yylineno;
                            yylloc.first_column = column;
                            column += yyleng;
                            return T_LEFTBRACKET;
                         }
\]                       {
                            yylloc.first_line = yylineno;
                            yylloc.first_column = column;
                            column += yyleng;
                            return T_RIGHTBRACKET;
                         }
,                        {
                            yylloc.first_line = yylineno;
                            yylloc.first_column = column;
                            column += yyleng;
                            return T_COMMA;
                         }
\<                       {
                            yylloc.first_line = yylineno;
                            yylloc.first_column = column;
                            column += yyleng;
                            return T_LESSTHAN;
                         }
\>                       {
                            yylloc.first_line = yylineno;
                            yylloc.first_column = column;
                            column += yyleng;
                            return T_GREATERTHAN;
                         }
\+                       {
                            yylloc.first_line = yylineno;
                            yylloc.first_column = column;
                            column += yyleng;
                            return T_ADD;
                         }
\-                       {
                            yylloc.first_line = yylineno;
                            yylloc.first_column = column;
                            column += yyleng;
                            return T_SUB;
                         }
\*                       {
                            yylloc.first_line = yylineno;
                            yylloc.first_column = column;
                            column += yyleng;
                            return T_MUL;
                         }
\/                       {
                            yylloc.first_line = yylineno;
                            yylloc.first_column = column;
                            column += yyleng;
                            return T_RDIV;
                         }
":="                     {
                            yylloc.first_line = yylineno;
                            yylloc.first_column = column;
                            column += yyleng;
                            return T_ASSIGN;
                         }
"<>"                     {
                            yylloc.first_line = yylineno;
                            yylloc.first_column = column;
                            column += yyleng;
                            return T_NOTEQ;
                         }


of                       {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_OF;
                         }
if                       {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_IF;
                         }
do                       {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_DO;
                         }
or                       {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_OR;
                         }
var                      {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_VAR;
                         }
end                      {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_END;
                         }
and                      {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_AND;
                         }
div                      {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_IDIV;
                         }
mod                      {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_MOD;
                         }
not                      {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_NOT;
                         }
then                     {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_THEN;
                         }
else                     {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_ELSE;
                         }
const                    {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_CONST;
                         }
array                    {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_ARRAY;
                         }
begin                    {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_BEGIN;
                         }
while                    {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_WHILE;
                         }
elsif                    {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_ELSIF;
                         }
return                   {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_RETURN;
                         }
program                  {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_PROGRAM;
                         }
function                 {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_FUNCTION;
                         }
procedure                {
  yylloc.first_line = yylineno;
  yylloc.first_column = column;
  column += yyleng;
  return T_PROCEDURE;
                         }


{STRING}	{
		yylloc.first_line = yylineno;
		yylloc.first_column = column;
		column += yyleng;
		yylval.str = sym_tab->pool_install(sym_tab->fix_string(yytext));
		return T_STRINGCONST;
}

{STRING_UNFINISHED} {
    yylloc.first_line = yylineno;
    yylloc.first_column = column;
    column =0;
    yyerror("Newline in string");
}

\/\/.*$                  column = 0; /* Skip single-line comment */
"{"."}"			column=0;

("/\*"|"{")              {
                            column += yyleng;
                            BEGIN(c_comment);
                         }


<c_comment>
{
    "\*/"                {
                            column += 2;
                            BEGIN(INITIAL);
                         }

    "}"			{
        column += 2;
			BEGIN(INITIAL);
			}

    "/\*"                {
                            column += 2;
                            yyerror("Suspicious comment");
                         }
    [^\n]                column++; /* Skip stuff in comments */
    \n                   column = 0;
    <<EOF>>              {
                            yyerror("Unterminated comment");
                            yyterminate();
                         }
}

{FLOAT}                  {
                            yylloc.first_line = yylineno;
                            yylloc.first_column = column;
                            column += yyleng;
                            yylval.rval = atof(yytext);
                            return T_REALNUM;
                         }
{INTEGER}               {
                            yylloc.first_line = yylineno;
                            yylloc.first_column = column;
                            column += yyleng;
                            yylval.ival = atoi(yytext);
                            return T_INTNUM;
                          }
{IDENTIFIER}               {
        yylloc.first_line = yylineno;
        yylloc.first_column = column;
        column += yyleng;
        yylval.pool_p = sym_tab->pool_install(sym_tab->capitalize(yytext));
        return T_IDENT;
}

\n                   {
      column = 0;
    }

<<EOF>>                  yyterminate();
.                        yyerror("Illegal character");
