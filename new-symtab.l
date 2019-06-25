%{

#include "y.tab.h"
#include"header.h"

 int inDouble = 0;
	    int inComment = 0,inDoublecom=0;
	    int line_number = 1;
		extern int gtype;
		extern symtab	** head;

%}
a "%d"|"%f"|"%c"|"%s"
%x C_COMMENT

%%

"int"				{yylval.str=strdup(yytext);return INT;}
"float"				{yylval.str=strdup(yytext);return FLOAT;}
"char"				{yylval.str=strdup(yytext);return CHAR;}
"double"			{yylval.str=strdup(yytext);return DOUBLE;}
"main"				return MAIN;
"if"				return IF;
"else"				return ELSE;
"switch"			return SWITCH;
"case"				return CASE;
"default"			return DEFAULT;
"break"				return BREAK;
"printf"			return PRINTF;
"scanf"				return SCANF;
"#include"			return INCLUDE;
"stdio.h"|"string.h"|"stdlib.h"	{return STD;}
"&&"				{yylval.str=strdup(yytext);return LOGAND;}
"||"				{ yylval.str=strdup(yytext);return LOGOR;}
;					return *yytext;
":"					return *yytext;
"["|"{"|"}"			return *yytext;
"]"  				return *yytext;
"(" 				return *yytext;
")"					return *yytext;
,					return *yytext;
=					return *yytext;
"+"					return *yytext;
-					return *yytext;
"*"					return *yytext;
"/"					return *yytext;
"^"					return *yytext;
"<"					{yylval.str=strdup(yytext);return LT;}
">"					{yylval.str=strdup(yytext);return GT;}
">="				{yylval.str=strdup(yytext);return GE;}
"<="				{yylval.str=strdup(yytext);return LE;}
"!="				{yylval.str=strdup(yytext);return NQ;}
"=="				{yylval.str=strdup(yytext);return EE;}
"++"				return INC;
"--"				return DEC;
"+="				return PE;
"-="				return MIE;
"*="				return MUE;
"/="				return DE;

printf\((\"({a}*|.*)*\"(,[a-zA-Z][a-zA-Z0-9]*)*\))\; {
										int i,j=0;
										for(i=0;i<yyleng;i++)
										{
											if(yytext[i]=='%')
											 j++;
                                            if(yytext[i]==',')
											 j--;
										}
										if(j==0)
										 printf("Correct..!!");
									 	else
								         printf("Incorrect..!!");
								        }
scanf\((\"({a}*|.*)*\"(,\&[a-zA-Z][a-zA-Z0-9]*)*\))\; {
										int i,j=0;
										for(i=0;i<yyleng;i++)
										{
										 if(yytext[i]=='%')
										 j++;
										 if(yytext[i]==',')
										 j--;
										}
										 if(j==0)
										  printf("Correct");
										 else
										  printf("Incorrect");
									  }
[A-Za-z]+[A-Za-z0-9_]*	{
							yylval.str=strdup(yytext);
						//	strcat(yylval.str,"\0");
							return ID;
						}
[0-9]+			{
					yylval.str=strdup(yytext);
					return NUM;
				}
[0-9]+\.[0-9]*   {
					yylval.str=strdup(yytext);
					//printf("inlex %s\n",yylval.str);
					//strcat(yylval.str,"\0");
					return NUM_F;
			      }

"//".* { if (inDouble && !inComment) printf("Unexpected1: %s\n", yytext); else {inDoublecom++;}}
"/*" { BEGIN(C_COMMENT); if(!inDoublecom) inComment=1;}
<C_COMMENT>"*/"	  { BEGIN(INITIAL);if (inComment) {inComment=0;}
		else
			printf("Unexpected2: %s\n", yytext);
	  }
<C_COMMENT>\n {if(inDoublecom) inDoublecom=0; line_number++;}
<C_COMMENT>[ \t] {}
<C_COMMENT>. {if (!inComment) {printf("Unexpected3: %s\n", yytext); exit(-1);}}

\n {if(inDoublecom) inDoublecom=0; line_number++;}
"\t"				;
" "					;
.					return *yytext;

%%

int yywrap(void)
{
	return 1;
}
