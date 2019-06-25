%{
	#include"header.h"
	int sym_c=-1;
	int sym_t=15;
	int gtype=-1;

	struct expr
	{
		double num;
		int hasval;
		int type;
	};

	typedef struct expr expr;

	int error;
	int yylex();

%}

%union
{
	char *str;
	double num;
	struct expr* exp;

}

%token	 MAIN VOID SWITCH CASE IF ELSE CONTINUE BREAK RETURN PRINTF SCANF DEFAULT INCLUDE STD
%token INC DEC PE MIE MUE DE
%token<str> ID NUM NUM_F
%token <str> LT LE GE GT LOGAND LOGOR EE NQ  CHAR DOUBLE FLOAT INT
%type<exp> E T F UnaryExpr
%type<num> X Y Z



%%
S 		: Start   {
						YYACCEPT;
				}
		;

Start	: Main
		| Header Start
		;
Header	: INCLUDE LT Hfile GT
		;

Hfile	:STD
		;

Main	: INT MAIN'('')' Compound_stmt
		;

Open_b : '{'		{
						//to create symbol table for each scope
						sym_c++;
						head[sym_c]=(symtab *)malloc(sizeof(symtab));
						head[sym_c]->max_symentry=50;
						head[sym_c]->symencount=0;
						head[sym_c]->symentry=(symtabentry **)malloc(sizeof(symtabentry*)*(head[sym_c]->max_symentry));
						head[sym_c]->max_arrcount=50;
						head[sym_c]->arrcount=0;
						head[sym_c]->arrentry=(arrays **)malloc(sizeof(arrays*)*(head[sym_c]->max_arrcount));

						symtab_load(head[sym_c]);
					}
	   ;

Close_b	: '}'		{

							//deleting symbol table of a particular scope if it is out of scope
						printf("\nScope %d\n",sym_c+1);
						symtab *temp=head[sym_c];
							symbol_print(temp);

							printf("Scope %d finished\n",sym_c+1);
							sym_c--;
							free(temp);

					}
		;

Compound_stmt 	: Open_b Stmt Close_b
				;


Stmt	: AssignExpr Stmt
		| Compound_stmt Stmt
		| DECL Stmt
		| Selection_stmt Stmt
		| Switch_stmt Stmt
		| Jump_stmt Stmt
		| UnaryExpr ';' Stmt
		|
		;

DECL	:  TYPE VAR_LIST';'
		;

TYPE   	: INT		{gtype=0;}
		| FLOAT		{gtype=1;}
		| DOUBLE	{gtype=2;}
		| CHAR		{gtype=3;}
		;

VAR_LIST : VAR','VAR_LIST  {gtype=-1;}
		 | VAR				{gtype=-1;}
		;

VAR	: ID 		{
					//inserting id into symbol table
					int f=insert(yylval.str,gtype,line_number,-1,-1,-1);
					int pos=search_symtab(head[sym_c],$1);
					head[sym_c]->symentry[pos]->hasval=0;
				}
	|ID'='E   {
				//inserting id and value into symbol table

					int f=insert($1,gtype,line_number,-1,-1,-1);

					if(f==1)
					{
						int pos=search_symtab(head[sym_c],$1);
						if($3->hasval)
						{
							if(gtype==0)
								head[sym_c]->symentry[pos]->val.it=(int)$3->num;
							else if(gtype==1)
								head[sym_c]->symentry[pos]->val.ft=(float)$3->num;
							else if(gtype==2)
								head[sym_c]->symentry[pos]->val.db=(double)$3->num;
							head[sym_c]->symentry[pos]->hasval=1;
						}
						else
							head[sym_c]->symentry[pos]->hasval=0;


					}
				}
	|ID'['NUM']'{
					int f=insert($1,gtype,line_number,0,1,(int)atof($3));
				}
	|ID'['NUM']''['NUM']'{
							int f=insert($1,gtype,line_number,1,(int)atof($3),(int)atof($6));
						 }
	;

AssignExpr	: ID'='E';'				{
										int pos=search_symtab(head[sym_c],$1);//cheack for id in current scope
										int c=sym_c;
										if(pos==-1)
										{	//check for id in main scope if not in local scope
											pos=search_symtab(head[0],$1);
											if(pos>=0)
												c=0;
										}
										if(pos>=0)
										{
											int t=head[c]->symentry[pos]->type;

											if($3->hasval==1)
											{
												if(t==0)
													head[c]->symentry[pos]->val.it=(int)$3->num;
												else if(t==1)
													head[c]->symentry[pos]->val.ft=(float)$3->num;
												else if(t==2)
													head[c]->symentry[pos]->val.db=(double)$3->num;
												head[c]->symentry[pos]->hasval=1;
											}

										}

									}
			| ID'['NUM']''='E';'	{	int pos=search_symtab(head[sym_c],$1);
									int c=sym_c;
									int index=(int)atof($3);
									//printf("ind %d\n",index);
									if(pos==-1)
									{	//check for id in main scope if not in local scope
										pos=search_symtab(head[0],$1);
										if(pos>=0)
											c=0;
									}
									if(pos>=0)
									{
										if(index<head[c]->arrentry[pos]->n)
										{
											int t=head[c]->arrentry[pos]->type;
											if($6->hasval==1)
											{

												if(t==0)
													head[c]->arrentry[pos]->arrele->it[index]=(int)$6->num;
												else if(t==1)
													head[c]->arrentry[pos]->arrele->ft[index]=(float)$6->num;
												else if(t==2)
													head[c]->arrentry[pos]->arrele->db[index]=(double)$6->num;
												head[c]->symentry[pos]->hasval=1;
											}
										}
										else
											{
												printf("index out of range\n");
												exit(0);

											}
									}
							}
			| ID'['NUM']''['NUM']''='E';' {	int pos=search_symtab(head[sym_c],$1);
									int c=sym_c;
									int index;
									if(pos==-1)
									{	//check for id in main scope if not in local scope
										pos=search_symtab(head[0],$1);
										if(pos>=0)
											c=0;
									}
									if(pos>=0)
									{
										int i=(int)atof($3),j=(int)atof($6);
										index=(head[c]->arrentry[pos]->n)*i+j;
										if(i<head[c]->arrentry[pos]->m && j<head[c]->arrentry[pos]->n){
											int t=head[c]->arrentry[pos]->type;

											if(t==0)
												head[c]->arrentry[pos]->arrele->it[index]=(int)$9->num;
											else if(t==1)
												head[c]->arrentry[pos]->arrele->ft[index]=(float)$9->num;
											else if(t==2)
												head[c]->arrentry[pos]->arrele->db[index]=(double)$9->num;
											}
										else
											{
												printf("index out of range\n");
												exit(0);

											}
									}
							}
			;




E	: E'+'T		{
					if($1->hasval && $3->hasval)
					{
						$$->num=$1->num+$3->num;
						$$->hasval=1;
					}
					else
					{
						$$->num=0;
						$$->hasval=0;
					}

				}
	| E'-'T		{
					if($1->hasval && $3->hasval)
					{
						$$->num=($1->num)-($3->num);
						$$->hasval=1;
					}
					else
					{
						$$->num=0;
						$$->hasval=0;
					}
				}
	| T			{$$=$1;}
	;

T	: T'*'F		{
					if($1->hasval && $3->hasval)
					{
						$$->num=($1->num)*($3->num);
						$$->hasval=1;
					}
					else
					{
						$$->num=0;
						$$->hasval=0;
					}
				}
	| T'/'F		{
					if($1->hasval && $3->hasval)
					{
						$$->num=($1->num)/($3->num);
						$$->hasval=1;
					}
					else
					{
						$$->num=0;
						$$->hasval=0;
					}
				}
	| F			{$$=$1;}
	;

F	:'('E')'	{$$=$2;}
	|NUM		{$$->num=(double)atof($1);
				 $$->hasval=1;
				 $$->type=0;
				}
	|NUM_F		{
					$$->num=(double)atof($1);
					$$->hasval=1;
					$$->type=1;
				}
	|ID			{	//searching for an id and get its value
					int pos=search_symtab(head[sym_c],$1);

					if(pos>=0)
					{
						int t=head[sym_c]->symentry[pos]->type;
						if(gtype==-1)
							gtype=t;

						if(head[sym_c]->symentry[pos]->hasval==1)
						{
							if(t==0)
								$$->num=(double)(head[sym_c]->symentry[pos]->val.it);
							else if(t==1)
								$$->num=(double)(head[sym_c]->symentry[pos]->val.ft);
							else if(t==2)
								$$->num=(double)(head[sym_c]->symentry[pos]->val.db);
						}
						$$->hasval=head[sym_c]->symentry[pos]->hasval;
						$$->type=t;

					}
					else if(pos==-1)
					{
							pos=search_symtab(head[0],$1);
							if(pos>=0)
							{
								int t=head[0]->symentry[pos]->type;
								if(gtype==-1)
									gtype=t;
								if(head[0]->symentry[pos]->hasval==1)
								{
									if(t==0)
										$$->num=(double)(head[0]->symentry[pos]->val.it);
									else if(t==1)
										$$->num=(double)(head[0]->symentry[pos]->val.ft);
									else if(t==2)
										$$->num=(double)(head[0]->symentry[pos]->val.db);
								}
								$$->hasval=head[0]->symentry[pos]->hasval;
								$$->type=t;

							}
							else
							{
								printf("error : variable %s not declared\n",yylval.str);
								exit(0);
							}
					}
					else
					{
						printf("error : variable %s not declared\n",yylval.str);
						exit(0);
					}
				}
	|ID'['NUM']'			{    int pos=search_symtab(head[sym_c],$1);
									int c=sym_c;
									int index=(int)atof($3);
									//printf("ind %d\n",index);
									if(pos==-1)
									{	//check for id in main scope if not in local scope
										pos=search_symtab(head[0],$1);
										if(pos>=0)
											c=0;
									}
									if(pos>=0)
									{
										if(index<head[c]->arrentry[pos]->n){
											int t=head[c]->arrentry[pos]->type;
											if(t==0)
												$$->num=(double)head[c]->arrentry[pos]->arrele->it[index];
											else if(t==1)
												$$->num=(double)head[c]->arrentry[pos]->arrele->ft[index];
											else if(t==2)
												$$->num=(double)head[c]->arrentry[pos]->arrele->db[index];
											$$->hasval=1;
											$$->type=t;
											}
										else
											{
												printf("index out of range\n");
												exit(0);

											}
									}
									else
									{
										printf("error : variable %s not declared\n",$1);
										exit(0);
									}

							}
	| ID'['NUM']''['NUM']' 		{	int pos=search_symtab(head[sym_c],$1);
									int c=sym_c;
									int index;
									if(pos==-1)
									{	//check for id in main scope if not in local scope
										pos=search_symtab(head[0],$1);
										if(pos>=0)
											c=0;
									}
									if(pos>=0)
									{
										int i=(int)atof($3),j=(int)atof($6);
										index=(head[c]->arrentry[pos]->n)*i+j;
										if(i<head[c]->arrentry[pos]->m && j<head[c]->arrentry[pos]->n){
											int t=head[c]->arrentry[pos]->type;
											if(t==0)
												$$->num=(double)head[c]->arrentry[pos]->arrele->it[index];
											else if(t==1)
												$$->num=(double)head[c]->arrentry[pos]->arrele->ft[index];
											else if(t==2)
												$$->num=(double)head[c]->arrentry[pos]->arrele->db[index];
											$$->hasval=1;
											$$->type=t;
											}
										else
											{
												printf("index out of range\n");
												exit(0);

											}
									}
									else
									{
										printf("error : variable %s not declared\n",$1);
										exit(0);
									}
							}
	|UnaryExpr {$$=$1;}
	;



Selection_stmt  : IF'('COND')' Compound_stmt
				| IF'('COND')' Compound_stmt ELSE Compound_stmt
				;

COND    : EXPR LOGAND COND
		| EXPR LOGOR COND
		| EXPR
		;

EXPR	: RELEXPR
		| LOGEXP
		;

RELEXPR : E RELOP E
	;

LOGEXP : E LOGOP E
		;

RELOP	: GT 
		| LT
		| LE
		| GE
		| NQ
		| EE
		;
LOGOP : LOGAND
		| LOGOR
		;

UnaryExpr :DEC ID	{
					int pos=search_symtab(head[sym_c],$2);//cheack for id in current scope
					int c=sym_c;
					expr* temp=(expr *)malloc(sizeof(expr));
					if(pos==-1)
					{	//check for id in main scope if not in local scope
						pos=search_symtab(head[0],$2);
						if(pos>=0)
							c=0;
					}
					if(pos>=0)
					{
						int t=head[c]->symentry[pos]->type;
						if(head[c]->symentry[pos]->hasval==1)
						{
							if(t==0)
								temp->num=(double)(--(head[c]->symentry[pos]->val.it));
							else if(t==1)
								temp->num=(double)(--(head[c]->symentry[pos]->val.ft));
							else if(t==2)
								temp->num=(double)(--(head[c]->symentry[pos]->val.db));
							temp->hasval=1;
						}
						else
						{
							temp->num=0;
							temp->hasval=0;
						}
						temp->type=t;
						$$=temp;
					}
					else
					{
						printf("error : variable %s not declared\n",$2);
						exit(0);
					}
				 }
	  |INC ID	{
					int pos=search_symtab(head[sym_c],$2);//cheack for id in current scope
					int c=sym_c;
					expr* temp=(expr *)malloc(sizeof(expr));
					if(pos==-1)
					{	//check for id in main scope if not in local scope
						pos=search_symtab(head[0],$2);
						if(pos>=0)
							c=0;
					}
					if(pos>=0)
					{
						int t=head[c]->symentry[pos]->type;

						if(head[c]->symentry[pos]->hasval==1)
						{
							if(t==0)
								temp->num=(double)++(head[c]->symentry[pos]->val.it);
							else if(t==1)
								temp->num=(double)++(head[c]->symentry[pos]->val.ft);
							else if(t==2)
								temp->num=(double)++(head[c]->symentry[pos]->val.db);
							temp->hasval=1;
						}
						else
						{
							temp->num=0;
							temp->hasval=0;
						}
						temp->type=t;
						$$=temp;
					}
					else
					{
						printf("error : variable %s not declared\n",$2);
						exit(0);
					}

				 }
		|ID INC {
					int pos=search_symtab(head[sym_c],$1);//cheack for id in current scope
					int c=sym_c;
					expr* temp=(expr *)malloc(sizeof(expr));
					if(pos==-1)
					{	//check for id in main scope if not in local scope
						pos=search_symtab(head[0],$1);
						if(pos>=0)
							c=0;
					}
					if(pos>=0)
					{
						int t=head[c]->symentry[pos]->type;
						if(head[c]->symentry[pos]->hasval==1)
						{
							if(t==0)
								temp->num=(double)(head[c]->symentry[pos]->val.it)++;
							else if(t==1)
								temp->num=(double)(head[c]->symentry[pos]->val.ft)++;
							else if(t==2)
								temp->num=(double)(head[c]->symentry[pos]->val.db)++;
							temp->hasval=1;
						}
						else
						{
							temp->num=0;
							temp->hasval=0;
						}
						temp->type=t;
						$$=temp;
					}
					else
					{
						printf("error : variable %s not declared\n",$1);
						exit(0);
					}

				 }
		|ID DEC	{
					int pos=search_symtab(head[sym_c],$1);//cheack for id in current scope
					expr* temp=(expr *)malloc(sizeof(expr));

					int c=sym_c;
					if(pos==-1)
					{	//check for id in main scope if not in local scope
						pos=search_symtab(head[0],$1);
						if(pos>=0)
							c=0;
					}
					if(pos>=0)
					{
						int t=head[c]->symentry[pos]->type;
						if(head[c]->symentry[pos]->hasval==1)
						{
							if(t==0)
								temp->num=(double)(head[c]->symentry[pos]->val.it)--;
							else if(t==1)
								temp->num=(double)(head[c]->symentry[pos]->val.ft)--;
							else if(t==2)
								temp->num=(double)(head[c]->symentry[pos]->val.db)--;
							temp->hasval=1;
						}
						else
						{
							temp->num=0;
							temp->hasval=0;
						}
						temp->type=t;
						$$=temp;
					}
					else
					{
						printf("error : variable %s not declared\n",$1);
						exit(0);
					}

				 }
			;

Switch_stmt : SWITCH'('E')' Open_b Multi_case_stmt Close_b
   ;

Multi_case_stmt : Case_stmt Multi_case_stmt
                | Case_stmt
                ;

Case_stmt : CASE LABEL ':' Stmt
          | DEFAULT ':'Stmt
          ;

Jump_stmt : BREAK';'
		|CONTINUE';'
		;



X	: X'+'Y		{$$=$1+$3;}
	| X'-'Y		{$$=$1-$3;}
	| Y
	;

Y	: Y'*'Z		{$$=$1*$3;}
	| Y'/'Z		{$$=$1/$3;}
	| Z
	;

Z	:'('X')'	{$$=$2;}
	|NUM		{$$=(double)atof($1);}
	|NUM_F		{$$=(double)atof($1);}
	;

LABEL : X
	  |	X RELOP X
	  | X LOGOP X
	  ;

%%

void yyerror(const char * s)
{
	printf("%s\n",s);
}

//loading keywords into symbol table
void symtab_load(symtab * sym)
{
	int total_k=14;
	char key[16][50]= {"if","else","break","continue","return","int","float","char","main","switch","case","default","double","printf","scanf"};
	int i;
	char des[50]="keyword";

	for(i=0;i<=total_k;i++)
	{
		//printf("%s\n",key[i]);
		insert_symentry(sym,key[i],des,-1,-1);

	}
}

int insert_symentry(symtab * sym,char *s_n,char des[],int typ,int line)
{

	symtabentry *new=(symtabentry *)malloc(sizeof(symtabentry));
	new->symname=(char *)malloc(sizeof(char)*strlen(s_n));
	strcpy(new->symname,s_n);
	strcpy(new->desc,des);
	new->type=typ;
	new->lineno=line;

	if(sym->symencount>=sym->max_symentry)
	{
		sym->max_symentry=(sym->max_symentry)*2;
		sym->symentry=(symtabentry **)realloc(sym->symentry,sizeof(symtabentry *)*sym->max_symentry);
	}

    sym->symentry[sym->symencount]=(symtabentry *)malloc(sizeof(symtabentry));
	sym->symentry[sym->symencount]=new;
	(sym->symencount)++;

	return 1;

}

int search_symtab(symtab *sym,char *id)
{
	int i;
	char s[50]="keyword";

	for(i=0;i<sym->symencount;i++)
	{
		if(strcmp( sym->symentry[i]->symname,id)==0)
		{
			if(strcmp(sym->symentry[i]->desc,s)==0)
			{
				return -2;
			}
			else
				return i;

		}
	}
	for(i=0;i<sym->arrcount;i++)
	{
		if(strcmp(sym->arrentry[i]->symname,id)==0)
		{
			return i;
		}
	}
	return -1;
}

int insert(char *st,int typ,int line,int atype,int nrows,int ncol)
{

	int f=search_symtab(head[sym_c],st);

	char s[50]="identifier";
	if(f==-2)
	{
		//fprintf(file," line %d :error : It is a keyword\n",line_number);

		printf("line %d :error : %s is a keyword\n",line_number,st);
		exit(0);
		return -1;
	}
	else if(f>=0)
	{
		//fprintf(file,"line %d :Variable %s already exists\n",line_number,yylval.str);
		printf("line %d :Variable %s already exists\n",line_number,yylval.str);
			exit(0);
		return -1;
	}
	else if(f==-1)
	{
		if(atype==-1){
		insert_symentry(head[sym_c],st,s,typ,line);
		return 1;
		}

		else
		{
		insert_array(head[sym_c],st,typ,line,atype,nrows,ncol);
		return 1;
		}
	}
}

int insert_array(symtab *sym,char *st,int typ,int line,int atype,int nrows,int ncol)
{

	arrays *new=(arrays *)malloc(sizeof(arrays));
	new->symname=(char *)malloc(sizeof(char)*strlen(st));
	strcpy(new->symname,st);
	new->type=typ;
	new->arraytype=atype;
	new->m=nrows;
	new->n=ncol;
	new->lineno=line;
	new->arrele=(valarr *)malloc(sizeof(valarr));

	int total=nrows*ncol;
	if(typ==0)
	{
		new->arrele->it=(int *)malloc(sizeof(int)*total);
	}
	else if(typ==1)
	{
		new->arrele->ft=(float *)malloc(sizeof(float)*total);
	}
	else if(typ==2)
	{
		new->arrele->db=(double *)malloc(sizeof(double)*total);
	}
	else if(typ==3)
	{
		new->arrele->ch=(char *)malloc(sizeof(char)*total);
	}


	if(sym->arrcount>=sym->max_arrcount)
	{
		sym->max_arrcount=(sym->max_arrcount)*2;
		sym->arrentry=(arrays **)realloc(sym->arrentry,sizeof(arrays *)*sym->max_arrcount);
	}

    sym->arrentry[sym->arrcount]=(arrays *)malloc(sizeof(arrays));
	sym->arrentry[sym->arrcount]=new;
	(sym->arrcount)++;

	return 1;
}

void symbol_print(symtab *sym)
{

	int i;
	int t;
	for(i=0;i<sym->symencount;i++)
	{
		t=sym->symentry[i]->type;
		if(t==0)
			printf("%s int %d %s %d\n",sym->symentry[i]->symname,sym->symentry[i]->val.it,sym->symentry[i]->desc,sym->symentry[i]->lineno);
		else if(t==1)
			printf("%s float %f %s %d\n",sym->symentry[i]->symname,sym->symentry[i]->val.ft,sym->symentry[i]->desc,sym->symentry[i]->lineno);
		else if(t==2)
			printf("%s double %lf %s %d \n",sym->symentry[i]->symname,sym->symentry[i]->val.db,sym->symentry[i]->desc,sym->symentry[i]->lineno);
		else if(t==3)
			printf("%s char %c %s %d\n",sym->symentry[i]->symname,sym->symentry[i]->val.ch,sym->symentry[i]->desc,sym->symentry[i]->lineno);
		/*else if(t==-1)
			printf("%s %s\n",sym->symentry[i]->symname,sym->symentry[i]->desc);*/

	}

	for(i=0;i<sym->arrcount;i++)
	{
		int g,n,m;
		t=sym->arrentry[i]->type;
		g=sym->arrentry[i]->arraytype;
		m=sym->arrentry[i]->m;
		n=sym->arrentry[i]->n;
		int j,k;
		printf("Array %s\n",sym->arrentry[i]->symname);

		if(t==0)
			printf("Type is int,  Line no %d\n",sym->arrentry[i]->lineno);
		else if(t==1)
			 printf("Type is float,   Line no %d\n",sym->arrentry[i]->lineno);
		else if(t==2)
			 printf("Type is double,   Line no %d\n",sym->arrentry[i]->lineno);
		else if(t==3)
			printf("Type is char,   Line no %d\n",sym->arrentry[i]->lineno);
		if(g==0)
		{
			for(j=0;j<n;j++)
			{
				if(t==0)
					printf("%d ",sym->arrentry[i]->arrele->it[j]);
				else if(t==1)
					printf("%f ",sym->arrentry[i]->arrele->ft[j]);
				else if(t==2)
					printf("%lf ",sym->arrentry[i]->arrele->db[j]);
				else if(t==3)
					printf("%c ",sym->arrentry[i]->arrele->ch[j]);
			}
			printf("\n");

		}
		else if(g==1)
		{
			int ct=0;
			for(j=0;j<n*m;j++)
			{
				if(t==0)
					printf("%d ",sym->arrentry[i]->arrele->it[j]);
				else if(t==1)
					printf("%f ",sym->arrentry[i]->arrele->ft[j]);
				else if(t==2)
					printf("%lf ",sym->arrentry[i]->arrele->db[j]);
				else if(t==3)
					printf("%c ",sym->arrentry[i]->arrele->ch[j]);
				ct++;
				if(ct==n)
					{
						printf("\n");
						ct=0;
					}
			}
			printf("\n");

		}


	}
}

int main()
{

	head=(symtab **)malloc(sizeof(symtab *)*sym_t);



	if(!yyparse())
		;
	else
		printf("Invalid");


}
