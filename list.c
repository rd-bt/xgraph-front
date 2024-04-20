#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "xgraph/header/expr.h"
#include <time.h>
#include <math.h>
const char *t2s[]={
	[EXPR_CONSTANT]="Constant",
	[EXPR_VARIABLE]="Variable",
	[EXPR_FUNCTION]="Function",
	[EXPR_MDFUNCTION]="Multi-dimension function",
	[EXPR_MDEPFUNCTION]="Multi-dimension function*",
	[EXPR_HOTFUNCTION]="Hot function",
	[EXPR_ZAFUNCTION]="Zero-argument function"
};
int main(int c,char **argv){
	srand48(time(NULL)+getpid());
	for(const struct expr_builtin_keyword *p=expr_keywords;p->str;++p){
		printf("%-12s\tKeyword \t%s(%s)\n",p->str,p->str,p->desc);
	}
	printf("\n");
	for(const struct expr_builtin_symbol *p=expr_bsyms;p->str;++p){
		printf("%-12s\t%-30s",p->str,t2s[p->type]);
		switch(p->type){
			case EXPR_CONSTANT:
				printf("value: %g",p->un.value);
				break;
			case EXPR_VARIABLE:
				printf("value: %g",*(double *)p->un.addr);
				break;
			case EXPR_FUNCTION:
				if(p->flag&EXPR_SF_INJECTION)
				printf("f(1.0):%.4g    f(e):%.4g",p->un.func(1),p->un.func(M_E));
				break;
			case EXPR_MDFUNCTION:
			case EXPR_MDEPFUNCTION:
				if(p->dim)
				printf("dimension: %hd",p->dim);
				else
				printf("dimension: no limit");
				break;
			case EXPR_ZAFUNCTION:
				//printf("f():%.4g",p->un.zafunc());
				break;
			default:
				abort();

		}
		printf("\n");
	}
	return 0;
}
