#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "xgraph/header/expr.h"
#include <time.h>
#include <err.h>
void add_common_symbols(struct expr_symset *es);
int main(int c,char **argv){
	if(c<2)
	errx(EXIT_FAILURE,"no expression input");
	struct expr_symset *es=new_expr_symset();
	//char buf[9999]={"sum(n,1,100,1,n)"};
	struct expr *ep;
	//double n=6.6;
	srand48(getpid()^time(NULL));
	//printf("%lu\n",(unsigned long)&n);
	//expr_symset_add(esp,"sb",&n,EXPR_PARAMETER);
	//printf("Input your expression:");
//	fgets(buf,9999,stdin);
	add_common_symbols(es);
	ep=new_expr(argv[1],"",es,&c);
	if(!ep){
		errx(EXIT_FAILURE,"expression error:%s",expr_error(c));
	}
	printf("%.64lf\n",expr_compute(ep,0));
	expr_free(ep);
	expr_symset_free(es);
	return 0;
}
