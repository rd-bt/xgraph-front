#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "xgraph/header/expr.h"
#include <time.h>
#include <err.h>
void add_common_symbols(struct expr_symset *es);
double dtime(void);
struct expr_symset es[1];
int main(int argc,char **argv){
	char buf[32];
	double st;
	size_t i,l=0,count=argc>2?atol(argv[2]):1000,n=argc>1?atol(argv[1]):1000;
	init_expr_symset(es);
	add_common_symbols(es);
	puts("creating");
	for(i=0;i<=n;++i){

		sprintf(buf,"x%zu",i);
		expr_symset_add(es,buf,EXPR_CONSTANT,(double)i);
		if(i-l>=10000){
			printf("added x%zu\n",i);
			l=i;
		}
	}
	if(i!=l)printf("added x%zu\n",n);
	fputs("searching ",stdout);
	i=0;
	while(n){
		++i;
		n/=10;
	}
	fwrite("x91664684968694845678883",1,i,stdout);
	fputs("\n",stdout);
	st=dtime();

			printf("i %zu\n",i);
	while(--count)
	expr_symset_search(es,"x91664684968694845678883",i);
	expr_symset_search(es,"x91664684968694845678883",i)?
		puts("ok"):puts("fail");
	printf("searched for %lg s\n",dtime()-st);
	expr_symset_free(es);
	return 0;
}
