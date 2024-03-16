#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "xgraph/header/expr.h"
#include <time.h>
#include <err.h>
void add_common_symbols(struct expr_symset *es);
int main(int argc,char **argv){
	char *buf,*p,*p1;
	int count=1;
	double result;
	if(argc<2)
	errx(EXIT_FAILURE,"no expression input");
	struct expr_symset *es=new_expr_symset();
	//char buf[9999]={"sum(n,1,100,1,n)"};
	struct expr ep[1];
	//double n=6.6;
	srand48(getpid()^time(NULL));
	//printf("%lu\n",(unsigned long)&n);
	//expr_symset_add(esp,"sb",&n,EXPR_PARAMETER);
	//printf("Input your expression:");
//	fgets(buf,9999,stdin);
	for(int i=1;i<argc-1;++i){
		if(!strcmp(argv[i],"-f")&&i<argc-3){
			p1=argv[++i];
			if((p=strchr(argv[++i],':'))){
				buf=argv[i];
				*(p++)=0;
			}else {
				buf="t";
				p=argv[i];
			}
			expr_symset_add(es,p1,EXPR_HOTFUNCTION,p,buf);
		}
	}
	add_common_symbols(es);
	if(init_expr(ep,argv[argc-1],"",es)<0){
		errx(EXIT_FAILURE,"expression error:%s (%s)",expr_error(ep->error),ep->errinfo);
	}
redo:
	result=expr_compute(ep,0);
	if(--count)goto redo;
	//asprintf(&buf,"%.64lf",result);
	asprintf(&buf,"%.64lf",result);
	p=strchr(buf,'.');
	if(p){
		p+=strlen(p);
		while(*(--p)=='0')*p=0;
		if(*p=='.')*p=0;

	}
	puts(buf);
	free(buf);
	expr_free(ep);
	expr_symset_free(es);
	return 0;
}
