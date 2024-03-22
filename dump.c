#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "xgraph/header/expr.h"
#include <time.h>
#include <err.h>
void add_common_symbols(struct expr_symset *es);
void list(struct expr *ep,int lv){
	char *sop,ssrc[64];
	for(struct expr_inst *ip=ep->data;ip-ep->data<ep->size;++ip){
		*ssrc=0;
		switch(ip->op){
			case EXPR_COPY:
				sop="copy";
				break;
			case EXPR_CONST:
				sop="const";
				sprintf(ssrc,"%g",ip->un.value);
				break;
			case EXPR_INPUT:
					sop="input";
					strcpy(ssrc," ");
					break;
			case EXPR_CALL:sop="call";break;
			case EXPR_ADD:sop="add";break;
			case EXPR_SUB:sop="sub";break;
			case EXPR_MUL:sop="mul";break;
			case EXPR_DIV:sop="div";break;
			case EXPR_MOD:sop="mod";break;
			case EXPR_POW:sop="pow";break;
			case EXPR_AND:sop="and";break;
			case EXPR_OR:sop="or";break;
			case EXPR_XOR:sop="xor";break;
			case EXPR_SHL:sop="shl";break;
			case EXPR_SHR:sop="shr";break;
			case EXPR_NEG:
					sop="neg";
					strcpy(ssrc," ");
					break;
			case EXPR_IF:sop="if";break;
			case EXPR_WHILE:sop="while";break;
			case EXPR_SUM:sop="sum";break;
			case EXPR_INT:sop="int";break;
			case EXPR_PROD:sop="prod";break;
			case EXPR_SUP:sop="sup";break;
			case EXPR_INF:sop="inf";break;
			case EXPR_ANDN:sop="andn";break;
			case EXPR_ORN:sop="orn";break;
			case EXPR_XORN:sop="xorn";break;
			case EXPR_GCDN:sop="gcdn";break;
			case EXPR_LCMN:sop="lcmn";break;
			case EXPR_LOOP:sop="loop";break;
			case EXPR_FOR:sop="for";break;
			case EXPR_CALLMD:sop="callmd";break;
			case EXPR_CALLMDEP:sop="callmdep";break;
			case EXPR_CALLHOT:sop="callhot";break;
			case EXPR_GT:sop="gt";break;
			case EXPR_GE:sop="ge";break;
			case EXPR_LT:sop="lt";break;
			case EXPR_LE:sop="le";break;
			case EXPR_EQ:sop="eq";break;
			case EXPR_NE:sop="ne";break;
			case EXPR_ANDL:sop="andl";break;
			case EXPR_ORL:sop="orl";break;
			case EXPR_XORL:sop="xorl";break;
			//case EXPR_ASSIGN:sop="assign";break;
			case EXPR_END:
					sop="end";
					strcpy(ssrc," ");
					break;
		}
		if(!*ssrc){
			if(ip->un.src>=ep->vars&&ip->un.src<ep->vars+ep->vsize){
				sprintf(ssrc,"vars[%zd]=%g",ip->un.src-ep->vars,*ip->un.src);
			}else {
				sprintf(ssrc,"%p",ip->un.src);
			}
		}
		if(ip->dst>=ep->vars&&ip->dst<ep->vars+ep->vsize)
			printf("%-12s\tvars[%zd]=%g\t%s\n",sop,ip->dst-ep->vars,*ip->dst,ssrc);
		else
			printf("%-12s\t%p\t%s\n",sop,ip->dst,ssrc);
	}
}
int main(int argc,char **argv){
	char *buf,*p,*p1;
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
	if(init_expr(ep,argv[argc-1],"t",es)<0){
		errx(EXIT_FAILURE,"expression error:%s (%s)",expr_error(ep->error),ep->errinfo);
	}
	list(ep,0);
	expr_free(ep);
	expr_symset_free(es);
	return 0;
}
