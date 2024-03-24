#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include "xgraph/header/expr.h"
#include <time.h>
#include <err.h>
void add_common_symbols(struct expr_symset *es);
char prefix[1024]={0};
void level_inc(void){
	strcat(prefix,"----");
}
void level_dec(void){
	memset(prefix+strlen(prefix)-4,0,4);
}
int xprintf(const char *fmt,...){
	va_list ap;
	int r;
	va_start(ap,fmt);
	r=printf("%s",prefix);
	r+=vprintf(fmt,ap);
	va_end(ap);
	return r;
}
int addr2sym(const struct expr *restrict ep,char buf[EXPR_SYMLEN],void *addr){
	union {
		const struct expr_symbol *es;
		const struct expr_builtin_symbol *ebs;
	} sym;
	sym.es=NULL;
	if(ep->sset)sym.es=expr_symset_rsearch(ep->sset,addr);
	if(sym.es){
		strcpy(buf,sym.es->str);
		return 0;
	}
	sym.ebs=expr_bsym_rsearch(addr);
	if(sym.ebs){
		strcpy(buf,sym.ebs->str);
		return 0;
	}
	return -1;
}
void list(const struct expr *restrict ep){
	char *sop=NULL,ssrc[EXPR_SYMLEN],sdst[EXPR_SYMLEN];
	for(struct expr_inst *ip=ep->data;ip-ep->data<ep->size;++ip){
		*ssrc=0;
		*sdst=0;
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
			case EXPR_IF:sop="if";goto branch;
			case EXPR_WHILE:sop="while";goto branch;
			case EXPR_SUM:sop="sum";goto sum;
			case EXPR_INT:sop="int";goto sum;
			case EXPR_PROD:sop="prod";goto sum;
			case EXPR_SUP:sop="sup";goto sum;
			case EXPR_INF:sop="inf";goto sum;
			case EXPR_ANDN:sop="andn";goto sum;
			case EXPR_ORN:sop="orn";goto sum;
			case EXPR_XORN:sop="xorn";goto sum;
			case EXPR_GCDN:sop="gcdn";goto sum;
			case EXPR_LCMN:sop="lcmn";goto sum;
			case EXPR_LOOP:sop="loop";goto sum;
			case EXPR_FOR:sop="for";goto sum;
			case EXPR_CALLMD:sop="callmd";goto md;
			case EXPR_CALLMDEP:sop="callmdep";goto md;
			case EXPR_CALLHOT:
					sop="callhot";
					level_inc();
					xprintf("hot function %p\n",ip->un.hotfunc);
					list(ip->un.hotfunc);
					level_dec();
					break;
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
sum:
					level_inc();
					xprintf("struct expr_suminfo %p index:%p\n",ip->un.es,&ip->un.es->index);
					xprintf("%p->from\n",ip->un.es);
					list(ip->un.es->from);
					xprintf("%p->to\n",ip->un.es);
					list(ip->un.es->to);
					xprintf("%p->step\n",ip->un.es);
					list(ip->un.es->step);
					xprintf("%p->ep\n",ip->un.es);
					list(ip->un.es->ep);
					level_dec();
					break;
branch:
					level_inc();
					xprintf("struct expr_branchinfo %p\n",ip->un.eb);
					xprintf("%p->cond\n",ip->un.eb);
					list(ip->un.eb->cond);
					xprintf("%p->body\n",ip->un.eb);
					list(ip->un.eb->body);
					xprintf("%p->value\n",ip->un.eb);
					list(ip->un.eb->value);
					level_dec();
					break;
md:
					level_inc();
					xprintf("struct expr_mdinfo %p\n",ip->un.em);
					for(size_t i=0;i<ip->un.em->dim;++i){
					xprintf("dimension %zu\n",i);
					list(ip->un.em->eps+i);
					}
					level_dec();
					break;
		}
		if(!sop)abort();
		if(!*ssrc){
			if(ip->un.src>=ep->vars&&ip->un.src<ep->vars+ep->vsize){
				sprintf(ssrc,"vars[%zd]=%g",ip->un.src-ep->vars,*ip->un.src);
			}else if(addr2sym(ep,ssrc,ip->un.src)<0){
				sprintf(ssrc,"%p",ip->un.src);
			}
		}
		if(!*sdst){
			if(ip->dst>=ep->vars&&ip->dst<ep->vars+ep->vsize)
				sprintf(sdst,"vars[%zd]=%g",ip->dst-ep->vars,*ip->dst);
			else if(addr2sym(ep,sdst,ip->dst)<0)
				sprintf(sdst,"%p",ip->dst);
		}
		xprintf("%-9s%s\t%s\n",sop,sdst,ssrc);
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
	list(ep);
	expr_free(ep);
	expr_symset_free(es);
	return 0;
}
