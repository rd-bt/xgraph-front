//#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "xgraph/header/expr.h"
#include <time.h>
//#include <err.h>
double dtime(double x){
	struct timespec ts;
	clock_gettime(CLOCK_REALTIME,&ts);
	return (double)ts.tv_sec+ts.tv_nsec/1000000000.0;
}

double draise(double x){
	return (double)raise((int)(x));
}
double dexit(double x){
	exit((int)(x));
}

double dkill(size_t n,double *args){
	return (double)kill((pid_t)(args[0]),(int)(args[1]));
}
void add_common_symbols(struct expr_symset *es){
	expr_symset_add(es,"time",EXPR_FUNCTION,dtime);
	expr_symset_add(es,"kill",EXPR_MDFUNCTION,dkill,2);
	expr_symset_add(es,"raise",EXPR_FUNCTION,draise);
	expr_symset_add(es,"exit",EXPR_FUNCTION,dexit);
	expr_symset_add(es,"abort",EXPR_FUNCTION,abort);
	expr_symset_add(es,"pid",EXPR_CONSTANT,(double)getpid());
	expr_symset_add(es,"uid",EXPR_CONSTANT,(double)getuid());
	expr_symset_add(es,"gid",EXPR_CONSTANT,(double)getgid());
}
