#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include "xgraph/header/expr.h"
#include <time.h>
#include <math.h>
#include <float.h>
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
int isprime(unsigned long n){
	if(n==2)return 1;
	if(!(n&1))return 0;
	unsigned long end=(unsigned long)(sqrt(n)+1.0);
	for(unsigned long i=3;i<end;i+=2)
		if(!(n%i))return 0;
	return 1;
}
unsigned long prime(unsigned long n){
	if(!n)return 1;
	if(!(--n))return 2;
	for(int i=3;;i+=2){
		if(isprime(i)&&!(--n))return i;
	}
}
double dprime(double x){
	return (double)prime((unsigned long)(fabs(x)+DBL_EPSILON));
}
double disprime(double x){
	return (double)isprime((unsigned long)(fabs(x)+DBL_EPSILON));
}
void add_common_symbols(struct expr_symset *es){
	expr_symset_add(es,"time",EXPR_FUNCTION,dtime);
	expr_symset_add(es,"prime",EXPR_FUNCTION,dprime);
	expr_symset_add(es,"isprime",EXPR_FUNCTION,disprime);
	expr_symset_add(es,"kill",EXPR_MDFUNCTION,dkill,2);
	expr_symset_add(es,"raise",EXPR_FUNCTION,draise);
	expr_symset_add(es,"exit",EXPR_FUNCTION,dexit);
	expr_symset_add(es,"abort",EXPR_FUNCTION,abort);
	expr_symset_add(es,"pid",EXPR_CONSTANT,(double)getpid());
	expr_symset_add(es,"uid",EXPR_CONSTANT,(double)getuid());
	expr_symset_add(es,"gid",EXPR_CONSTANT,(double)getgid());
}
