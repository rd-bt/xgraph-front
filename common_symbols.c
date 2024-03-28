#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include "xgraph/header/expr.h"
#include <time.h>
#include <math.h>
#include "prime.c"
#include <float.h>
#include <fcntl.h>
#include <stdarg.h>
#include <string.h>
#include <signal.h>
#include <limits.h>
#include <sys/socket.h>
#include <netinet/in.h>
int vfdprintf_atomic(int fd,const char *restrict format,va_list ap){
	int r;
	char buf[PIPE_BUF];
	if((r=vsnprintf(buf,PIPE_BUF,format,ap))==EOF)return EOF;
	return write(fd,buf,r>PIPE_BUF?PIPE_BUF:r);
}
int fdprintf_atomic(int fd,const char *restrict format,...){
	int r;
	va_list ap;
	va_start(ap,format);
	r=vfdprintf_atomic(fd,format,ap);
	va_end(ap);
	return r;
}
int fprintd(int fd,double v){
	char buf[PIPE_BUF],*p;
	sprintf(buf,"%.64lf",v);
	p=strchr(buf,'.');
	if(p){
		p+=strlen(p);
		while(*(--p)=='0')*p=0;
		if(*p=='.')*p=0;

	}
	return fdprintf_atomic(fd,"%s\n",buf);
}
double dtime(void){
	struct timespec ts;
	clock_gettime(CLOCK_REALTIME,&ts);
	return (double)ts.tv_sec+ts.tv_nsec/1000000000.0;
}
double dsleep(double x){
	struct timespec rts,ts;
	double fx;
	x=fabs(x);
	fx=floor(x);
	ts.tv_sec=(time_t)fx;
	ts.tv_nsec=(time_t)((x-fx)*1000000000.0);
	memset(&rts,0,sizeof(struct timespec));
	nanosleep(&ts,&rts);
	return (double)rts.tv_sec+rts.tv_nsec/1000000000.0;
}

double draise(double x){
	return (double)raise((int)(x));
}
double dhtons(double x){
	return (double)htons((short)(x));
}
double dhtonl(double x){
	return (double)htonl((int)(x));
}
double dclose(double x){
	return (double)close((int)(x));
}
double dexit(double x){
	exit((int)(x));
}

double dkill(size_t n,double *args){
	return (double)kill((pid_t)(args[0]),(int)(args[1]));
}
#define a2s(buf,args,size) buf=alloca(size+1);\
	for(size_t i=0;i<size;++i)\
		buf[i]=(char)*(args++)
double dwrite(size_t n,double *args){
	char *buf;
	size_t size;
	int fd;
	if(n<2)return -1.0;
	size=n-1;
	fd=(int)*args;
	++args;
	a2s(buf,args,size);
	return (double)write(fd,buf,size);
}
double dopen(size_t n,double *args){
	char *buf;
	int flag;
	if(n<2)return -1.0;
	size_t size;
	size=n-1;
	a2s(buf,args,size);
	buf[size]=0;
	flag=(int)*args;
	return (double)open(buf,flag,0600);
}
double dconnect(size_t n,double *args){
//	char *buf;
	struct sockaddr_in sa;
//	size_t size;
	int fd;
//	if(n<2)return -1.0;
//	size=n-3;
	memset(&sa,0,sizeof(sa));
	fd=(int)*(args++);
//	sa.sin_family=(short)*(args++);
	sa.sin_family=AF_INET;
//	a2s(buf,args,size);
//	buf[size]=0;
//	sa.sin_addr.s_addr=inet_addr(buf);
	sa.sin_addr.s_addr=(in_addr_t)*(args++);
	sa.sin_port=(short)*(args++);
	return (double)connect(fd,(struct sockaddr *)&sa,sizeof(sa));
}
double dinet_addr(size_t n,double *args){
	switch(n){
		case 1:
			return (double)(uint32_t)*args;
		case 2:
			return (double)((uint32_t)args[0]+
				       ((uint32_t)args[1]<<16)
				       );
		case 3:
			return (double)((uint32_t)args[0]+
				       ((uint32_t)args[1]<<8)+
				       ((uint32_t)args[2]<<16)
				       );
		case 4:
			return (double)((uint32_t)args[0]+
				       ((uint32_t)args[1]<<8)+
				       ((uint32_t)args[2]<<16)+
				       ((uint32_t)args[3]<<24)
				       );
		default:
			return -1.0;
	}
}
double dtgkill(size_t n,double *args){
	return (double)tgkill((pid_t)(args[0]),(int)(args[1]),(int)(args[2]));
}
double dsocket(size_t n,double *args){
	return (double)socket((int)(args[0]),(int)(args[1]),(int)(args[2]));
}
int isprime(unsigned long n){
	if(n==2)return 1;
	if(!(n&1))return 0;
	unsigned long end=(unsigned long)(sqrt(n)+1.0);
	for(unsigned long i=3;i<end;i+=2)
		if(!(n%i))return 0;
	return 1;
}
double cal_prime(double x,unsigned long (*f)(unsigned long)){
	double fx,xmfx;
	if(x<=0.0)return 1.0;
	fx=floor(x);
	xmfx=x-fx;
	if(xmfx<=DBL_EPSILON)
		return (double)f((unsigned long)(x));
	return (1.0-xmfx)*(double)f((unsigned long)(fx))
		+xmfx*(double)f(1ul+(unsigned long)fx);
}
double dprime(double x){
	return cal_prime(x,prime);
}
double dprime_mt(double x){
	return cal_prime(x,prime_mt);
}
double dprime_old(double x){
	return cal_prime(x,prime_old);
}
double disprime(double x){
	return (double)isprime((unsigned long)(fabs(x)));
}
double dprint(double x){
	return (double)fprintd(STDOUT_FILENO,x);
}
double dfprint(size_t n,double *args){
	return (double)fprintd((int)args[0],args[1]);
}
volatile double vx[128];
void add_common_symbols(struct expr_symset *es){
	char buf[32];
	for(size_t i=0;i<(sizeof(vx)/sizeof(*vx));++i){
		sprintf(buf,"x%zu",i);
		expr_symset_add(es,buf,EXPR_VARIABLE,vx+i);
	}
	//puts("vx ok");
	expr_symset_add(es,"abort",EXPR_ZAFUNCTION,abort);
	expr_symset_add(es,"time",EXPR_ZAFUNCTION,dtime);
#define setfunc(c) expr_symset_add(es,#c,EXPR_FUNCTION,d##c)
	setfunc(close);
	setfunc(exit);
	setfunc(htonl);
	setfunc(htons);
	setfunc(isprime)->flag|=EXPR_SF_INJECTION;
	setfunc(prime)->flag|=EXPR_SF_INJECTION;
	setfunc(prime_mt)->flag|=EXPR_SF_INJECTION;
	setfunc(prime_old)->flag|=EXPR_SF_INJECTION;
	setfunc(print);
	setfunc(raise);
	setfunc(sleep);
#define setmd(c,dim) expr_symset_add(es,#c,EXPR_MDFUNCTION,d##c,(size_t)dim)
	setmd(connect,0);
	setmd(fprint,2);
	setmd(inet_addr,0);
	setmd(kill,2);
	setmd(socket,3);
	setmd(tgkill,3);
	setmd(write,0);
	setmd(open,0);
#define setconst(c) expr_symset_add(es,#c,EXPR_CONSTANT,(double)(c))
	setconst(AF_UNIX);
	setconst(AF_INET);
	setconst(AF_INET6);
	setconst(AF_PACKET);
	setconst(PF_UNIX);
	setconst(PF_INET);
	setconst(PF_INET6);
	setconst(PF_PACKET);
	setconst(SOCK_DGRAM);
	setconst(SOCK_RAW);
	setconst(SOCK_RDM);
	setconst(SOCK_SEQPACKET);
	setconst(SOCK_STREAM);
	setconst(IPPROTO_ICMP);
	setconst(IPPROTO_IP);
	setconst(IPPROTO_IGMP);
	setconst(IPPROTO_TCP);
	setconst(IPPROTO_UDP);
	setconst(O_APPEND);
	setconst(O_ASYNC);
	setconst(O_CLOEXEC);
	setconst(O_CREAT);
	//setconst(O_DIRECT);
	setconst(O_DIRECTORY);
	setconst(O_DSYNC);
	setconst(O_EXCL);
	//setconst(O_LARGEFILE);
	//setconst(O_NOATIME);
	setconst(O_NOCTTY);
	setconst(O_NDELAY);
	setconst(O_NOFOLLOW);
	setconst(O_NONBLOCK);
	//setconst(O_PATH);
	setconst(O_RDONLY);
	setconst(O_RDWR);
	setconst(O_SYNC);
	//setconst(O_TMPFILE);
	setconst(O_TRUNC);
	setconst(O_WRONLY);
	setconst(EXIT_FAILURE);
	setconst(EXIT_SUCCESS);
	setconst(STDIN_FILENO);
	setconst(STDOUT_FILENO);
	setconst(STDERR_FILENO);
	expr_symset_add(es,"pid",EXPR_CONSTANT,(double)getpid());
	expr_symset_add(es,"uid",EXPR_CONSTANT,(double)getuid());
	expr_symset_add(es,"gid",EXPR_CONSTANT,(double)getgid());

}
