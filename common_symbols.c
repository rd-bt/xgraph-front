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
#define casting(x,T) ((T)(x))
#define cast(x,T) expr_cast(x,T)
/*_Generic(T,\
		void *:expr_cast(x,T),\
		double:(x),\
		default:(T)(x)\
		)
		,void *:expr_cast(x,double)\*/
#define castingd(x) ((double)(x))
//_Generic((x),void *:(expr_cast(x,double)),default:((double)(x)))
#define warp1(rtype,sym,atype) double d##sym(double x){\
	rtype r=(rtype)sym(casting(x,atype));\
	return castingd(r);\
}
#define warp2(rtype,sym,at0,at1) double d##sym(size_t n,double *v){\
	rtype r=(rtype)sym(casting(v[0],at0),casting(v[1],at1));\
	return castingd(r);\
}
#define warppip(rtype,sym,at0,at1) double d##sym(size_t n,double *v){\
	rtype r=(rtype)sym(casting(v[0],at0),cast(v[1],at1));\
	return expr_cast(r,double);\
}
#define warp3(rtype,sym,at0,at1,at2) double d##sym(size_t n,double *v){\
	rtype r=(rtype)sym(casting(v[0],at0),casting(v[1],at1),casting(v[2],at2));\
	return castingd(r);\
}
#define warpiipi(rtype,sym,at0,at1,at2) double d##sym(size_t n,double *v){\
	rtype r=(rtype)sym(casting(v[0],at0),cast(v[1],at1),casting(v[2],at2));\
	return castingd(r);\
}
#define warpiipp(rtype,sym,at0,at1,at2) double d##sym(size_t n,double *v){\
	rtype r=(rtype)sym(casting(v[0],at0),cast(v[1],at1),cast(v[2],at2));\
	return castingd(r);\
}
#define warpz(rtype,sym) double d##sym(void){\
	rtype r=(rtype)sym();\
	return castingd(r);\
}
warp1(int,close,int)
warp1(int,raise,int)
warp2(int,kill,pid_t,int)
warp2(int,listen,int,int)
warppip(void *,signal,int,void *)
warp3(int,socket,int,int,int)
int tgkill(int,int,int);
warp3(int,tgkill,int,int,int)
warpiipi(int,bind,int,struct sockaddr *,socklen_t)
warpiipp(int,accept,int,struct sockaddr *,socklen_t *)
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
int fprintda(int fd,double *v,size_t n){
	int r;
	if(!n)return 0;
	r=0;
	do
		r+=fprintd(fd,*(v++));
	while(--n);
	return r;
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
double dhtons(double x){
	return (double)htons((short)(x));
}
double dhtonl(double x){
	return (double)htonl((int)(x));
}

#define a2s(buf,args,size) buf=alloca(size+1);\
	for(size_t i=0;i<size;++i)\
		buf[i]=(char)*(args++)
double dwrite3(size_t n,double *args){
	size_t size;
	union {
		void *buf;
		double dr;
	} un;
	int fd;
	fd=(int)*args;
	un.dr=*(++args);
	size=(size_t)*(++args);
	return write(fd,un.buf,size);
}
double dread3(size_t n,double *args){
	size_t size;
	union {
		void *buf;
		double dr;
	} un;
	int fd;
	fd=(int)*args;
	un.dr=*(++args);
	size=(size_t)*(++args);
	return read(fd,un.buf,size);
}
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
double dsizeof(size_t n,double *args){
	return (double)(n*sizeof(double));
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

double dputs(double x){
	/*union {
		double d;
		char *r;
	} un;
	un.d=x;*/
	return (double)puts(expr_cast(x,char *));
}
double dfprint(size_t n,double *args){
	return (double)fprintd((int)args[0],args[1]);
}
double dprinta(size_t n,double *args){
	return (double)fprintda(STDOUT_FILENO,args,n);
}
double dsorta_old(size_t n,double *args){
	expr_sort_old(args,n);
	return (double)fprintda(STDOUT_FILENO,args,n);
}
double dsorta(size_t n,double *args){
	void *r=expr_sort3(args,n,malloc);
	if(!r)return NAN;
	free(r);
	return (double)fprintda(STDOUT_FILENO,args,n);
}
double dfrya(size_t n,double *args){
	expr_fry(args,n);
	return (double)fprintda(STDOUT_FILENO,args,n);
}
double dfprinta(size_t n,double *args){
	return (double)fprintda((int)args[0],args+1,n-1);
}
volatile double vx[128];
void add_common_symbols(struct expr_symset *es){
	char buf[32];
	for(size_t i=0;i<(sizeof(vx)/sizeof(*vx));++i){
		sprintf(buf,"x%zu",i);
		expr_symset_add(es,buf,EXPR_VARIABLE,vx+i);
	}
	//puts("vx ok");
	expr_symset_add(es,"time",EXPR_ZAFUNCTION,dtime);
#define setfunc0(c) expr_symset_add(es,#c,EXPR_FUNCTION,c)
#define setfunc(c) expr_symset_add(es,#c,EXPR_FUNCTION,d##c)
	setfunc(close);
	setfunc(htonl)->flag|=EXPR_SF_INJECTION;
	setfunc(htons)->flag|=EXPR_SF_INJECTION;
	setfunc(isprime)->flag|=EXPR_SF_INJECTION;
	setfunc(prime)->flag|=EXPR_SF_INJECTION;
	setfunc(prime_mt)->flag|=EXPR_SF_INJECTION;
	setfunc(prime_old)->flag|=EXPR_SF_INJECTION;
	setfunc(print);
	setfunc(puts);
	setfunc(raise);
	setfunc(sleep);
#define setmd(c,dim) expr_symset_add(es,#c,EXPR_MDFUNCTION,d##c,(size_t)dim)
	setmd(accept,3);
	setmd(bind,3);
	setmd(connect,0);
	setmd(fprint,2);
	setmd(fprinta,0);
	setmd(frya,0);
	setmd(inet_addr,0);
	setmd(kill,2);
	setmd(listen,2);
	setmd(printa,0);
	setmd(sizeof,0)->flag|=EXPR_SF_INJECTION;
	setmd(read3,3);
	setmd(signal,2);
	setmd(sorta,0);
	setmd(sorta_old,0);
	setmd(socket,3);
	setmd(tgkill,3);
	setmd(write,0);
	setmd(write3,3);
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
	setconst(SIGHUP);
	setconst(SIGINT);
	setconst(SIGQUIT);
	setconst(SIGILL);
	setconst(SIGTRAP);
	setconst(SIGABRT);
	setconst(SIGBUS);
	setconst(SIGFPE);
	setconst(SIGKILL);
	setconst(SIGUSR1);
	setconst(SIGSEGV);
	setconst(SIGUSR2);
	setconst(SIGPIPE);
	setconst(SIGALRM);
	setconst(SIGTERM);
	setconst(SIGSTKFLT);
	setconst(SIGCHLD);
	setconst(SIGCONT);
	setconst(SIGSTOP);
	setconst(SIGTSTP);
	setconst(SIGTTIN);
	setconst(SIGTTOU);
	setconst(SIGURG);
	setconst(SIGXCPU);
	setconst(SIGXFSZ);
	setconst(SIGVTALRM);
	setconst(SIGPROF);
	setconst(SIGWINCH);
	setconst(SIGIO);
	setconst(SIGPWR);
	setconst(SIGSYS);
	setconst(SIGRTMIN);
	setconst(SIGRTMAX);
	setconst(STDIN_FILENO);
	setconst(STDOUT_FILENO);
	setconst(STDERR_FILENO);
	expr_symset_add(es,"pid",EXPR_CONSTANT,(double)getpid());
	expr_symset_add(es,"uid",EXPR_CONSTANT,(double)getuid());
	expr_symset_add(es,"gid",EXPR_CONSTANT,(double)getgid());

}
