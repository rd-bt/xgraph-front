ssize_t readall(int fd,void **bufp){
	char *buf,*p;
	size_t bufsiz,r1;
	ssize_t r,ret=0;
	static const size_t bufsize=1024;
	int i;
	bufsiz=bufsize;
	if((buf=malloc(bufsize))==NULL)return -errno;
	r1=0;
	while((r=read(fd,buf+ret,bufsize-r1))>0){
		r1+=r;
		ret+=r;
		if(ret==bufsiz){
			bufsiz+=bufsize;
			if((p=realloc(buf,bufsiz))==NULL){
				i=errno;
				free(buf);
				return -i;
			}
			buf=p;
			r1=0;
		}
	}
	if(ret==bufsiz){
	if((p=realloc(buf,bufsiz+1))==NULL){
		i=errno;
		free(buf);
		return -i;
	}
	buf=p;
	}
	buf[ret]=0;
	*bufp=buf;
	return ret;
}
