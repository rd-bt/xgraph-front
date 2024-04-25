struct dnode {
	struct dnode *lt,*gt;
	size_t eq;
	double val;
};
static void expr_sort3_write(double **dst,const struct dnode *dnp){
	size_t eq;
	if(dnp->lt)
		expr_sort3_write(dst,dnp->lt);
	eq=dnp->eq;
	do
		*((*dst)++)=dnp->val;
	while(--eq);
	if(dnp->gt)
		expr_sort3_write(dst,dnp->gt);
}
__attribute__((noinline))
void *expr_sort3(double *v,size_t n,void *(*allocator)(size_t)){
	struct dnode *top,*dnp;
	double *restrict p;
	double *endp=v+n;
	if(allocator){
		dnp=allocator(n*sizeof(struct dnode));
		if(!dnp)return NULL;
		expr_fry(v,n);
	}else {
		expr_fry(v,n);
		dnp=alloca(n*sizeof(struct dnode));
	}
	top=dnp;
	p=v;
	goto create;
	for(;p<endp;++p){
		for(struct dnode *p1=dnp;;){
			if(*p==p1->val){
				++p1->eq;
				break;
			}else if((*p>p1->val)){
				if(p1->gt){
					p1=p1->gt;
					continue;
				}
				p1->gt=++top;
				goto create;
			}else {
				if(p1->lt){
					p1=p1->lt;
					continue;
				}
				p1->lt=++top;
				goto create;
			}
		}
		continue;
create:
		top->lt=NULL;
		top->gt=NULL;
		top->eq=1;
		top->val=*p;
	}
	expr_sort3_write(&v,dnp);
	return dnp;//return for free
}
void expr_sort_old(double *restrict v,size_t n){
	double swapbuf;
	for(size_t i=0;i<n;++i){
		for(size_t j=i+1;j<n;++j){
			if(v[j]<v[i]){
				swapbuf=v[i];
				v[i]=v[j];
				v[j]=swapbuf;
			}
		}
	}
}
