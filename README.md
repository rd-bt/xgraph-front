# xgraph-front
Front end of [xgraph](https://github.com/xbound/xgraph) library
Example:
```
./draw -o 1.bmp 'sin(t)'
```
draw a graph of sin(t) to 1.bmp

```
./calc 'sum(n,1,100,1,n)'
```
output the sum from 1 to 100

```
./calc 'sum(n,1,10000,1,kill(n,15)==0)'
```
send signal 15 to process with pid from 1 to 10000 and output the number of successes

compare with [zserge's expr](https://github.com/zserge/expr)
(By evaluating sqrt(4)+(0.75|1.5) for 10^8 times)

```
~/zserge_expr $ gcc e.c -o e -lm -Wall -O3
//a Syntax error will occur if -Ofast
~/zserge_expr $ time ./e 'sqrt(4)+(0.75|1.5)'
result: 3.000000

real    0m1.328s
user    0m1.318s
sys     0m0.006s
~/zserge_expr $ time ../xgraph-front/calc -n 100000000 'sqrt(4)+(0.75|1.5)'
3.75

real    0m0.980s
user    0m0.965s
sys     0m0.010s
```
e.c:
```
#include "expr.h"
static float mysqrt(struct expr_func *f, vec_expr_t *args, void *c) {
  float a = expr_eval(&vec_nth(args, 0));
  return sqrtf(a);
}
static struct expr_func user_funcs[] = {
    {"sqrt", mysqrt, NULL, 0},
    {NULL, NULL, NULL, 0},
};
int main(int c,char **v) {
  const char *s = c>1?v[1]:"5+2";
  struct expr_var_list vars = {0};
  struct expr *e = expr_create(s, strlen(s), &vars,user_funcs);
  int count=100000000;
  float result;
  if (e == NULL) {
    printf("Syntax error");
    return 1;
  }
redo:
  result = expr_eval(e);
	if(--count)goto redo;
  printf("result: %f\n", result);

  expr_destroy(e, &vars);
  return 0;
}

```
How to build

Run
```
git clone https://github.com/xbound/xgraph.git
```
or
```
./getxgraph
```
before build
