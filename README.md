# xgraph-front
Front end of [xgraph](https://github.com/xbound/xgraph) library
Example:
```
./draw -o 1.bmp 'sin(t)'
```
draw a sin(t) graph to 1.bmp

```
./calc 'sum(n,1,100,1,n)'
```
output the sum from 1 to 100

```
./calc 'sum(n,1,10000,1,kill(n,15)==0)'
```
send signal 15 to process with pid from 1 to 10000 and output the number of successes


Run
```
git clone https://github.com/xbound/xgraph.git
```
or
```
./getxgraph
```
before build
