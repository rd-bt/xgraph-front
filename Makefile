CC := gcc
CFLAG := -Wall -Ofast
LFLAG := -lc -lm xgraph/lib/xgraph.a
all: draw calc list
draw: main.c xgraph/lib/xgraph.a xgraph/header/xdraw.h xgraph common_symbols.c
	$(CC) $(CFLAG) $(LFLAG) main.c common_symbols.c -o draw
calc: calc.c xgraph/lib/xgraph.a xgraph/header/expr.h xgraph common_symbols.c
	$(CC) $(CFLAG) $(LFLAG) calc.c common_symbols.c -o calc
list: list.c xgraph/lib/xgraph.a xgraph/header/expr.h xgraph common_symbols.c
	$(CC) $(CFLAG) $(LFLAG) list.c common_symbols.c -o list
xgraph:
	make -C xgraph
xgraph/lib/xgraph.a:
	make -C xgraph
xgraph/header/xdraw.h:
	make -C xgraph
xgraph/header/expr.h:
	make -C xgraph
.PHONY:
clean:
	rm -f main calc list
	make -C xgraph cleanall
