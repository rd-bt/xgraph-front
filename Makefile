CC := gcc
CFLAG := -Wall -Ofast
LFLAG := -lc -lm xgraph/lib/xgraph.a
all: draw calc list
draw: main.c xgraph/lib/xgraph.a xgraph/header/xdraw.h xgraph common_symbols.c
	$(CC) $(CFLAG) main.c common_symbols.c -o draw $(LFLAG)
calc: calc.c xgraph/lib/xgraph.a xgraph/header/expr.h xgraph common_symbols.c
	$(CC) $(CFLAG) calc.c common_symbols.c -o calc $(LFLAG)
list: list.c xgraph/lib/xgraph.a xgraph/header/expr.h xgraph common_symbols.c
	$(CC) $(CFLAG) list.c common_symbols.c -o list $(LFLAG)
xgraph:
	make -C xgraph
xgraph/expr.c:
	make -C xgraph
xgraph/xdraw.c:
	make -C xgraph
xgraph/lib/xgraph.a: xgraph/expr.c xgraph/xdraw.c
	make -C xgraph
xgraph/header/xdraw.h:
	make -C xgraph
xgraph/header/expr.h:
	make -C xgraph
.PHONY:
clean:
	rm -f draw calc list
	make -C xgraph cleanall
