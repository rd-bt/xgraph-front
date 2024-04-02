CC := gcc
CFLAG := -Wall -Ofast
LFLAG := xgraph/lib/xgraph.a -lc -lm
all: draw calc list dump symtest sorttest
draw: main.c xgraph/lib/xgraph.a xgraph/header/xdraw.h xgraph common_symbols.c
	$(CC) $(CFLAG) main.c common_symbols.c -o draw $(LFLAG)
calc: calc.c xgraph/lib/xgraph.a xgraph/header/expr.h xgraph common_symbols.c
	$(CC) $(CFLAG) calc.c common_symbols.c -o calc $(LFLAG)
list: list.c xgraph/lib/xgraph.a xgraph/header/expr.h xgraph common_symbols.c
	$(CC) $(CFLAG) list.c common_symbols.c -o list $(LFLAG)
dump: dump.c xgraph/lib/xgraph.a xgraph/header/expr.h xgraph common_symbols.c
	$(CC) $(CFLAG) dump.c common_symbols.c -o dump $(LFLAG)
symtest: symtest.c xgraph/lib/xgraph.a xgraph/header/expr.h xgraph common_symbols.c
	$(CC) $(CFLAG) symtest.c common_symbols.c -o symtest $(LFLAG)
sorttest: sorttest.c xgraph/lib/xgraph.a xgraph/header/expr.h xgraph common_symbols.c
	$(CC) $(CFLAG) sorttest.c common_symbols.c -o sorttest $(LFLAG)
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
	rm -f draw calc list dump symtest sorttest
	make -C xgraph cleanall
