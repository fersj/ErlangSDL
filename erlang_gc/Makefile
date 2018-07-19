
CFLAGS=-fpic -shared -I/usr/local/Cellar/erlang/20.3.1/lib/erlang/usr/include/ -flat_namespace -undefined suppress


all : gc_nif.so erlang_gc.beam

erlang_gc.beam : erlang_gc.erl
	erlc erlang_gc.erl

gc_nif.so : gc_nif.c
	gcc $(CFLAGS) -o gc_nif.so gc_nif.c
