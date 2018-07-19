
CFLAGS=-fpic -shared -I/usr/local/Cellar/erlang/20.3.1/lib/erlang/usr/include/ -flat_namespace -undefined suppress
CFRAMEWORKS=-framework SDL2

all : gc_nif.so erlang_gc.beam sdl_ports_gen.beam sdl_ports_gen

erlang_gc.beam : erlang_gc.erl
	erlc erlang_gc.erl

gc_nif.so : gc_nif.c
	gcc $(CFLAGS) -o gc_nif.so gc_nif.c

gc_nif.so_w : gc_nif.c
	gcc $(CFLAGS) -o gc_nif.so gc_nif.c -w

sdl_ports_gen.beam : sdl_ports_gen.erl
	erlc sdl_ports_gen.erl

sdl_ports_gen : sdl_ports_gen.c
	gcc -o sdl_ports_gen sdl_ports_gen.c $(CFRAMEWORKS)

sdl_ports_gen_w : sdl_ports_gen.c
	gcc -o sdl_ports_gen sdl_ports_gen.c $(CFRAMEWORKS) -w

clean :
	rm ./*.beam
	rm ./*.so
	rm ./sdl_ports_gen

allw : gc_nif.so_w erlang_gc.beam sdl_ports_gen.beam sdl_ports_gen_w