
CFLAGS=`sdl2-config --cflags`
LDFLAGS=`sdl2-config --libs`

all: test_ports sdl_handler sdl_ports.beam

test_ports.o: test_ports.c

sdl_handler.o: sdl_handler.c


test_ports: test_ports.o

sdl_handler: sdl_handler.o

sdl_ports.beam: sdl_ports.erl
	erlc sdl_ports.erl