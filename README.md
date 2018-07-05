- Build erlangSDL lib on macOS (NIFs):
```sh
$ gcc -o erlangSDL.so -fpic -shared erlangSDL.c -I/usr/local/Cellar/erlang/20.3.1/lib/erlang/usr/include/ -flat_namespace -undefined suppress -I/Library/Frameworks/SDL2.framework/Versions/A/Headers -framework SDL2 -framework SDL2_image
```
- Build sdl_handler on macOS (Ports):
```sh
$ gcc -o sdl_ports sdl_handler.c -framework SDL2 -framework SDL2_image
```
- Build and run sdl_generator on macOS (with Rebar3):
```sh
$ cd ./sdl_generator
$ ./rebar3 escriptize
$ ./_build/default/bin/sdl_generator
```
- Build and run sdl_gametest on macOS (with Rebar3):
```sh
$ cd ./sdl_gametest
$ ./rebar3 escriptize
$ ./_build/default/bin/sdl_gametest
```
