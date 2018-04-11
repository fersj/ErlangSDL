- Build on macOS:
```sh
$ gcc -o erlangSDL.so -fpic -shared erlangSDL.c -I/usr/local/Cellar/erlang/20.3.1/lib/erlang/usr/include/ -flat_namespace -undefined suppress -I/Library/Frameworks/SDL2.framework/Versions/A/Headers -framework SDL2 -framework SDL2_image
```
