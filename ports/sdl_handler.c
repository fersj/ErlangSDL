#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include <SDL2/SDL.h>
#include <SDL2_image/SDL_image.h>

#define BUF_SIZE 60000
#define MAX_WINDOWS 100
#define MAX_SURFACES 100


typedef unsigned char byte;

byte string_buffer[BUF_SIZE];

FILE *log_file;


// En este array voy a ir almacenando todos los SDL_Window que se generen
// mediante SDL_CreateWindow.
//
// num_windows denota el índice con la primera posición vacía del array
SDL_Window *windows[MAX_WINDOWS]; 
unsigned int num_windows = 0;


// En este array voy a ir almacenando todos los SDL_Surface que se generen
// mediante SDL_LoadBMP o mediante SDL_GetWindowSurface. Este último caso
// es problemático, ya que si llamamos, por ejemplo, veinte veces a la función
// SDL_GetWindowSurface, se van a ocupar veinte posiciónes de este array, cuando
// todas ellas apuntan al mismo SDL_Surface.
//
// num_surfaces denota el índice con la primera posición vacía del array
SDL_Surface *surfaces[MAX_SURFACES];
unsigned int num_surfaces = 0;


// Función de depuración - Compilar con -DDEBUG para que se escriban las funciones
// llamadas a un fichero
#ifdef DEBUG
void log_port(char *format, ...) {
  va_list args;
  
  va_start(args, format);
  vfprintf(log_file, format, args);
  fflush(log_file);
  va_end(args);
}
#endif

// A partir de un puntero (in) al buffer donde se almacena el comando recibido, lee cuatro bytes
// y construye un entero de 32 bits a partir de ellos. Devuelve el puntero `in` incrementado
// en cuatro bytes, con el fin de poder ir encadenando operaciones de la siguiente forma:

// puntero_in = read_in(puntero_in, &entero1);
// puntero_in = read_in(puntero_in, &entero2);
// puntero_in = read_in(puntero_in, &entero3);
// ...
byte *read_int(byte *in, int *result) {
  byte *current_in = in;
  
  *result = (*current_in++ << 24) | (*current_in++ << 16) | (*current_in++ << 8) | *current_in++;
  
  return current_in;  
}


// Igual que antes, pero con un Uint32 de SDL
byte *read_Uint32(byte *in, Uint32 *result) {
  byte *current_in = in;
  
  *result = (*current_in++ << 24) | (*current_in++ << 16) | (*current_in++ << 8) | *current_in++;
  
  return current_in;  
}

// Igual que antes, pero con un byte. En este caso, el puntero solo se incrementa
// en una posición
byte *read_byte(byte *in, byte *result) {
  byte *current_in = in;
  
  *result = *current_in++;
  
  return current_in;  
}


// Lee una cadena desde la posición apuntada por el puntero `in`. Los primeros cuatro
// bytes obtenidos del buffer de entrada indican la longitud de la cadena. La cadena
// leida se almacena en una variable global (string_buffer). Además, se devuelve el
// puntero a la posición del buffer de entrada siguiente al fin de la cadena.
byte *read_string(byte *in) {
  byte *current_in = in;
  byte *current_out = string_buffer;
  int len;
  
  // Obtenemos la longitud
  current_in = read_int(current_in, &len);
  
  // Copiamos desde el buffer de entrada a la variable string_buffer
  for (int i = 0; i < len; i++) {
    *current_out = *current_in;
    current_out++;
    current_in++;
  }
  // Finalizamos la variable string_buffer con el carácter de fin de cadena.
  *current_out = '\0'; *current_out++;
  
  return current_in;
}

// Escribe el número `number` en el buffer de bytes a partir del puntero indicado
// en `out`. El parámetro `len` es de entrada/salida. La función write_int incrementará
// este parámetro según el número de bytes escritos.
//
// Devuelve el puntero a la siguiente posición vacía del buffer recibido (apuntando justo
// detrás del elemento escrito).
byte *write_int(int number, byte *out, size_t *len) {
  byte *current_out = out;
  
  *current_out++ = number >> 24; (*len)++;
  *current_out++ = (number >> 16) & 255; (*len)++;
  *current_out++ = (number >> 8) & 255; (*len)++;
  *current_out++ = number & 255; (*len)++;
  
  return current_out;
}

byte *write_Uint32(Uint32 number, byte *out, size_t *len) {
    byte *current_out = out;
    
    *current_out++ = number >> 24; (*len)++;
    *current_out++ = (number >> 16) & 255; (*len)++;
    *current_out++ = (number >> 8) & 255; (*len)++;
    *current_out++ = number & 255; (*len)++;
    
    return current_out;
}

byte *write_string(char *string, byte *out, size_t *len) {
    byte *current_out = out;
    
    int strsize = (int) strlen(string);
    current_out = write_int(strsize, current_out, len);
    
    for (long i=0; i<strsize; i++) {
        *current_out++ = string[i]; (*len)++;
    }
    *current_out++ = '\0'; (*len)++;
    
    return current_out;
}

// =====================================================================
//    MANEJADORES DE LAS FUNCIONES DE SDL
// =====================================================================

// Todos los manejadores tienen la misma interfaz:
//
//  in: Puntero al buffer de entrada, que contiene el opcode de la función
//      a ejecutar, seguido de los parámetros
//  len_in: Longitud del buffer de entrada (no sé si es necesario)
//  out: Puntero al buffer de salida, donde deben escribirse los resultados
//      de la función
//  len_out: Variable de salida, con el número de bytes escritos en *out
typedef void (*handler)(byte *in, size_t len_in, byte *out, size_t *len_out);




// Función SDL_Init(Flags)

void SDL_Init_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
  byte *current_in = in, *current_out = out;
  *len_out = 0;
  
  // Siempre tenemos que incrementar para saltarse el opcode
  *current_in++;
  
  
  Uint32 flags;
  // Leemos el primer parámetro
  current_in = read_Uint32(current_in, &flags);
    
#ifdef DEBUG
  log_port("SDL_Init(%d)\n", flags);
#endif  
  // Llamamos a la función
  int result = SDL_Init(flags);
  
  // Escribimos el resultado en el buffer de salida
  current_out = write_int(result, current_out, len_out);
}


// Función SDL_Quit(). Esta es la más fácil :-)


void SDL_Quit_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {

#ifdef DEBUG
  log_port("SDL_Quit()\n");
#endif  
  
  *len_out = 0;
}


// Función SDL_CreateWindow(title, x, y, w, h, flags). Esta es la más difícil :-)
void SDL_CreateWindow_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
  byte *current_in = in, *current_out = out;
  *len_out = 0;
  
  // Siempre tenemos que incrementar para saltarse el opcode
  *current_in++;
  
  int x, y, w, h;
  Uint32 flags;
  
  // Leemos parámetros
  current_in = read_string(current_in);
  current_in = read_int(current_in, &x);
  current_in = read_int(current_in, &y);
  current_in = read_int(current_in, &w);
  current_in = read_int(current_in, &h);
  current_in = read_Uint32(current_in, &flags);
  
#ifdef DEBUG
  log_port("SDL_CreateWindow(%s, %d, %d, %d, %d, %d)\n", string_buffer, x, y, w, h, flags);
#endif
  // Llamamos a la función
  SDL_Window *window = SDL_CreateWindow(string_buffer, x, y, w, h, flags);
  
  // Añadimos el SDL_Window creado a nuestro array de ventanas
  // y devolvemos el índice dentro de dicho array.
  int window_num = num_windows++;
  windows[window_num] = window;
  
  
  current_out = write_int(window_num, current_out, len_out);
}

// Función SDL_GetWindowSurface(SDL_Window *).
void SDL_GetWindowSurface_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
  byte *current_in = in, *current_out = out;
  *len_out = 0;
  
  // Siempre tenemos que incrementar para saltarse el opcode
  *current_in++;

  int window_number;
  
  // Leemos el parámetro, que es la posición de la ventana
  // dentro del array windows.
  current_in = read_int(current_in, &window_number);
  
  SDL_Window *window = windows[window_number];
#ifdef DEBUG
  log_port("SDL_GetWindowSurface(%p)\n", window);
#endif  
  // Llamamos a SDL_GetWindowSurface
  SDL_Surface *s = SDL_GetWindowSurface(window);
  
  // Añadimos el surface devuelto al array surfaces, y devolvemos
  // la posición asignada
  int surface_num = num_surfaces++;
  surfaces[surface_num] = s;
  
  current_out = write_int(surface_num, current_out, len_out);
}

// Función SDL_LoadBMP
void SDL_LoadBMP_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
  byte *current_in = in, *current_out = out;
  *len_out = 0;
  
  // Siempre tenemos que incrementar para saltarse el opcode
  *current_in++;
  
  current_in = read_string(current_in);
  
#ifdef DEBUG
  log_port("SDL_LoadBMP(%s)\n", string_buffer);
#endif  
  SDL_Surface *s = SDL_LoadBMP(string_buffer);
  
  int surface_num = num_surfaces++;
  surfaces[surface_num] = s;
  
  current_out = write_int(surface_num, current_out, len_out);
  
}  

// Función SDL_BlitSurface(SDL_Surface *, SDL_Rect *, SDL_Surface *, SDL_Rect *)
void SDL_BlitSurface_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
  byte *current_in = in, *current_out = out;
  *len_out = 0;
  // Siempre tenemos que incrementar para saltarse el opcode
  *current_in++;
  
  // Aquí tenemos una dificultad especial: los punteros SDL_Rect * pasados como
  // segundo y cuarto parámetro pueden ser valores NULL. Estos punteros se serializan
  // de la siguiente forma:
  //
  //  - Puntero NULL: lista [0]
  //  - Puntero a SDL_Rect: lista [1, ...x..., ...y..., ...w..., ...h...]
  
  
  int srcSurfaceCode;
  // tagSrcRect sirve para distinguir si el segundo parámetro es nulo (0) o no (1).
  byte tagSrcRect;
  SDL_Rect srcRect;
  
  
  int dstSurfaceCode;
  byte tagDstRect;
  SDL_Rect dstRect;
  
  current_in = read_int(current_in, &srcSurfaceCode);
  current_in = read_byte(current_in, &tagSrcRect);
  
  // Si tagSrcRect es distinto de cero, leemos los cuatro enteros
  // que vienen detras
  if (tagSrcRect) {
    current_in = read_int(current_in, &(srcRect.x));
    current_in = read_int(current_in, &(srcRect.y));
    current_in = read_int(current_in, &(srcRect.w));
    current_in = read_int(current_in, &(srcRect.h));
  }

  current_in = read_int(current_in, &dstSurfaceCode);
  current_in = read_byte(current_in, &tagDstRect);
  
  // Igualmente para el rectángulo de salida
  if (tagDstRect) {
    current_in = read_int(current_in, &(dstRect.x));
    current_in = read_int(current_in, &(dstRect.y));
    current_in = read_int(current_in, &(dstRect.w));
    current_in = read_int(current_in, &(dstRect.h));    
  }
  
  SDL_Surface *srcSurface = surfaces[srcSurfaceCode];
  SDL_Surface *dstSurface = surfaces[dstSurfaceCode];
  
#ifdef DEBUG  
  log_port("SDL_BlitSurface(%d", srcSurfaceCode);
  if (tagSrcRect) {
    log_port(", {%d, %d, %d, %d}", srcRect.x, srcRect.y, srcRect.w, srcRect.h);
  } else {
    log_port(", NULL");
  }
  log_port(", %d", dstSurfaceCode);
  if (tagDstRect) {
    log_port(", {%d, %d, %d, %d}", dstRect.x, dstRect.y, dstRect.w, dstRect.h);
  } else {
    log_port(", NULL");
  }
  log_port(")\n");
#endif

  // Llamamos, por fin, a la función.
  int result = SDL_BlitSurface(srcSurface, 
                               tagSrcRect ? &srcRect : NULL,
                               dstSurface,
                               tagDstRect ? &dstRect : NULL);
  
  // Y escribimos el resultado
  current_out = write_int(result, current_out, len_out);
  
}

// Manejador para SDL_UpdateWindowSurface(SDL_Window *)
void SDL_UpdateWindowSurface_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
  byte *current_in = in, *current_out = out;
  *len_out = 0;
  
  // Siempre tenemos que incrementar para saltarse el opcode
  *current_in++;
  
  int windowCode;
  current_in = read_int(current_in, &windowCode);
  
  SDL_Window *window = windows[windowCode];
#ifdef DEBUG
  log_port("SDL_UpdateWindowSurface(%d)\n", windowCode);
#endif
  int result = SDL_UpdateWindowSurface(window);
  
  current_out = write_int(result, current_out, len_out);
}

// Manejador para SDL_DestroyWindow(SDL_Window *)
void SDL_DestroyWindow_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
  byte *current_in = in;
  
  // We skip the opcode
  *current_in++;
  
  int windowCode;
  current_in = read_int(current_in, &windowCode);
  
  SDL_Window *window = windows[windowCode];
#ifdef DEBUG
  log_port("SDL_DestroyWindow(%d)\n", windowCode);
#endif  
  SDL_DestroyWindow(window);
  
  *len_out = 0;
}

void SDL_GetWindowSize_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
    byte *current_in = in, *current_out = out;
    *len_out = 0;
    current_in++;
    
    int window_number;
    
    // Leemos el parámetro, que es la posición de la ventana
    // dentro del array windows.
    current_in = read_int(current_in, &window_number);
    
    SDL_Window *window = windows[window_number];
#ifdef DEBUG
    log_port("SDL_GetWindowSize(%p)\n", window);
#endif
    // Llamamos a SDL_GetWindowSize
    int w, h;
    SDL_GetWindowSize(window, &w, &h);
    
    current_out = write_int(w, current_out, len_out);
    current_out = write_int(h, current_out, len_out);
}

void SDL_FreeSurface_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
    byte *current_in = in;
    *len_out = 0;
    current_in++;
    
    int surface_code;
    current_in = read_int(current_in, &surface_code);
    SDL_Surface *surface = surfaces[surface_code];
    
    SDL_FreeSurface(surface);
    surfaces[surface_code] = NULL;
}

// Función SDL_BlitScaled(SDL_Surface *, SDL_Rect *, SDL_Surface *, SDL_Rect *)
void SDL_BlitScaled_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
    byte *current_in = in, *current_out = out;
    *len_out = 0;
    // Siempre tenemos que incrementar para saltarse el opcode
    *current_in++;
    
    // Aquí tenemos una dificultad especial: los punteros SDL_Rect * pasados como
    // segundo y cuarto parámetro pueden ser valores NULL. Estos punteros se serializan
    // de la siguiente forma:
    //
    //  - Puntero NULL: lista [0]
    //  - Puntero a SDL_Rect: lista [1, ...x..., ...y..., ...w..., ...h...]
    
    int srcSurfaceCode;
    // tagSrcRect sirve para distinguir si el segundo parámetro es nulo (0) o no (1).
    byte tagSrcRect;
    SDL_Rect srcRect;
    
    int dstSurfaceCode;
    byte tagDstRect;
    SDL_Rect dstRect;
    
    current_in = read_int(current_in, &srcSurfaceCode);
    current_in = read_byte(current_in, &tagSrcRect);
    
    // Si tagSrcRect es distinto de cero, leemos los cuatro enteros
    // que vienen detras
    if (tagSrcRect) {
        current_in = read_int(current_in, &(srcRect.x));
        current_in = read_int(current_in, &(srcRect.y));
        current_in = read_int(current_in, &(srcRect.w));
        current_in = read_int(current_in, &(srcRect.h));
    }
    
    current_in = read_int(current_in, &dstSurfaceCode);
    current_in = read_byte(current_in, &tagDstRect);
    
    // Igualmente para el rectángulo de salida
    if (tagDstRect) {
        current_in = read_int(current_in, &(dstRect.x));
        current_in = read_int(current_in, &(dstRect.y));
        current_in = read_int(current_in, &(dstRect.w));
        current_in = read_int(current_in, &(dstRect.h));
    }
    
    SDL_Surface *srcSurface = surfaces[srcSurfaceCode];
    SDL_Surface *dstSurface = surfaces[dstSurfaceCode];
    
#ifdef DEBUG
    log_port("SDL_BlitScaled(%d", srcSurfaceCode);
    if (tagSrcRect) {
        log_port(", {%d, %d, %d, %d}", srcRect.x, srcRect.y, srcRect.w, srcRect.h);
    } else {
        log_port(", NULL");
    }
    log_port(", %d", dstSurfaceCode);
    if (tagDstRect) {
        log_port(", {%d, %d, %d, %d}", dstRect.x, dstRect.y, dstRect.w, dstRect.h);
    } else {
        log_port(", NULL");
    }
    log_port(")\n");
#endif
    
    // Llamamos, por fin, a la función.
    int result = SDL_BlitScaled(srcSurface,
                                 tagSrcRect ? &srcRect : NULL,
                                 dstSurface,
                                 tagDstRect ? &dstRect : NULL);
    
    // Y escribimos el resultado
    current_out = write_int(result, current_out, len_out);
}

void SDL_GetSurfaceSize_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
    byte *current_in = in, *current_out = out;
    *len_out = 0;
    current_in++;
    
    int surface_code;
    
    // Leemos el parámetro, que es la posición de la ventana
    // dentro del array windows.
    current_in = read_int(current_in, &surface_code);
    
    SDL_Surface *surface = surfaces[surface_code];
#ifdef DEBUG
    log_port("SDL_GetSurfaceSize(%p)\n", surface);
#endif
    // Llamamos a SDL_GetWindowSize

    current_out = write_int(surface->w, current_out, len_out);
    current_out = write_int(surface->h, current_out, len_out);
}

void SDL_GetError_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
    byte *current_out = out;
    *len_out = 0;

    char *error = (char *) SDL_GetError();
    current_out = write_string(error, current_out, len_out);
}

void SDL_PollEvent_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
    byte *current_out = out;
    *len_out = 0;
    
    SDL_Event event;
    int result = SDL_PollEvent(&event);

    // Devolvemos [result, event.type, event.key.keysym.sym (SDL_KEYDOWN), event.key.repeat(SDL_KEYDOWN)]
    current_out = write_int(result, current_out, len_out);
    current_out = write_Uint32(event.type, current_out, len_out);
    if (event.type == SDL_KEYDOWN || event.type == SDL_KEYUP) {
        current_out = write_int(event.key.keysym.sym, current_out, len_out);
        current_out = write_int(event.key.repeat, current_out, len_out);
    } else {
        current_out = write_int(0, current_out, len_out);
        current_out = write_int(0, current_out, len_out);
    }
}

// Función IMG_Load
void IMG_Load_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
    byte *current_in = in, *current_out = out;
    *len_out = 0;
    
    // Siempre tenemos que incrementar para saltarse el opcode
    *current_in++;
    
    current_in = read_string(current_in);
    
#ifdef DEBUG
    log_port("SDL_LoadBMP(%s)\n", string_buffer);
#endif
    SDL_Surface *s = IMG_Load(string_buffer);
    
    int surface_num = num_surfaces++;
    surfaces[surface_num] = s;
    
    current_out = write_int(surface_num, current_out, len_out);
}

void IMG_GetError_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
    byte *current_out = out;
    *len_out = 0;
    
    char *error = (char *) IMG_GetError();
    write_string(error, current_out, len_out);
}

// Array con todos los manejadores implementados.
// 
// Están indexados según el opcode. Por ejemplo, el opcode 3
// corresponde a SDL_CreateWindow_Handler.
//
// La posición 0 está reservada para indicar el cierre de la
// aplicación.

handler handlers[] = {
    0,
    SDL_Init_Handler,                // 1
    SDL_Quit_Handler,                // 2
    SDL_CreateWindow_Handler,        // 3
    SDL_GetWindowSurface_Handler,    // 4
    SDL_LoadBMP_Handler,             // 5
    SDL_BlitSurface_Handler,         // 6
    SDL_UpdateWindowSurface_Handler, // 7
    SDL_DestroyWindow_Handler,       // 8
    SDL_GetWindowSize_Handler,       // 9
    SDL_FreeSurface_Handler,         // 10
    SDL_BlitScaled_Handler,          // 11
    SDL_GetSurfaceSize_Handler,      // 12
    SDL_GetError_Handler,            // 13
    SDL_PollEvent_Handler,           // 14
    IMG_Load_Handler,                // 15
    IMG_GetError_Handler             // 16
};


// Lee un comando de la entrada estándar y guarda su contenido
// en el buffer pasado como parámetro

int read_command(byte *buf) {
  int len;

  // Los mensajes enviados mediante Erlang vienen en forma de paquetes
  // Cada paquete viene precedido por 2 bytes, que indican su longitud.
  
  // Leemos esta longitud
  if (fread(buf, 2, 1, stdin) != 1) {
    return -1;
  }
  len = (buf[0] << 8) | buf[1];
  
#ifdef DEBUG  
  log_port("Received package of length %d:", len);
#endif  

  // A continuación, leemos el resto del mensaje (payload).
  if (len >= BUF_SIZE) {
    return -1;
  } else {
    int numRead = fread(buf, len, 1, stdin);
    
#ifdef DEBUG    
    log_port(" [");
    for (int i = 0; i < len; i++) {
      log_port("%d", buf[i]);
      if (i != len - 1) log_port(",");
    }
    log_port("]\n");
#endif    

    return (numRead == 1) ? len : -1;
  }
}

void write_command(byte *buf, size_t len) {
  // Al enviar mensajes a Erlang, debemos preceder el mensaje
  // por su longitud (2 bytes)
  byte len_h = (len >> 8) & 255;
  byte len_v = len & 255;

  byte arr[] = { len_h, len_v };
  
#ifdef DEBUG
  log_port("Output: [%d, %d", len_h, len_v);
  for (int i = 0; i < len; i++) {
    log_port(", %d", buf[i]);
  }
  log_port("]\n");
#endif  
  
  // Escribimos la longitud
  fwrite(arr, 2, 1, stdout);
  // Escribimos el buffer
  fwrite(buf, len, 1, stdout);
  fflush(stdout);
}

int main() {
#ifdef DEBUG  
  log_file = fopen("log.txt", "a");
  log_port("Started.\n");
#endif  
  
  // Buffer con los mensajes recibidos de Erlang,
  // y los mensajes enviados hacia Erlang
  byte input_buffer[BUF_SIZE], output_buffer[BUF_SIZE];
  
  
  // Leemos el primer comando recibido desde Erlang
  int len_in = read_command(input_buffer);
  size_t len_out;
  
  // Mientras el opcode sea distinto de cero, o no haya habido 
  // error o EOF en la lectura
  while (len_in > 0 && input_buffer[0] != 0) {
    // Obtenemos la función manejadora, en función del opcode.    
    handler current_handler = handlers[input_buffer[0]];
    
    // Llamamos a la función manejadora
    (*current_handler)(input_buffer, len_in, output_buffer, &len_out);
    
    // Escribimos el buffer de salida escrito por la función manejadora
    // en la salida estándar
    write_command(output_buffer, len_out);
    
    // Leemos el siguiente comando
    len_in = read_command(input_buffer);
  }
  
#ifdef DEBUG  
  log_port("Finished.\n");
#endif  
  
  fclose(log_file);
}



