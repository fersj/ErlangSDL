#include <erl_nif.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

const char *MEMORY_MANAGER_NAME = "memory_manager";

/*
 * Estructura que almacena los datos necesarios para la liberación de un puntero.
 */
typedef struct {
  char *free_module_name;       // Nombre del módulo que contiene la función destructora
  char *free_function_name;     // Nombre de la función destructora
  uintptr_t wrapped_ptr;        // Puntero a destruir cuando le toque recolección de basura
} pointer_wrapper;

/*
 * Tipo Erlang asociado a pointer_wrapper. Sirve para asignar la función
 * pointer_wrapper_destructor como función a ejecutar cuando un pointer_wrapper sea
 * recogido por el GC de Erlang.
 */
ErlNifResourceType *wrapper_type;


/*
 * Función destructora de un pointer_wrapper. Se limita a enviar un mensaje al
 * proceso gestor de la recolección de basura (memory_handler) para que llame a la función de 
 * liberación del puntero `envuelto`
 */
void pointer_wrapper_destructor(ErlNifEnv* env, void* obj) {
  pointer_wrapper *pw = (pointer_wrapper *) obj;
  
  ErlNifEnv *dst_env = enif_alloc_env();
  
  // Construimos la tupla { free, ModuleName, FunName, Pointer }
  ERL_NIF_TERM result = enif_make_tuple4(dst_env,
    enif_make_atom(dst_env, "free"),
    enif_make_atom(dst_env, pw->free_module_name),
    enif_make_atom(dst_env, pw->free_function_name),
    enif_make_long(dst_env, pw->wrapped_ptr)
  );
  
  // ...y se la enviamos al proceso que esté registrado como memory manager
  ErlNifPid memory_handler_pid;
  
  if (enif_whereis_pid(env, enif_make_atom(env, MEMORY_MANAGER_NAME), &memory_handler_pid)) {
    enif_send(env, &memory_handler_pid, dst_env, result);
  }
  
}

/*
 * Función llamara al cargar el módulo NIF.
 */
int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {

  // Creamos el tipo "pointer_wrapper" asociándole la función destructora creada
  // anteriormente.
  wrapper_type = enif_open_resource_type(
    env,
    "erlang_gc",
    "pointer_wrapper",
    pointer_wrapper_destructor, 
    ERL_NIF_RT_CREATE,
    NULL
  );
  return 0;
}


/*
 * Esta función recibe un puntero "crudo" (como void *), junto con el módulo y la
 * función destructora, y devuelve un pointer_wrapper gestionado automáticamente por el gestor
 * de memoria de Erlang
 */
static ERL_NIF_TERM manage_ptr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  int module_atom_length, function_atom_length;
  
  // Obtenemos las longitudes de las cadenas con los nombres del módulo y de
  // la función para reservar los buffers correspondientes.
  if (!enif_get_atom_length(env, argv[0], &module_atom_length, ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  }
  
  if (!enif_get_atom_length(env, argv[1], &function_atom_length, ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  }
  
  // Reservamos dos buffers para que quepan sendas cadenas más sus carácteres nulos
  char *module_atom = malloc(module_atom_length + 1);
  char *function_atom = malloc(function_atom_length + 1);
  
  // Copiamos los átomos en los buffers
  enif_get_atom(env, argv[0], module_atom, module_atom_length + 1, ERL_NIF_LATIN1);
  enif_get_atom(env, argv[1], function_atom, function_atom_length + 1, ERL_NIF_LATIN1);

  
  // Obtenemos el tercer parámetro: el puntero a gestionar.
  unsigned long long_ptr;
  
  if (!enif_get_ulong(env, argv[2], &long_ptr)) {
    return enif_make_badarg(env);
  }
  
  uintptr_t ptr = (uintptr_t) long_ptr;
  
  // Creamos e inicializamos la estructura pointer_wrapper con la información pasada
  // como parámetro.
  pointer_wrapper *pw = enif_alloc_resource(wrapper_type, sizeof(pointer_wrapper));
  
  pw->free_module_name = module_atom;
  pw->free_function_name = function_atom;
  pw->wrapped_ptr = ptr;
  
  
  // Construimos la tupla resultado: { managed_pointer, WrappedPointer }
  ERL_NIF_TERM managed_pointer_atom = enif_make_atom(env, "managed_pointer");
  ERL_NIF_TERM wrapper_ptr = enif_make_resource(env, pw);
  
  ERL_NIF_TERM result = enif_make_tuple2(env, managed_pointer_atom, wrapper_ptr);
  
  // Liberamos la referencia al WrappedPointer para que quede a cargo del 
  // recolector de basura de Erlang.
  enif_release_resource(pw);
  
  return result;
}


static ERL_NIF_TERM get_wrapped_pointer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  const ERL_NIF_TERM *tuple;
  int arity;
  
  // Obtenemos la tupla como primer parámetro
  if (!enif_get_tuple(env, argv[0], &arity, &tuple)) {
    enif_make_badarg(env);
  }
  
  // Si no es de la forma { Atom, Number }, devolvemos bad_arg
  if (arity != 2 || !enif_is_atom(env, tuple[0]) || !enif_is_ref(env, tuple[1])) {
    enif_make_badarg(env);
  }
  
  // Si la primera componente no es el átomo managed_pointer, devolvemos bad_arg
  int tag_length;
  enif_get_atom_length(env, tuple[0], &tag_length, ERL_NIF_LATIN1);
  
  char tag[tag_length + 1];
  enif_get_atom(env, tuple[0], tag, tag_length + 1, ERL_NIF_LATIN1);
  
  if (strcmp(tag, "managed_pointer") != 0) {
    enif_make_badarg(env);
  }
  
  // Obtenemos el wrapped_pointer en la segunda componente de la tupla  
  void *wp_void;
  if (!enif_get_resource(env, tuple[1], wrapper_type, &wp_void)) {
    enif_make_badarg(env);
  }
    
  pointer_wrapper *wp = (pointer_wrapper *) wp_void;
  
  // Extraemos el puntero "crudo" como un long
  long pointer = (long) wp->wrapped_ptr;
  
  return enif_make_long(env, pointer);
}

ErlNifFunc funcs[] = {
  { "manage_ptr", 3, manage_ptr, 0 },
  { "get_wrapped_pointer", 1, get_wrapped_pointer, 0 },
};

ERL_NIF_INIT(erlang_gc, funcs, load, NULL, NULL, NULL)


