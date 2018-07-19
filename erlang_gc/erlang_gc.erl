%
% Módulo genérico para la gestión automática de punteros C en Erlang
%
% Proporciona una función `manage_ptr` que convierte un puntero "crudo" en C
% en un puntero `wrapped`, que es automáticamente liberado por el recolector
% de basura de Erlang.
%

-module(erlang_gc).

-export([init/0, manage_ptr/3, get_wrapped_pointer/1]).

-on_load(init/0).

-type wrapped_pointer() :: { wrapped_pointer, reference() }.
-type raw_pointer() :: integer().

%
% Función de inicialización
% -------------------------
% Se encarga de crear el proceso memory_manager, que es el que llamará a las
% funciones destructoras de los punteros
%

init() -> 
  erlang:load_nif("./gc_nif", 0),
  MemManager = spawn(fun() -> memory_manager_loop() end),
  register(memory_manager, MemManager),
  io:format("Memory manager initialized~n"),
  ok.

  
%
% Función manage_ptr/3
% -------------------------
% Recibe un nombre de módulo, un nombre de función y un puntero crudo, y devuelve
% un puntero "envuelto", que será liberado automáticamente por el GC de Erlang.
%

-spec manage_ptr(atom(), atom(), raw_pointer()) -> wrapped_pointer().
manage_ptr(_, _, _) -> { error, not_loaded }.


%
% Función get_wrapped_pointer/1
% -----------------------------
% Obtiene el puntero "crudo" a partir del puntero envuelto
%

-spec get_wrapped_pointer(wrapped_pointer()) -> raw_pointer().
get_wrapped_pointer(_) -> { error, not_loaded }.

%
% Proceso gestor de memoria. Llama a la función de liberación
% cuando la función NIF destructora le envía un mensaje.
%

memory_manager_loop() ->
  receive
    { free, ModuleName, FunctionName, Ptr } -> 
      ModuleName:FunctionName({raw_pointer, Ptr}),
      memory_manager_loop()
  end.
