#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdint.h>

#include <{{CLib}}>

#define BUF_SIZE 60000

typedef unsigned char byte;
typedef unsigned char string[BUF_SIZE];
typedef void (*handler)(byte *in, size_t len_in, byte *out, size_t *len_out);

