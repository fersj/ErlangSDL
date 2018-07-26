#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdint.h>
#include <{{CLib}}>

#define BUF_SIZE 60000
typedef unsigned char byte;
typedef unsigned char string[BUF_SIZE];
typedef void (*handler)(byte *in, size_t len_in, byte *out, size_t *len_out);

#define CALL_CODE 10
#define RET_CODE 20

void write_command(byte *buf, size_t len);
void wait_for_input(byte *input_buffer, byte *output_buffer);