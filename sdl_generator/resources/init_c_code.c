#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdint.h>

#include <{{CLib}}>

#define BUF_SIZE 60000

typedef unsigned char byte;
typedef unsigned char string[BUF_SIZE];
typedef void (*handler)(byte *in, size_t len_in, byte *out, size_t *len_out);

typedef struct {
	int id;
	int values[10];
} ArrayA;

typedef struct {
	int id;
	int *values;
} ArrayB;

typedef struct {
	int id;
	int *values;
	int size;
} ArrayC;

int max (int *array, int size) {
	int max = -2147483648;
	
	for (int i=0; i<size; i++) {
		if (array[i]>max) max=array[i];
	}

	return max;
}