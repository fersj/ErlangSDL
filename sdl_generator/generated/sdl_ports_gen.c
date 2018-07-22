#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdint.h>
#include <SDL2/SDL.h>

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

//--------------------------------------------------------

byte * read_pointer(byte *in, void **result);
byte * write_pointer(void **pointer, byte *out, size_t *len);

// ---- Int ----

byte * read_int8(byte *in, int8_t *result) {
	byte *current_in = in;

	*result = *current_in++;

	return current_in;
}

byte * read_int16(byte *in, int16_t *result) {
	byte *current_in = in;

	// *result = (((int16_t)*current_in++) << 8) | ((int16_t)*current_in++);
	*result = (((int16_t)*current_in++) << 8);
	*result = *result | ((int16_t)*current_in++);

	return current_in;
}

byte * read_int32(byte *in, int *result) {
	byte *current_in = in;

	// *result = (((int)*current_in++) << 24) | (((int)*current_in++) << 16) |
	// 					(((int)*current_in++) << 8) | ((int)*current_in++);
	*result = (((int)*current_in++) << 24);
	*result = *result | (((int)*current_in++) << 16);
	*result = *result | (((int)*current_in++) << 8);
	*result = *result | ((int)*current_in++);

	return current_in;
}

byte * read_int64(byte *in, int64_t *result) {
	byte *current_in = in;

	current_in = in;
	// *result = (((int64_t)*current_in++) << 56) | (((int64_t)*current_in++) << 48) |
	// 			(((int64_t)*current_in++) << 40) | (((int64_t)*current_in++) << 32) |
	// 			(((int64_t)*current_in++) << 24) | (((int64_t)*current_in++) << 16) |
	// 			(((int64_t)*current_in++) << 8) | ((int64_t)*current_in++);
	*result = (((int64_t)*current_in++) << 56);
	*result = *result | (((int64_t)*current_in++) << 48);
	*result = *result | (((int64_t)*current_in++) << 40);
	*result = *result | (((int64_t)*current_in++) << 32);
	*result = *result | (((int64_t)*current_in++) << 24);
	*result = *result | (((int64_t)*current_in++) << 16);
	*result = *result | (((int64_t)*current_in++) << 8);
	*result = *result | ((int64_t)*current_in++);
	
	return current_in;
}

byte * read_int(byte *in, int *result) {
	return read_int32(in, result);
}

byte * write_int8(int8_t *number, byte *out, size_t *len) {
	byte *current_out = out;

	*current_out++ = *number; (*len)++;

	return current_out;
}

byte * write_int16(int16_t *number, byte *out, size_t *len) {
	byte *current_out = out;

	*current_out++ = *number >> 8; (*len)++;
	*current_out++ = *number & 255; (*len)++;

	return current_out;
}

byte * write_int32(int *number, byte *out, size_t *len) {
	byte *current_out = out;

	*current_out++ = *number >> 24; (*len)++;
	*current_out++ = (*number >> 16) & 255; (*len)++;
	*current_out++ = (*number >> 8) & 255; (*len)++;
	*current_out++ = *number & 255; (*len)++;

	return current_out;
}

byte * write_int64(int64_t *number, byte *out, size_t *len) {
	byte *current_out = out;

	*current_out++ = *number >> 56; (*len)++;
	*current_out++ = (*number >> 48) & 255; (*len)++;
	*current_out++ = (*number >> 40) & 255; (*len)++;
	*current_out++ = (*number >> 32) & 255; (*len)++;
	*current_out++ = (*number >> 24) & 255; (*len)++;
	*current_out++ = (*number >> 16) & 255; (*len)++;
	*current_out++ = (*number >> 8) & 255; (*len)++;
	*current_out++ = *number & 255; (*len)++;

	return current_out;
}

byte * write_int(int *number, byte *current_out, size_t *len) {
	return write_int32(number, current_out, len);
}

byte * read_int8_array(byte *in, int8_t *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_int8(current_in, &array[i]);

	return current_in;
}

byte * write_int8_array(int8_t *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_int8(&array[i], current_out, len);

	return current_out;
}

byte * read_int16_array(byte *in, int16_t *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_int16(current_in, &array[i]);

	return current_in;
}

byte * write_int16_array(int16_t *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_int16(&array[i], current_out, len);

	return current_out;
}

byte * read_int32_array(byte *in, int *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_int32(current_in, &array[i]);

	return current_in;
}

byte * write_int32_array(int *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_int32(&array[i], current_out, len);

	return current_out;
}

byte * read_int64_array(byte *in, int64_t *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_int64(current_in, &array[i]);

	return current_in;
}

byte * write_int64_array(int64_t *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_int64(&array[i], current_out, len);

	return current_out;
}

byte * read_int_array(byte *in, int *array, int n) {
	return read_int32_array(in, array, n);
}

byte * write_int_array(int *array, byte *out, size_t *len, int n) {
	return write_int32_array(array, out, len, n);
}

void pointer_deref_int8_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int8_t *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int8(ptr, current_out, len_out);
}

void pointer_deref_int8_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int8_t *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_int8(&(ptr[index]), current_out, len_out);
}

void pointer_deref_int8_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int8_t *ptr, value;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int8(current_in, &value);
	*ptr = value;
}

void pointer_deref_int8_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int8_t *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_int8(current_in, &value);
	ptr[index] = value;
}

void new_int8_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int8_t *ptr = malloc(sizeof(int8_t));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_int8_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	int8_t *ptr = malloc(sizeof(int8_t)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_int8_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int8_t *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void pointer_deref_int16_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int16_t *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int16(ptr, current_out, len_out);
}

void pointer_deref_int16_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int16_t *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_int16(&(ptr[index]), current_out, len_out);
}

void pointer_deref_int16_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int16_t *ptr, value;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int16(current_in, &value);
	*ptr = value;
}

void pointer_deref_int16_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int16_t *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_int16(current_in, &value);
	ptr[index] = value;
}

void new_int16_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int16_t *ptr = malloc(sizeof(int16_t));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_int16_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	int16_t *ptr = malloc(sizeof(int16_t)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_int16_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int16_t *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void pointer_deref_int32_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int32(ptr, current_out, len_out);
}

void pointer_deref_int32_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int *ptr, index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_int32(&(ptr[index]), current_out, len_out);
}

void pointer_deref_int32_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int *ptr, value;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int32(current_in, &value);
	*ptr = value;
}

void pointer_deref_int32_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int *ptr, index, value;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_int32(current_in, &value);
	ptr[index] = value;
}

void new_int32_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int *ptr = malloc(sizeof(int));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_int32_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	int *ptr = malloc(sizeof(int)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_int32_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void pointer_deref_int64_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int64_t *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int64(ptr, current_out, len_out);
}

void pointer_deref_int64_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int64_t *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_int64(&(ptr[index]), current_out, len_out);
}

void pointer_deref_int64_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int64_t *ptr, value;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int64(current_in, &value);
	*ptr = value;
}

void pointer_deref_int64_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int64_t *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_int64(current_in, &value);
	ptr[index] = value;
}

void new_int64_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int64_t *ptr = malloc(sizeof(int64_t));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_int64_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	int64_t *ptr = malloc(sizeof(int64_t)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_int64_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int64_t *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void pointer_deref_int_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	pointer_deref_int32_Handler(in, len_in, out, len_out);
}

void pointer_deref_int_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	pointer_deref_int32_array_Handler(in, len_in, out, len_out);
}

void pointer_deref_int_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	pointer_deref_int32_assign_Handler(in, len_in, out, len_out);
}

void pointer_deref_int_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	pointer_deref_int32_array_assign_Handler(in, len_in, out, len_out);
}

void new_int_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	new_int32_Handler(in, len_in, out, len_out);
}

void new_int_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	new_int32_array_Handler(in, len_in, out, len_out);
}

void delete_int_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	delete_int32_Handler(in, len_in, out, len_out);
}

// ---- Float ----

byte * read_float(byte *in, float *result) {
	byte *current_in = in;
	union {
		float number;
		unsigned char arr[4];
	} aux_float;

	for (int i=3; i>=0; i--)
		aux_float.arr[i] = *current_in++;

	*result = aux_float.number;

	return current_in;
}

byte * write_float(float *number, byte *out, size_t *len) {
	byte *current_out = out;
	union {
		float number;
		unsigned char arr[4];
	} aux_float;

	aux_float.number = *number;

	for (int i=3; i>=0; i--)
		*current_out++ = aux_float.arr[i];

	return current_out;
}

byte * read_float_array(byte *in, float *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_float(current_in, &array[i]);

	return current_in;
}

byte * write_float_array(float *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_float(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_float_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	float *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_float(ptr, current_out, len_out);
}

void pointer_deref_float_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	float *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_float(&(ptr[index]), current_out, len_out);
}

void pointer_deref_float_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	float *ptr, value;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_float(current_in, &value);
	*ptr = value;
}

void pointer_deref_float_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	float *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_float(current_in, &value);
	ptr[index] = value;
}

void new_float_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	float *ptr = malloc(sizeof(float));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_float_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	float *ptr = malloc(sizeof(float)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_float_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	float *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

// ---- Double ----

byte * read_double(byte *in, double *result) {
	byte *current_in = in;
	union {
		double number;
		unsigned char arr[8];
	} aux_double;

	for (int i=7; i>=0; i--)
		aux_double.arr[i] = *current_in++;

	*result = aux_double.number;

	return current_in;
}

byte * write_double(double *number, byte *out, size_t *len) {
	byte *current_out = out;
	union {
		double number;
		unsigned char arr[8];
	} aux_double;

	aux_double.number = *number;

	for (int i=7; i>=0; i--)
		*current_out++ = aux_double.arr[i];

	return current_out;
}

byte * read_double_array(byte *in, double *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_double(current_in, &array[i]);

	return current_in;
}

byte * write_double_array(double *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_double(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_double_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	double *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_double(ptr, current_out, len_out);
}

void pointer_deref_double_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	double *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_double(&(ptr[index]), current_out, len_out);
}

void pointer_deref_double_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	double *ptr, value;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_double(current_in, &value);
	*ptr = value;
}

void pointer_deref_double_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	double *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_double(current_in, &value);
	ptr[index] = value;
}

void new_double_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	double *ptr = malloc(sizeof(double));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_double_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	double *ptr = malloc(sizeof(double)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_double_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	double *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

// ---- String ----

byte * read_string(byte *in, string *result) {
	byte *current_in = in;

	int i = 0;
	while (*current_in != '\0' && i<(BUF_SIZE-1)) {
		(*result)[i] = *current_in;
		i++; current_in++;
	}

	if (i >= (BUF_SIZE-1)) {
		while (*current_in != '\0')
			current_in++;
	}

	(*result)[i] = '\0'; current_in++;

	return current_in;
}

byte * write_string(string *str, byte *out, size_t *len) {
	byte *current_out = out;

	int strsize = (int) strlen((const char *) *str);

	for (long i=0; i<strsize; i++) {
		*current_out++ = (*str)[i]; (*len)++;
	}
	*current_out++ = '\0'; (*len)++;

	return current_out;
}

byte * read_string_array(byte *in, string *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_string(current_in, &array[i]);

	return current_in;
}

byte * write_string_array(string *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_string(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_string_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	string *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_string(ptr, current_out, len_out);
}

void pointer_deref_string_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	string *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_string(&(ptr[index]), current_out, len_out);
}

void pointer_deref_string_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	string *ptr, value;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_string(current_in, &value);
	strcpy((char *) *ptr, (const char *) value);
}

void pointer_deref_string_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	string *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_string(current_in, &value);
	strcpy((char *) ptr[index], (const char *) value);
}

void new_string_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	string *ptr = malloc(sizeof(string));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_string_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	string *ptr = malloc(sizeof(string)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_string_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	string *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

// ---- Pointer ----

byte * read_pointer(byte *in, void **result) {
	byte *current_in = in;
	uintptr_t p;

	current_in = read_int64(current_in, (int64_t *) &p);
	*result = (void *) p;

	return current_in;
}

byte * write_pointer(void **pointer, byte *out, size_t *len) {
	byte *current_out = out;
	uintptr_t p = (uintptr_t) (*pointer);

	current_out = write_int64((int64_t *) &p, current_out, len);

	return current_out;
}

byte * read_pointer_array(byte *in, void **array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_pointer(current_in, &array[i]);

	return current_in;
}

byte * write_pointer_array(void **array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_pointer(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_pointer_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	void **ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_pointer(ptr, current_out, len_out);
}

void pointer_deref_pointer_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	void **ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_pointer(&(ptr[index]), current_out, len_out);
}

void pointer_deref_pointer_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	void **ptr, *value;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_pointer(current_in, &value);
	*ptr = value;
}

void pointer_deref_pointer_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	void **ptr, *value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_pointer(current_in, &value);
	ptr[index] = value;
}

void new_pointer_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	void **ptr = malloc(sizeof(void *));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_pointer_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	void **ptr = malloc(sizeof(void *)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_pointer_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	void **ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

//--------------------------------------------------------

byte * read_arrayA(byte *in, ArrayA *result) {
	byte *current_in = in;

	current_in = read_int(current_in, &(result->id));
	current_in = read_int_array(current_in, result->values, 10);

	return current_in;
}

byte * write_arrayA(ArrayA *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_int(&(value->id), current_out, len);
	current_out = write_int_array(value->values, current_out, len, 10);

	return current_out;
}

byte * read_arrayA_array(byte *in, ArrayA *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_arrayA(current_in, &array[i]);

	return current_in;
}

byte * write_arrayA_array(ArrayA *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_arrayA(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_arrayA_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayA *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_arrayA(pointer, current_out, len_out);
}

void pointer_deref_arrayA_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayA *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_arrayA(&(ptr[index]), current_out, len_out);
}

void pointer_deref_arrayA_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayA *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_arrayA(current_in, pointer);
}

void pointer_deref_arrayA_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayA *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_arrayA(current_in, &value);
	ptr[index] = value;
}

void new_arrayA_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayA *ptr = malloc(sizeof(ArrayA));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_arrayA_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	ArrayA *ptr = malloc(sizeof(ArrayA)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_arrayA_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayA *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void arrayA_get_id_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayA *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int(&(ptr->id), current_out, len_out);
}

void arrayA_set_id_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayA *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &(ptr->id));
}

void arrayA_get_values_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayA *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int_array(ptr->values, current_out, len_out, 10);
}

void arrayA_set_values_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayA *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int_array(current_in, ptr->values, 10);
}

byte * read_arrayB(byte *in, ArrayB *result) {
	byte *current_in = in;

	current_in = read_int(current_in, &(result->id));
	current_in = read_pointer(current_in, (void **) &(result->values));

	return current_in;
}

byte * write_arrayB(ArrayB *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_int(&(value->id), current_out, len);
	current_out = write_pointer((void **) &(value->values), current_out, len);

	return current_out;
}

byte * read_arrayB_array(byte *in, ArrayB *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_arrayB(current_in, &array[i]);

	return current_in;
}

byte * write_arrayB_array(ArrayB *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_arrayB(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_arrayB_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayB *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_arrayB(pointer, current_out, len_out);
}

void pointer_deref_arrayB_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayB *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_arrayB(&(ptr[index]), current_out, len_out);
}

void pointer_deref_arrayB_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayB *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_arrayB(current_in, pointer);
}

void pointer_deref_arrayB_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayB *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_arrayB(current_in, &value);
	ptr[index] = value;
}

void new_arrayB_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayB *ptr = malloc(sizeof(ArrayB));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_arrayB_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	ArrayB *ptr = malloc(sizeof(ArrayB)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_arrayB_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayB *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void arrayB_get_id_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayB *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int(&(ptr->id), current_out, len_out);
}

void arrayB_set_id_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayB *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &(ptr->id));
}

void arrayB_get_values_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayB *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_pointer((void **) &(ptr->values), current_out, len_out);
}

void arrayB_set_values_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayB *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_pointer(current_in, (void **) &(ptr->values));
}

byte * read_arrayC(byte *in, ArrayC *result) {
	byte *current_in = in;

	current_in = read_int(current_in, &(result->id));
	current_in = read_pointer(current_in, (void **) &(result->values));
	current_in = read_int(current_in, &(result->size));

	return current_in;
}

byte * write_arrayC(ArrayC *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_int(&(value->id), current_out, len);
	current_out = write_pointer((void **) &(value->values), current_out, len);
	current_out = write_int(&(value->size), current_out, len);

	return current_out;
}

byte * read_arrayC_array(byte *in, ArrayC *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_arrayC(current_in, &array[i]);

	return current_in;
}

byte * write_arrayC_array(ArrayC *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_arrayC(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_arrayC_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayC *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_arrayC(pointer, current_out, len_out);
}

void pointer_deref_arrayC_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayC *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_arrayC(&(ptr[index]), current_out, len_out);
}

void pointer_deref_arrayC_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayC *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_arrayC(current_in, pointer);
}

void pointer_deref_arrayC_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayC *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_arrayC(current_in, &value);
	ptr[index] = value;
}

void new_arrayC_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayC *ptr = malloc(sizeof(ArrayC));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_arrayC_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	ArrayC *ptr = malloc(sizeof(ArrayC)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_arrayC_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayC *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void arrayC_get_id_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayC *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int(&(ptr->id), current_out, len_out);
}

void arrayC_set_id_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayC *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &(ptr->id));
}

void arrayC_get_values_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayC *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_pointer((void **) &(ptr->values), current_out, len_out);
}

void arrayC_set_values_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayC *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_pointer(current_in, (void **) &(ptr->values));
}

void arrayC_get_size_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayC *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int(&(ptr->size), current_out, len_out);
}

void arrayC_set_size_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	ArrayC *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &(ptr->size));
}

byte * read_uint64(byte *in, Uint64 *result) {
	return read_int64(in, (int64_t *) result);
}

byte * write_uint64(Uint64 *value, byte *out, size_t *len) {
	return write_int64((int64_t *) value, out, len);
}

byte * read_uint64_array(byte *in, Uint64 *array, int n) {
	return read_int64_array(in, (int64_t *) array, n);
}

byte * write_uint64_array(Uint64 *array, byte *out, size_t *len, int n) {
	return write_int64_array((int64_t *) array, out, len, n);
}

byte * read_uint32(byte *in, Uint32 *result) {
	return read_int32(in, (int32_t *) result);
}

byte * write_uint32(Uint32 *value, byte *out, size_t *len) {
	return write_int32((int32_t *) value, out, len);
}

byte * read_uint32_array(byte *in, Uint32 *array, int n) {
	return read_int32_array(in, (int32_t *) array, n);
}

byte * write_uint32_array(Uint32 *array, byte *out, size_t *len, int n) {
	return write_int32_array((int32_t *) array, out, len, n);
}

byte * read_uint16(byte *in, Uint16 *result) {
	return read_int16(in, (int16_t *) result);
}

byte * write_uint16(Uint16 *value, byte *out, size_t *len) {
	return write_int16((int16_t *) value, out, len);
}

byte * read_uint16_array(byte *in, Uint16 *array, int n) {
	return read_int16_array(in, (int16_t *) array, n);
}

byte * write_uint16_array(Uint16 *array, byte *out, size_t *len, int n) {
	return write_int16_array((int16_t *) array, out, len, n);
}

byte * read_uint8(byte *in, Uint8 *result) {
	return read_int8(in, (int8_t *) result);
}

byte * write_uint8(Uint8 *value, byte *out, size_t *len) {
	return write_int8((int8_t *) value, out, len);
}

byte * read_uint8_array(byte *in, Uint8 *array, int n) {
	return read_int8_array(in, (int8_t *) array, n);
}

byte * write_uint8_array(Uint8 *array, byte *out, size_t *len, int n) {
	return write_int8_array((int8_t *) array, out, len, n);
}

byte * read_sint64(byte *in, Sint64 *result) {
	return read_int64(in, (int64_t *) result);
}

byte * write_sint64(Sint64 *value, byte *out, size_t *len) {
	return write_int64((int64_t *) value, out, len);
}

byte * read_sint64_array(byte *in, Sint64 *array, int n) {
	return read_int64_array(in, (int64_t *) array, n);
}

byte * write_sint64_array(Sint64 *array, byte *out, size_t *len, int n) {
	return write_int64_array((int64_t *) array, out, len, n);
}

byte * read_sint32(byte *in, Sint32 *result) {
	return read_int32(in, (int32_t *) result);
}

byte * write_sint32(Sint32 *value, byte *out, size_t *len) {
	return write_int32((int32_t *) value, out, len);
}

byte * read_sint32_array(byte *in, Sint32 *array, int n) {
	return read_int32_array(in, (int32_t *) array, n);
}

byte * write_sint32_array(Sint32 *array, byte *out, size_t *len, int n) {
	return write_int32_array((int32_t *) array, out, len, n);
}

byte * read_sint16(byte *in, Sint16 *result) {
	return read_int16(in, (int16_t *) result);
}

byte * write_sint16(Sint16 *value, byte *out, size_t *len) {
	return write_int16((int16_t *) value, out, len);
}

byte * read_sint16_array(byte *in, Sint16 *array, int n) {
	return read_int16_array(in, (int16_t *) array, n);
}

byte * write_sint16_array(Sint16 *array, byte *out, size_t *len, int n) {
	return write_int16_array((int16_t *) array, out, len, n);
}

byte * read_sint8(byte *in, Sint8 *result) {
	return read_int8(in, (int8_t *) result);
}

byte * write_sint8(Sint8 *value, byte *out, size_t *len) {
	return write_int8((int8_t *) value, out, len);
}

byte * read_sint8_array(byte *in, Sint8 *array, int n) {
	return read_int8_array(in, (int8_t *) array, n);
}

byte * write_sint8_array(Sint8 *array, byte *out, size_t *len, int n) {
	return write_int8_array((int8_t *) array, out, len, n);
}

byte * read_window(byte *in, SDL_Window *result) {
	return read_pointer(in, (void **) &result);
}

byte * write_window(SDL_Window *value, byte *out, size_t *len) {
	return write_pointer((void **) &value, out, len);
}

byte * read_window_array(byte *in, SDL_Window **array, int n) {
	return read_pointer_array(in, (void **) array, n);
}

byte * write_window_array(SDL_Window **array, byte *out, size_t *len, int n) {
	return write_pointer_array((void **) array, out, len, n);
}

void pointer_deref_window_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	pointer_deref_pointer_array_Handler(in, len_in, out, len_out);
}

void pointer_deref_window_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	pointer_deref_pointer_array_assign_Handler(in, len_in, out, len_out);
}

void new_window_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	new_pointer_array_Handler(in, len_in, out, len_out);
}

void delete_window_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	delete_pointer_Handler(in, len_in, out, len_out);
}

byte * read_color(byte *in, SDL_Color *result) {
	byte *current_in = in;

	current_in = read_uint8(current_in, &(result->r));
	current_in = read_uint8(current_in, &(result->g));
	current_in = read_uint8(current_in, &(result->b));
	current_in = read_uint8(current_in, &(result->a));

	return current_in;
}

byte * write_color(SDL_Color *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint8(&(value->r), current_out, len);
	current_out = write_uint8(&(value->g), current_out, len);
	current_out = write_uint8(&(value->b), current_out, len);
	current_out = write_uint8(&(value->a), current_out, len);

	return current_out;
}

byte * read_color_array(byte *in, SDL_Color *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_color(current_in, &array[i]);

	return current_in;
}

byte * write_color_array(SDL_Color *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_color(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_color_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_color(pointer, current_out, len_out);
}

void pointer_deref_color_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_color(&(ptr[index]), current_out, len_out);
}

void pointer_deref_color_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_color(current_in, pointer);
}

void pointer_deref_color_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_color(current_in, &value);
	ptr[index] = value;
}

void new_color_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *ptr = malloc(sizeof(SDL_Color));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_color_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_Color *ptr = malloc(sizeof(SDL_Color)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_color_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void color_get_r_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->r), current_out, len_out);
}

void color_set_r_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->r));
}

void color_get_g_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->g), current_out, len_out);
}

void color_set_g_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->g));
}

void color_get_b_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->b), current_out, len_out);
}

void color_set_b_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->b));
}

void color_get_a_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->a), current_out, len_out);
}

void color_set_a_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->a));
}

byte * read_palette(byte *in, SDL_Palette *result) {
	byte *current_in = in;

	current_in = read_int(current_in, &(result->ncolors));
	current_in = read_pointer(current_in, (void **) &(result->colors));
	current_in = read_uint32(current_in, &(result->version));
	current_in = read_int(current_in, &(result->refcount));

	return current_in;
}

byte * write_palette(SDL_Palette *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_int(&(value->ncolors), current_out, len);
	current_out = write_pointer((void **) &(value->colors), current_out, len);
	current_out = write_uint32(&(value->version), current_out, len);
	current_out = write_int(&(value->refcount), current_out, len);

	return current_out;
}

byte * read_palette_array(byte *in, SDL_Palette *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_palette(current_in, &array[i]);

	return current_in;
}

byte * write_palette_array(SDL_Palette *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_palette(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_palette_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_palette(pointer, current_out, len_out);
}

void pointer_deref_palette_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_palette(&(ptr[index]), current_out, len_out);
}

void pointer_deref_palette_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_palette(current_in, pointer);
}

void pointer_deref_palette_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_palette(current_in, &value);
	ptr[index] = value;
}

void new_palette_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *ptr = malloc(sizeof(SDL_Palette));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_palette_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_Palette *ptr = malloc(sizeof(SDL_Palette)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_palette_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void palette_get_ncolors_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int(&(ptr->ncolors), current_out, len_out);
}

void palette_set_ncolors_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &(ptr->ncolors));
}

void palette_get_colors_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_pointer((void **) &(ptr->colors), current_out, len_out);
}

void palette_set_colors_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_pointer(current_in, (void **) &(ptr->colors));
}

void palette_get_version_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->version), current_out, len_out);
}

void palette_set_version_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->version));
}

void palette_get_refcount_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int(&(ptr->refcount), current_out, len_out);
}

void palette_set_refcount_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &(ptr->refcount));
}

byte * read_pixel_format(byte *in, SDL_PixelFormat *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->format));
	current_in = read_pointer(current_in, (void **) &(result->palette));
	current_in = read_uint8(current_in, &(result->BitsPerPixel));
	current_in = read_uint8(current_in, &(result->BytesPerPixel));
	current_in = read_uint32(current_in, &(result->Rmask));
	current_in = read_uint32(current_in, &(result->Gmask));
	current_in = read_uint32(current_in, &(result->Bmask));
	current_in = read_uint32(current_in, &(result->Amask));
	current_in = read_uint8(current_in, &(result->Rloss));
	current_in = read_uint8(current_in, &(result->Gloss));
	current_in = read_uint8(current_in, &(result->Bloss));
	current_in = read_uint8(current_in, &(result->Aloss));
	current_in = read_uint8(current_in, &(result->Rshift));
	current_in = read_uint8(current_in, &(result->Gshift));
	current_in = read_uint8(current_in, &(result->Bshift));
	current_in = read_uint8(current_in, &(result->Ashift));
	current_in = read_int(current_in, &(result->refcount));
	current_in = read_pointer(current_in, (void **) &(result->next));

	return current_in;
}

byte * write_pixel_format(SDL_PixelFormat *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->format), current_out, len);
	current_out = write_pointer((void **) &(value->palette), current_out, len);
	current_out = write_uint8(&(value->BitsPerPixel), current_out, len);
	current_out = write_uint8(&(value->BytesPerPixel), current_out, len);
	current_out = write_uint32(&(value->Rmask), current_out, len);
	current_out = write_uint32(&(value->Gmask), current_out, len);
	current_out = write_uint32(&(value->Bmask), current_out, len);
	current_out = write_uint32(&(value->Amask), current_out, len);
	current_out = write_uint8(&(value->Rloss), current_out, len);
	current_out = write_uint8(&(value->Gloss), current_out, len);
	current_out = write_uint8(&(value->Bloss), current_out, len);
	current_out = write_uint8(&(value->Aloss), current_out, len);
	current_out = write_uint8(&(value->Rshift), current_out, len);
	current_out = write_uint8(&(value->Gshift), current_out, len);
	current_out = write_uint8(&(value->Bshift), current_out, len);
	current_out = write_uint8(&(value->Ashift), current_out, len);
	current_out = write_int(&(value->refcount), current_out, len);
	current_out = write_pointer((void **) &(value->next), current_out, len);

	return current_out;
}

byte * read_pixel_format_array(byte *in, SDL_PixelFormat *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_pixel_format(current_in, &array[i]);

	return current_in;
}

byte * write_pixel_format_array(SDL_PixelFormat *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_pixel_format(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_pixel_format_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_pixel_format(pointer, current_out, len_out);
}

void pointer_deref_pixel_format_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_pixel_format(&(ptr[index]), current_out, len_out);
}

void pointer_deref_pixel_format_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_pixel_format(current_in, pointer);
}

void pointer_deref_pixel_format_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_pixel_format(current_in, &value);
	ptr[index] = value;
}

void new_pixel_format_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr = malloc(sizeof(SDL_PixelFormat));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_pixel_format_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_PixelFormat *ptr = malloc(sizeof(SDL_PixelFormat)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_pixel_format_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void pixel_format_get_format_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->format), current_out, len_out);
}

void pixel_format_set_format_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->format));
}

void pixel_format_get_palette_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_pointer((void **) &(ptr->palette), current_out, len_out);
}

void pixel_format_set_palette_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_pointer(current_in, (void **) &(ptr->palette));
}

void pixel_format_get_bits_per_pixel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->BitsPerPixel), current_out, len_out);
}

void pixel_format_set_bits_per_pixel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->BitsPerPixel));
}

void pixel_format_get_bytes_per_pixel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->BytesPerPixel), current_out, len_out);
}

void pixel_format_set_bytes_per_pixel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->BytesPerPixel));
}

void pixel_format_get_r_mask_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->Rmask), current_out, len_out);
}

void pixel_format_set_r_mask_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->Rmask));
}

void pixel_format_get_g_mask_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->Gmask), current_out, len_out);
}

void pixel_format_set_g_mask_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->Gmask));
}

void pixel_format_get_b_mask_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->Bmask), current_out, len_out);
}

void pixel_format_set_b_mask_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->Bmask));
}

void pixel_format_get_a_mask_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->Amask), current_out, len_out);
}

void pixel_format_set_a_mask_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->Amask));
}

void pixel_format_get_r_loss_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->Rloss), current_out, len_out);
}

void pixel_format_set_r_loss_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->Rloss));
}

void pixel_format_get_g_loss_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->Gloss), current_out, len_out);
}

void pixel_format_set_g_loss_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->Gloss));
}

void pixel_format_get_b_loss_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->Bloss), current_out, len_out);
}

void pixel_format_set_b_loss_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->Bloss));
}

void pixel_format_get_a_loss_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->Aloss), current_out, len_out);
}

void pixel_format_set_a_loss_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->Aloss));
}

void pixel_format_get_r_shift_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->Rshift), current_out, len_out);
}

void pixel_format_set_r_shift_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->Rshift));
}

void pixel_format_get_g_shift_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->Gshift), current_out, len_out);
}

void pixel_format_set_g_shift_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->Gshift));
}

void pixel_format_get_b_shift_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->Bshift), current_out, len_out);
}

void pixel_format_set_b_shift_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->Bshift));
}

void pixel_format_get_a_shift_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->Ashift), current_out, len_out);
}

void pixel_format_set_a_shift_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->Ashift));
}

void pixel_format_get_refcount_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int(&(ptr->refcount), current_out, len_out);
}

void pixel_format_set_refcount_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &(ptr->refcount));
}

void pixel_format_get_next_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_pointer((void **) &(ptr->next), current_out, len_out);
}

void pixel_format_set_next_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_pointer(current_in, (void **) &(ptr->next));
}

byte * read_rect(byte *in, SDL_Rect *result) {
	byte *current_in = in;

	current_in = read_int(current_in, &(result->x));
	current_in = read_int(current_in, &(result->y));
	current_in = read_int(current_in, &(result->w));
	current_in = read_int(current_in, &(result->h));

	return current_in;
}

byte * write_rect(SDL_Rect *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_int(&(value->x), current_out, len);
	current_out = write_int(&(value->y), current_out, len);
	current_out = write_int(&(value->w), current_out, len);
	current_out = write_int(&(value->h), current_out, len);

	return current_out;
}

byte * read_rect_array(byte *in, SDL_Rect *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_rect(current_in, &array[i]);

	return current_in;
}

byte * write_rect_array(SDL_Rect *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_rect(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_rect_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_rect(pointer, current_out, len_out);
}

void pointer_deref_rect_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_rect(&(ptr[index]), current_out, len_out);
}

void pointer_deref_rect_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_rect(current_in, pointer);
}

void pointer_deref_rect_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_rect(current_in, &value);
	ptr[index] = value;
}

void new_rect_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *ptr = malloc(sizeof(SDL_Rect));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_rect_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_Rect *ptr = malloc(sizeof(SDL_Rect)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_rect_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void rect_get_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int(&(ptr->x), current_out, len_out);
}

void rect_set_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &(ptr->x));
}

void rect_get_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int(&(ptr->y), current_out, len_out);
}

void rect_set_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &(ptr->y));
}

void rect_get_w_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int(&(ptr->w), current_out, len_out);
}

void rect_set_w_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &(ptr->w));
}

void rect_get_h_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int(&(ptr->h), current_out, len_out);
}

void rect_set_h_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &(ptr->h));
}

byte * read_blit_map(byte *in, struct SDL_BlitMap *result) {
	return read_pointer(in, (void **) &result);
}

byte * write_blit_map(struct SDL_BlitMap *value, byte *out, size_t *len) {
	return write_pointer((void **) &value, out, len);
}

byte * read_blit_map_array(byte *in, struct SDL_BlitMap **array, int n) {
	return read_pointer_array(in, (void **) array, n);
}

byte * write_blit_map_array(struct SDL_BlitMap **array, byte *out, size_t *len, int n) {
	return write_pointer_array((void **) array, out, len, n);
}

void pointer_deref_blit_map_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	pointer_deref_pointer_array_Handler(in, len_in, out, len_out);
}

void pointer_deref_blit_map_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	pointer_deref_pointer_array_assign_Handler(in, len_in, out, len_out);
}

void new_blit_map_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	new_pointer_array_Handler(in, len_in, out, len_out);
}

void delete_blit_map_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	delete_pointer_Handler(in, len_in, out, len_out);
}

byte * read_surface(byte *in, SDL_Surface *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->flags));
	current_in = read_pointer(current_in, (void **) &(result->format));
	current_in = read_int(current_in, &(result->w));
	current_in = read_int(current_in, &(result->h));
	current_in = read_int(current_in, &(result->pitch));
	current_in = read_pointer(current_in, (void **) &(result->pixels));
	current_in = read_pointer(current_in, (void **) &(result->userdata));
	current_in = read_int(current_in, &(result->locked));
	current_in = read_pointer(current_in, (void **) &(result->lock_data));
	current_in = read_rect(current_in, &(result->clip_rect));
	current_in = read_pointer(current_in, (void **) &(result->map));
	current_in = read_int(current_in, &(result->refcount));

	return current_in;
}

byte * write_surface(SDL_Surface *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->flags), current_out, len);
	current_out = write_pointer((void **) &(value->format), current_out, len);
	current_out = write_int(&(value->w), current_out, len);
	current_out = write_int(&(value->h), current_out, len);
	current_out = write_int(&(value->pitch), current_out, len);
	current_out = write_pointer((void **) &(value->pixels), current_out, len);
	current_out = write_pointer((void **) &(value->userdata), current_out, len);
	current_out = write_int(&(value->locked), current_out, len);
	current_out = write_pointer((void **) &(value->lock_data), current_out, len);
	current_out = write_rect(&(value->clip_rect), current_out, len);
	current_out = write_pointer((void **) &(value->map), current_out, len);
	current_out = write_int(&(value->refcount), current_out, len);

	return current_out;
}

byte * read_surface_array(byte *in, SDL_Surface *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_surface(current_in, &array[i]);

	return current_in;
}

byte * write_surface_array(SDL_Surface *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_surface(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_surface_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_surface(pointer, current_out, len_out);
}

void pointer_deref_surface_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_surface(&(ptr[index]), current_out, len_out);
}

void pointer_deref_surface_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_surface(current_in, pointer);
}

void pointer_deref_surface_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_surface(current_in, &value);
	ptr[index] = value;
}

void new_surface_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr = malloc(sizeof(SDL_Surface));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_surface_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_Surface *ptr = malloc(sizeof(SDL_Surface)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_surface_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void surface_get_flags_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->flags), current_out, len_out);
}

void surface_set_flags_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->flags));
}

void surface_get_format_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_pointer((void **) &(ptr->format), current_out, len_out);
}

void surface_set_format_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_pointer(current_in, (void **) &(ptr->format));
}

void surface_get_w_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int(&(ptr->w), current_out, len_out);
}

void surface_set_w_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &(ptr->w));
}

void surface_get_h_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int(&(ptr->h), current_out, len_out);
}

void surface_set_h_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &(ptr->h));
}

void surface_get_pitch_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int(&(ptr->pitch), current_out, len_out);
}

void surface_set_pitch_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &(ptr->pitch));
}

void surface_get_pixels_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_pointer((void **) &(ptr->pixels), current_out, len_out);
}

void surface_set_pixels_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_pointer(current_in, (void **) &(ptr->pixels));
}

void surface_get_userdata_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_pointer((void **) &(ptr->userdata), current_out, len_out);
}

void surface_set_userdata_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_pointer(current_in, (void **) &(ptr->userdata));
}

void surface_get_locked_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int(&(ptr->locked), current_out, len_out);
}

void surface_set_locked_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &(ptr->locked));
}

void surface_get_lock_data_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_pointer((void **) &(ptr->lock_data), current_out, len_out);
}

void surface_set_lock_data_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_pointer(current_in, (void **) &(ptr->lock_data));
}

void surface_get_clip_rect_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_rect(&(ptr->clip_rect), current_out, len_out);
}

void surface_set_clip_rect_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_rect(current_in, &(ptr->clip_rect));
}

void surface_get_map_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_pointer((void **) &(ptr->map), current_out, len_out);
}

void surface_set_map_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_pointer(current_in, (void **) &(ptr->map));
}

void surface_get_refcount_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int(&(ptr->refcount), current_out, len_out);
}

void surface_set_refcount_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &(ptr->refcount));
}

byte * read_scancode(byte *in, SDL_Scancode *result) {
	return read_int(in, (int *) result);
}

byte * write_scancode(SDL_Scancode *value, byte *out, size_t *len) {
	return write_int((int *) value, out, len);
}

byte * read_scancode_array(byte *in, SDL_Scancode *array, int n) {
	return read_int_array(in, (int *) array, n);
}

byte * write_scancode_array(SDL_Scancode *array, byte *out, size_t *len, int n) {
	return write_int_array((int *) array, out, len, n);
}

byte * read_keycode(byte *in, SDL_Keycode *result) {
	return read_sint32(in, result);
}

byte * write_keycode(SDL_Keycode *value, byte *out, size_t *len) {
	return write_sint32(value, out, len);
}

byte * read_keycode_array(byte *in, SDL_Keycode *array, int n) {
	return read_sint32_array(in, array, n);
}

byte * write_keycode_array(SDL_Keycode *array, byte *out, size_t *len, int n) {
	return write_sint32_array(array, out, len, n);
}

byte * read_keysym(byte *in, SDL_Keysym *result) {
	byte *current_in = in;

	current_in = read_scancode(current_in, &(result->scancode));
	current_in = read_keycode(current_in, &(result->sym));
	current_in = read_uint16(current_in, &(result->mod));
	current_in = read_uint32(current_in, &(result->unused));

	return current_in;
}

byte * write_keysym(SDL_Keysym *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_scancode(&(value->scancode), current_out, len);
	current_out = write_keycode(&(value->sym), current_out, len);
	current_out = write_uint16(&(value->mod), current_out, len);
	current_out = write_uint32(&(value->unused), current_out, len);

	return current_out;
}

byte * read_keysym_array(byte *in, SDL_Keysym *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_keysym(current_in, &array[i]);

	return current_in;
}

byte * write_keysym_array(SDL_Keysym *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_keysym(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_keysym_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_keysym(pointer, current_out, len_out);
}

void pointer_deref_keysym_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_keysym(&(ptr[index]), current_out, len_out);
}

void pointer_deref_keysym_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_keysym(current_in, pointer);
}

void pointer_deref_keysym_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_keysym(current_in, &value);
	ptr[index] = value;
}

void new_keysym_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *ptr = malloc(sizeof(SDL_Keysym));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_keysym_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_Keysym *ptr = malloc(sizeof(SDL_Keysym)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_keysym_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void keysym_get_scancode_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_scancode(&(ptr->scancode), current_out, len_out);
}

void keysym_set_scancode_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_scancode(current_in, &(ptr->scancode));
}

void keysym_get_sym_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_keycode(&(ptr->sym), current_out, len_out);
}

void keysym_set_sym_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_keycode(current_in, &(ptr->sym));
}

void keysym_get_mod_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint16(&(ptr->mod), current_out, len_out);
}

void keysym_set_mod_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint16(current_in, &(ptr->mod));
}

void keysym_get_unused_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->unused), current_out, len_out);
}

void keysym_set_unused_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->unused));
}

byte * read_joystick_id(byte *in, SDL_JoystickID *result) {
	return read_sint32(in, result);
}

byte * write_joystick_id(SDL_JoystickID *value, byte *out, size_t *len) {
	return write_sint32(value, out, len);
}

byte * read_joystick_id_array(byte *in, SDL_JoystickID *array, int n) {
	return read_sint32_array(in, array, n);
}

byte * write_joystick_id_array(SDL_JoystickID *array, byte *out, size_t *len, int n) {
	return write_sint32_array(array, out, len, n);
}

byte * read_syswm_msg(byte *in, SDL_SysWMmsg *result) {
	return read_pointer(in, (void **) &result);
}

byte * write_syswm_msg(SDL_SysWMmsg *value, byte *out, size_t *len) {
	return write_pointer((void **) &value, out, len);
}

byte * read_syswm_msg_array(byte *in, SDL_SysWMmsg **array, int n) {
	return read_pointer_array(in, (void **) array, n);
}

byte * write_syswm_msg_array(SDL_SysWMmsg **array, byte *out, size_t *len, int n) {
	return write_pointer_array((void **) array, out, len, n);
}

void pointer_deref_syswm_msg_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	pointer_deref_pointer_array_Handler(in, len_in, out, len_out);
}

void pointer_deref_syswm_msg_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	pointer_deref_pointer_array_assign_Handler(in, len_in, out, len_out);
}

void new_syswm_msg_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	new_pointer_array_Handler(in, len_in, out, len_out);
}

void delete_syswm_msg_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	delete_pointer_Handler(in, len_in, out, len_out);
}

byte * read_touch_id(byte *in, SDL_TouchID *result) {
	return read_sint64(in, result);
}

byte * write_touch_id(SDL_TouchID *value, byte *out, size_t *len) {
	return write_sint64(value, out, len);
}

byte * read_touch_id_array(byte *in, SDL_TouchID *array, int n) {
	return read_sint64_array(in, array, n);
}

byte * write_touch_id_array(SDL_TouchID *array, byte *out, size_t *len, int n) {
	return write_sint64_array(array, out, len, n);
}

byte * read_finger_id(byte *in, SDL_FingerID *result) {
	return read_sint64(in, result);
}

byte * write_finger_id(SDL_FingerID *value, byte *out, size_t *len) {
	return write_sint64(value, out, len);
}

byte * read_finger_id_array(byte *in, SDL_FingerID *array, int n) {
	return read_sint64_array(in, array, n);
}

byte * write_finger_id_array(SDL_FingerID *array, byte *out, size_t *len, int n) {
	return write_sint64_array(array, out, len, n);
}

byte * read_gesture_id(byte *in, SDL_GestureID *result) {
	return read_sint64(in, result);
}

byte * write_gesture_id(SDL_GestureID *value, byte *out, size_t *len) {
	return write_sint64(value, out, len);
}

byte * read_gesture_id_array(byte *in, SDL_GestureID *array, int n) {
	return read_sint64_array(in, array, n);
}

byte * write_gesture_id_array(SDL_GestureID *array, byte *out, size_t *len, int n) {
	return write_sint64_array(array, out, len, n);
}

byte * read_common_event(byte *in, SDL_CommonEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));

	return current_in;
}

byte * write_common_event(SDL_CommonEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);

	return current_out;
}

byte * read_common_event_array(byte *in, SDL_CommonEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_common_event(current_in, &array[i]);

	return current_in;
}

byte * write_common_event_array(SDL_CommonEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_common_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_common_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_CommonEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_common_event(pointer, current_out, len_out);
}

void pointer_deref_common_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_CommonEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_common_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_common_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_CommonEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_common_event(current_in, pointer);
}

void pointer_deref_common_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_CommonEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_common_event(current_in, &value);
	ptr[index] = value;
}

void new_common_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_CommonEvent *ptr = malloc(sizeof(SDL_CommonEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_common_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_CommonEvent *ptr = malloc(sizeof(SDL_CommonEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_common_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_CommonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void common_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_CommonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void common_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_CommonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void common_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_CommonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void common_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_CommonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

byte * read_window_event(byte *in, SDL_WindowEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_uint32(current_in, &(result->windowID));
	current_in = read_uint8(current_in, &(result->event));
	current_in = read_uint8(current_in, &(result->padding1));
	current_in = read_uint8(current_in, &(result->padding2));
	current_in = read_uint8(current_in, &(result->padding3));
	current_in = read_sint32(current_in, &(result->data1));
	current_in = read_sint32(current_in, &(result->data2));

	return current_in;
}

byte * write_window_event(SDL_WindowEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_uint32(&(value->windowID), current_out, len);
	current_out = write_uint8(&(value->event), current_out, len);
	current_out = write_uint8(&(value->padding1), current_out, len);
	current_out = write_uint8(&(value->padding2), current_out, len);
	current_out = write_uint8(&(value->padding3), current_out, len);
	current_out = write_sint32(&(value->data1), current_out, len);
	current_out = write_sint32(&(value->data2), current_out, len);

	return current_out;
}

byte * read_window_event_array(byte *in, SDL_WindowEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_window_event(current_in, &array[i]);

	return current_in;
}

byte * write_window_event_array(SDL_WindowEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_window_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_window_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_window_event(pointer, current_out, len_out);
}

void pointer_deref_window_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_window_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_window_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_window_event(current_in, pointer);
}

void pointer_deref_window_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_window_event(current_in, &value);
	ptr[index] = value;
}

void new_window_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr = malloc(sizeof(SDL_WindowEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_window_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_WindowEvent *ptr = malloc(sizeof(SDL_WindowEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_window_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void window_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void window_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void window_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void window_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void window_event_get_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->windowID), current_out, len_out);
}

void window_event_set_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->windowID));
}

void window_event_get_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->event), current_out, len_out);
}

void window_event_set_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->event));
}

void window_event_get_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding1), current_out, len_out);
}

void window_event_set_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding1));
}

void window_event_get_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding2), current_out, len_out);
}

void window_event_set_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding2));
}

void window_event_get_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding3), current_out, len_out);
}

void window_event_set_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding3));
}

void window_event_get_data1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_sint32(&(ptr->data1), current_out, len_out);
}

void window_event_set_data1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_sint32(current_in, &(ptr->data1));
}

void window_event_get_data2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_sint32(&(ptr->data2), current_out, len_out);
}

void window_event_set_data2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_sint32(current_in, &(ptr->data2));
}

byte * read_keyboard_event(byte *in, SDL_KeyboardEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_uint32(current_in, &(result->windowID));
	current_in = read_uint8(current_in, &(result->state));
	current_in = read_uint8(current_in, &(result->repeat));
	current_in = read_uint8(current_in, &(result->padding2));
	current_in = read_uint8(current_in, &(result->padding3));
	current_in = read_keysym(current_in, &(result->keysym));

	return current_in;
}

byte * write_keyboard_event(SDL_KeyboardEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_uint32(&(value->windowID), current_out, len);
	current_out = write_uint8(&(value->state), current_out, len);
	current_out = write_uint8(&(value->repeat), current_out, len);
	current_out = write_uint8(&(value->padding2), current_out, len);
	current_out = write_uint8(&(value->padding3), current_out, len);
	current_out = write_keysym(&(value->keysym), current_out, len);

	return current_out;
}

byte * read_keyboard_event_array(byte *in, SDL_KeyboardEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_keyboard_event(current_in, &array[i]);

	return current_in;
}

byte * write_keyboard_event_array(SDL_KeyboardEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_keyboard_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_keyboard_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_keyboard_event(pointer, current_out, len_out);
}

void pointer_deref_keyboard_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_keyboard_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_keyboard_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_keyboard_event(current_in, pointer);
}

void pointer_deref_keyboard_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_keyboard_event(current_in, &value);
	ptr[index] = value;
}

void new_keyboard_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr = malloc(sizeof(SDL_KeyboardEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_keyboard_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_KeyboardEvent *ptr = malloc(sizeof(SDL_KeyboardEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_keyboard_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void keyboard_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void keyboard_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void keyboard_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void keyboard_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void keyboard_event_get_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->windowID), current_out, len_out);
}

void keyboard_event_set_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->windowID));
}

void keyboard_event_get_state_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->state), current_out, len_out);
}

void keyboard_event_set_state_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->state));
}

void keyboard_event_get_repeat_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->repeat), current_out, len_out);
}

void keyboard_event_set_repeat_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->repeat));
}

void keyboard_event_get_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding2), current_out, len_out);
}

void keyboard_event_set_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding2));
}

void keyboard_event_get_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding3), current_out, len_out);
}

void keyboard_event_set_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding3));
}

void keyboard_event_get_keysym_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_keysym(&(ptr->keysym), current_out, len_out);
}

void keyboard_event_set_keysym_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_keysym(current_in, &(ptr->keysym));
}

byte * read_text_editing_event(byte *in, SDL_TextEditingEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_uint32(current_in, &(result->windowID));
	current_in = read_string(current_in, (string *) result->text);
	current_in = read_sint32(current_in, &(result->start));
	current_in = read_sint32(current_in, &(result->length));

	return current_in;
}

byte * write_text_editing_event(SDL_TextEditingEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_uint32(&(value->windowID), current_out, len);
	current_out = write_string((string *) value->text, current_out, len);
	current_out = write_sint32(&(value->start), current_out, len);
	current_out = write_sint32(&(value->length), current_out, len);

	return current_out;
}

byte * read_text_editing_event_array(byte *in, SDL_TextEditingEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_text_editing_event(current_in, &array[i]);

	return current_in;
}

byte * write_text_editing_event_array(SDL_TextEditingEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_text_editing_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_text_editing_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_text_editing_event(pointer, current_out, len_out);
}

void pointer_deref_text_editing_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_text_editing_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_text_editing_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_text_editing_event(current_in, pointer);
}

void pointer_deref_text_editing_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_text_editing_event(current_in, &value);
	ptr[index] = value;
}

void new_text_editing_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr = malloc(sizeof(SDL_TextEditingEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_text_editing_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_TextEditingEvent *ptr = malloc(sizeof(SDL_TextEditingEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_text_editing_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void text_editing_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void text_editing_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void text_editing_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void text_editing_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void text_editing_event_get_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->windowID), current_out, len_out);
}

void text_editing_event_set_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->windowID));
}

void text_editing_event_get_text_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_string((string *) ptr->text, current_out, len_out);
}

void text_editing_event_set_text_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_string(current_in, (string *) ptr->text);
}

void text_editing_event_get_start_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_sint32(&(ptr->start), current_out, len_out);
}

void text_editing_event_set_start_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_sint32(current_in, &(ptr->start));
}

void text_editing_event_get_length_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_sint32(&(ptr->length), current_out, len_out);
}

void text_editing_event_set_length_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_sint32(current_in, &(ptr->length));
}

byte * read_text_input_event(byte *in, SDL_TextInputEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_uint32(current_in, &(result->windowID));
	current_in = read_string(current_in, (string *) result->text);

	return current_in;
}

byte * write_text_input_event(SDL_TextInputEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_uint32(&(value->windowID), current_out, len);
	current_out = write_string((string *) value->text, current_out, len);

	return current_out;
}

byte * read_text_input_event_array(byte *in, SDL_TextInputEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_text_input_event(current_in, &array[i]);

	return current_in;
}

byte * write_text_input_event_array(SDL_TextInputEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_text_input_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_text_input_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_text_input_event(pointer, current_out, len_out);
}

void pointer_deref_text_input_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_text_input_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_text_input_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_text_input_event(current_in, pointer);
}

void pointer_deref_text_input_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_text_input_event(current_in, &value);
	ptr[index] = value;
}

void new_text_input_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *ptr = malloc(sizeof(SDL_TextInputEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_text_input_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_TextInputEvent *ptr = malloc(sizeof(SDL_TextInputEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_text_input_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void text_input_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void text_input_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void text_input_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void text_input_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void text_input_event_get_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->windowID), current_out, len_out);
}

void text_input_event_set_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->windowID));
}

void text_input_event_get_text_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_string((string *) ptr->text, current_out, len_out);
}

void text_input_event_set_text_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_string(current_in, (string *) ptr->text);
}

byte * read_mouse_motion_event(byte *in, SDL_MouseMotionEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_uint32(current_in, &(result->windowID));
	current_in = read_uint32(current_in, &(result->which));
	current_in = read_uint32(current_in, &(result->state));
	current_in = read_sint32(current_in, &(result->x));
	current_in = read_sint32(current_in, &(result->y));
	current_in = read_sint32(current_in, &(result->xrel));
	current_in = read_sint32(current_in, &(result->yrel));

	return current_in;
}

byte * write_mouse_motion_event(SDL_MouseMotionEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_uint32(&(value->windowID), current_out, len);
	current_out = write_uint32(&(value->which), current_out, len);
	current_out = write_uint32(&(value->state), current_out, len);
	current_out = write_sint32(&(value->x), current_out, len);
	current_out = write_sint32(&(value->y), current_out, len);
	current_out = write_sint32(&(value->xrel), current_out, len);
	current_out = write_sint32(&(value->yrel), current_out, len);

	return current_out;
}

byte * read_mouse_motion_event_array(byte *in, SDL_MouseMotionEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_mouse_motion_event(current_in, &array[i]);

	return current_in;
}

byte * write_mouse_motion_event_array(SDL_MouseMotionEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_mouse_motion_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_mouse_motion_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_mouse_motion_event(pointer, current_out, len_out);
}

void pointer_deref_mouse_motion_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_mouse_motion_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_mouse_motion_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_mouse_motion_event(current_in, pointer);
}

void pointer_deref_mouse_motion_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_mouse_motion_event(current_in, &value);
	ptr[index] = value;
}

void new_mouse_motion_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr = malloc(sizeof(SDL_MouseMotionEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_mouse_motion_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_MouseMotionEvent *ptr = malloc(sizeof(SDL_MouseMotionEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_mouse_motion_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void mouse_motion_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void mouse_motion_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void mouse_motion_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void mouse_motion_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void mouse_motion_event_get_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->windowID), current_out, len_out);
}

void mouse_motion_event_set_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->windowID));
}

void mouse_motion_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->which), current_out, len_out);
}

void mouse_motion_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->which));
}

void mouse_motion_event_get_state_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->state), current_out, len_out);
}

void mouse_motion_event_set_state_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->state));
}

void mouse_motion_event_get_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_sint32(&(ptr->x), current_out, len_out);
}

void mouse_motion_event_set_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_sint32(current_in, &(ptr->x));
}

void mouse_motion_event_get_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_sint32(&(ptr->y), current_out, len_out);
}

void mouse_motion_event_set_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_sint32(current_in, &(ptr->y));
}

void mouse_motion_event_get_xrel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_sint32(&(ptr->xrel), current_out, len_out);
}

void mouse_motion_event_set_xrel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_sint32(current_in, &(ptr->xrel));
}

void mouse_motion_event_get_yrel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_sint32(&(ptr->yrel), current_out, len_out);
}

void mouse_motion_event_set_yrel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_sint32(current_in, &(ptr->yrel));
}

byte * read_mouse_button_event(byte *in, SDL_MouseButtonEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_uint32(current_in, &(result->windowID));
	current_in = read_uint32(current_in, &(result->which));
	current_in = read_uint8(current_in, &(result->button));
	current_in = read_uint8(current_in, &(result->state));
	current_in = read_uint8(current_in, &(result->clicks));
	current_in = read_sint32(current_in, &(result->x));
	current_in = read_sint32(current_in, &(result->y));

	return current_in;
}

byte * write_mouse_button_event(SDL_MouseButtonEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_uint32(&(value->windowID), current_out, len);
	current_out = write_uint32(&(value->which), current_out, len);
	current_out = write_uint8(&(value->button), current_out, len);
	current_out = write_uint8(&(value->state), current_out, len);
	current_out = write_uint8(&(value->clicks), current_out, len);
	current_out = write_sint32(&(value->x), current_out, len);
	current_out = write_sint32(&(value->y), current_out, len);

	return current_out;
}

byte * read_mouse_button_event_array(byte *in, SDL_MouseButtonEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_mouse_button_event(current_in, &array[i]);

	return current_in;
}

byte * write_mouse_button_event_array(SDL_MouseButtonEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_mouse_button_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_mouse_button_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_mouse_button_event(pointer, current_out, len_out);
}

void pointer_deref_mouse_button_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_mouse_button_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_mouse_button_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_mouse_button_event(current_in, pointer);
}

void pointer_deref_mouse_button_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_mouse_button_event(current_in, &value);
	ptr[index] = value;
}

void new_mouse_button_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr = malloc(sizeof(SDL_MouseButtonEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_mouse_button_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_MouseButtonEvent *ptr = malloc(sizeof(SDL_MouseButtonEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_mouse_button_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void mouse_button_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void mouse_button_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void mouse_button_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void mouse_button_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void mouse_button_event_get_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->windowID), current_out, len_out);
}

void mouse_button_event_set_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->windowID));
}

void mouse_button_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->which), current_out, len_out);
}

void mouse_button_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->which));
}

void mouse_button_event_get_button_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->button), current_out, len_out);
}

void mouse_button_event_set_button_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->button));
}

void mouse_button_event_get_state_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->state), current_out, len_out);
}

void mouse_button_event_set_state_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->state));
}

void mouse_button_event_get_clicks_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->clicks), current_out, len_out);
}

void mouse_button_event_set_clicks_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->clicks));
}

void mouse_button_event_get_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_sint32(&(ptr->x), current_out, len_out);
}

void mouse_button_event_set_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_sint32(current_in, &(ptr->x));
}

void mouse_button_event_get_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_sint32(&(ptr->y), current_out, len_out);
}

void mouse_button_event_set_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_sint32(current_in, &(ptr->y));
}

byte * read_mouse_wheel_event(byte *in, SDL_MouseWheelEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_uint32(current_in, &(result->windowID));
	current_in = read_uint32(current_in, &(result->which));
	current_in = read_sint32(current_in, &(result->x));
	current_in = read_sint32(current_in, &(result->y));
	current_in = read_uint32(current_in, &(result->direction));

	return current_in;
}

byte * write_mouse_wheel_event(SDL_MouseWheelEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_uint32(&(value->windowID), current_out, len);
	current_out = write_uint32(&(value->which), current_out, len);
	current_out = write_sint32(&(value->x), current_out, len);
	current_out = write_sint32(&(value->y), current_out, len);
	current_out = write_uint32(&(value->direction), current_out, len);

	return current_out;
}

byte * read_mouse_wheel_event_array(byte *in, SDL_MouseWheelEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_mouse_wheel_event(current_in, &array[i]);

	return current_in;
}

byte * write_mouse_wheel_event_array(SDL_MouseWheelEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_mouse_wheel_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_mouse_wheel_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_mouse_wheel_event(pointer, current_out, len_out);
}

void pointer_deref_mouse_wheel_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_mouse_wheel_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_mouse_wheel_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_mouse_wheel_event(current_in, pointer);
}

void pointer_deref_mouse_wheel_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_mouse_wheel_event(current_in, &value);
	ptr[index] = value;
}

void new_mouse_wheel_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr = malloc(sizeof(SDL_MouseWheelEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_mouse_wheel_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_MouseWheelEvent *ptr = malloc(sizeof(SDL_MouseWheelEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_mouse_wheel_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void mouse_wheel_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void mouse_wheel_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void mouse_wheel_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void mouse_wheel_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void mouse_wheel_event_get_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->windowID), current_out, len_out);
}

void mouse_wheel_event_set_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->windowID));
}

void mouse_wheel_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->which), current_out, len_out);
}

void mouse_wheel_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->which));
}

void mouse_wheel_event_get_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_sint32(&(ptr->x), current_out, len_out);
}

void mouse_wheel_event_set_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_sint32(current_in, &(ptr->x));
}

void mouse_wheel_event_get_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_sint32(&(ptr->y), current_out, len_out);
}

void mouse_wheel_event_set_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_sint32(current_in, &(ptr->y));
}

void mouse_wheel_event_get_direction_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->direction), current_out, len_out);
}

void mouse_wheel_event_set_direction_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->direction));
}

byte * read_joy_axis_event(byte *in, SDL_JoyAxisEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_joystick_id(current_in, &(result->which));
	current_in = read_uint8(current_in, &(result->axis));
	current_in = read_uint8(current_in, &(result->padding1));
	current_in = read_uint8(current_in, &(result->padding2));
	current_in = read_uint8(current_in, &(result->padding3));
	current_in = read_sint16(current_in, &(result->value));

	return current_in;
}

byte * write_joy_axis_event(SDL_JoyAxisEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_joystick_id(&(value->which), current_out, len);
	current_out = write_uint8(&(value->axis), current_out, len);
	current_out = write_uint8(&(value->padding1), current_out, len);
	current_out = write_uint8(&(value->padding2), current_out, len);
	current_out = write_uint8(&(value->padding3), current_out, len);
	current_out = write_sint16(&(value->value), current_out, len);

	return current_out;
}

byte * read_joy_axis_event_array(byte *in, SDL_JoyAxisEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_joy_axis_event(current_in, &array[i]);

	return current_in;
}

byte * write_joy_axis_event_array(SDL_JoyAxisEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_joy_axis_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_joy_axis_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_joy_axis_event(pointer, current_out, len_out);
}

void pointer_deref_joy_axis_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_joy_axis_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_joy_axis_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_joy_axis_event(current_in, pointer);
}

void pointer_deref_joy_axis_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_joy_axis_event(current_in, &value);
	ptr[index] = value;
}

void new_joy_axis_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr = malloc(sizeof(SDL_JoyAxisEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_joy_axis_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_JoyAxisEvent *ptr = malloc(sizeof(SDL_JoyAxisEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_joy_axis_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void joy_axis_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void joy_axis_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void joy_axis_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void joy_axis_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void joy_axis_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_joystick_id(&(ptr->which), current_out, len_out);
}

void joy_axis_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_joystick_id(current_in, &(ptr->which));
}

void joy_axis_event_get_axis_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->axis), current_out, len_out);
}

void joy_axis_event_set_axis_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->axis));
}

void joy_axis_event_get_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding1), current_out, len_out);
}

void joy_axis_event_set_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding1));
}

void joy_axis_event_get_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding2), current_out, len_out);
}

void joy_axis_event_set_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding2));
}

void joy_axis_event_get_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding3), current_out, len_out);
}

void joy_axis_event_set_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding3));
}

void joy_axis_event_get_value_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_sint16(&(ptr->value), current_out, len_out);
}

void joy_axis_event_set_value_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_sint16(current_in, &(ptr->value));
}

byte * read_joy_ball_event(byte *in, SDL_JoyBallEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_joystick_id(current_in, &(result->which));
	current_in = read_uint8(current_in, &(result->ball));
	current_in = read_uint8(current_in, &(result->padding1));
	current_in = read_uint8(current_in, &(result->padding2));
	current_in = read_uint8(current_in, &(result->padding3));
	current_in = read_sint16(current_in, &(result->xrel));
	current_in = read_sint16(current_in, &(result->yrel));

	return current_in;
}

byte * write_joy_ball_event(SDL_JoyBallEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_joystick_id(&(value->which), current_out, len);
	current_out = write_uint8(&(value->ball), current_out, len);
	current_out = write_uint8(&(value->padding1), current_out, len);
	current_out = write_uint8(&(value->padding2), current_out, len);
	current_out = write_uint8(&(value->padding3), current_out, len);
	current_out = write_sint16(&(value->xrel), current_out, len);
	current_out = write_sint16(&(value->yrel), current_out, len);

	return current_out;
}

byte * read_joy_ball_event_array(byte *in, SDL_JoyBallEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_joy_ball_event(current_in, &array[i]);

	return current_in;
}

byte * write_joy_ball_event_array(SDL_JoyBallEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_joy_ball_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_joy_ball_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_joy_ball_event(pointer, current_out, len_out);
}

void pointer_deref_joy_ball_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_joy_ball_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_joy_ball_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_joy_ball_event(current_in, pointer);
}

void pointer_deref_joy_ball_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_joy_ball_event(current_in, &value);
	ptr[index] = value;
}

void new_joy_ball_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr = malloc(sizeof(SDL_JoyBallEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_joy_ball_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_JoyBallEvent *ptr = malloc(sizeof(SDL_JoyBallEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_joy_ball_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void joy_ball_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void joy_ball_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void joy_ball_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void joy_ball_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void joy_ball_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_joystick_id(&(ptr->which), current_out, len_out);
}

void joy_ball_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_joystick_id(current_in, &(ptr->which));
}

void joy_ball_event_get_ball_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->ball), current_out, len_out);
}

void joy_ball_event_set_ball_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->ball));
}

void joy_ball_event_get_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding1), current_out, len_out);
}

void joy_ball_event_set_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding1));
}

void joy_ball_event_get_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding2), current_out, len_out);
}

void joy_ball_event_set_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding2));
}

void joy_ball_event_get_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding3), current_out, len_out);
}

void joy_ball_event_set_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding3));
}

void joy_ball_event_get_xrel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_sint16(&(ptr->xrel), current_out, len_out);
}

void joy_ball_event_set_xrel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_sint16(current_in, &(ptr->xrel));
}

void joy_ball_event_get_yrel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_sint16(&(ptr->yrel), current_out, len_out);
}

void joy_ball_event_set_yrel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_sint16(current_in, &(ptr->yrel));
}

byte * read_joy_hat_event(byte *in, SDL_JoyHatEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_joystick_id(current_in, &(result->which));
	current_in = read_uint8(current_in, &(result->hat));
	current_in = read_uint8(current_in, &(result->value));
	current_in = read_uint8(current_in, &(result->padding1));
	current_in = read_uint8(current_in, &(result->padding2));

	return current_in;
}

byte * write_joy_hat_event(SDL_JoyHatEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_joystick_id(&(value->which), current_out, len);
	current_out = write_uint8(&(value->hat), current_out, len);
	current_out = write_uint8(&(value->value), current_out, len);
	current_out = write_uint8(&(value->padding1), current_out, len);
	current_out = write_uint8(&(value->padding2), current_out, len);

	return current_out;
}

byte * read_joy_hat_event_array(byte *in, SDL_JoyHatEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_joy_hat_event(current_in, &array[i]);

	return current_in;
}

byte * write_joy_hat_event_array(SDL_JoyHatEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_joy_hat_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_joy_hat_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_joy_hat_event(pointer, current_out, len_out);
}

void pointer_deref_joy_hat_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_joy_hat_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_joy_hat_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_joy_hat_event(current_in, pointer);
}

void pointer_deref_joy_hat_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_joy_hat_event(current_in, &value);
	ptr[index] = value;
}

void new_joy_hat_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr = malloc(sizeof(SDL_JoyHatEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_joy_hat_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_JoyHatEvent *ptr = malloc(sizeof(SDL_JoyHatEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_joy_hat_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void joy_hat_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void joy_hat_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void joy_hat_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void joy_hat_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void joy_hat_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_joystick_id(&(ptr->which), current_out, len_out);
}

void joy_hat_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_joystick_id(current_in, &(ptr->which));
}

void joy_hat_event_get_hat_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->hat), current_out, len_out);
}

void joy_hat_event_set_hat_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->hat));
}

void joy_hat_event_get_value_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->value), current_out, len_out);
}

void joy_hat_event_set_value_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->value));
}

void joy_hat_event_get_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding1), current_out, len_out);
}

void joy_hat_event_set_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding1));
}

void joy_hat_event_get_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding2), current_out, len_out);
}

void joy_hat_event_set_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding2));
}

byte * read_joy_button_event(byte *in, SDL_JoyButtonEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_joystick_id(current_in, &(result->which));
	current_in = read_uint8(current_in, &(result->button));
	current_in = read_uint8(current_in, &(result->state));
	current_in = read_uint8(current_in, &(result->padding1));
	current_in = read_uint8(current_in, &(result->padding2));

	return current_in;
}

byte * write_joy_button_event(SDL_JoyButtonEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_joystick_id(&(value->which), current_out, len);
	current_out = write_uint8(&(value->button), current_out, len);
	current_out = write_uint8(&(value->state), current_out, len);
	current_out = write_uint8(&(value->padding1), current_out, len);
	current_out = write_uint8(&(value->padding2), current_out, len);

	return current_out;
}

byte * read_joy_button_event_array(byte *in, SDL_JoyButtonEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_joy_button_event(current_in, &array[i]);

	return current_in;
}

byte * write_joy_button_event_array(SDL_JoyButtonEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_joy_button_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_joy_button_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_joy_button_event(pointer, current_out, len_out);
}

void pointer_deref_joy_button_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_joy_button_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_joy_button_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_joy_button_event(current_in, pointer);
}

void pointer_deref_joy_button_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_joy_button_event(current_in, &value);
	ptr[index] = value;
}

void new_joy_button_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr = malloc(sizeof(SDL_JoyButtonEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_joy_button_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_JoyButtonEvent *ptr = malloc(sizeof(SDL_JoyButtonEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_joy_button_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void joy_button_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void joy_button_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void joy_button_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void joy_button_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void joy_button_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_joystick_id(&(ptr->which), current_out, len_out);
}

void joy_button_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_joystick_id(current_in, &(ptr->which));
}

void joy_button_event_get_button_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->button), current_out, len_out);
}

void joy_button_event_set_button_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->button));
}

void joy_button_event_get_state_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->state), current_out, len_out);
}

void joy_button_event_set_state_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->state));
}

void joy_button_event_get_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding1), current_out, len_out);
}

void joy_button_event_set_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding1));
}

void joy_button_event_get_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding2), current_out, len_out);
}

void joy_button_event_set_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding2));
}

byte * read_joy_device_event(byte *in, SDL_JoyDeviceEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_sint32(current_in, &(result->which));

	return current_in;
}

byte * write_joy_device_event(SDL_JoyDeviceEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_sint32(&(value->which), current_out, len);

	return current_out;
}

byte * read_joy_device_event_array(byte *in, SDL_JoyDeviceEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_joy_device_event(current_in, &array[i]);

	return current_in;
}

byte * write_joy_device_event_array(SDL_JoyDeviceEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_joy_device_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_joy_device_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyDeviceEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_joy_device_event(pointer, current_out, len_out);
}

void pointer_deref_joy_device_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyDeviceEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_joy_device_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_joy_device_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyDeviceEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_joy_device_event(current_in, pointer);
}

void pointer_deref_joy_device_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyDeviceEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_joy_device_event(current_in, &value);
	ptr[index] = value;
}

void new_joy_device_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyDeviceEvent *ptr = malloc(sizeof(SDL_JoyDeviceEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_joy_device_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_JoyDeviceEvent *ptr = malloc(sizeof(SDL_JoyDeviceEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_joy_device_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void joy_device_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void joy_device_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void joy_device_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void joy_device_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void joy_device_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_sint32(&(ptr->which), current_out, len_out);
}

void joy_device_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_sint32(current_in, &(ptr->which));
}

byte * read_controller_axis_event(byte *in, SDL_ControllerAxisEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_joystick_id(current_in, &(result->which));
	current_in = read_uint8(current_in, &(result->axis));
	current_in = read_uint8(current_in, &(result->padding1));
	current_in = read_uint8(current_in, &(result->padding2));
	current_in = read_uint8(current_in, &(result->padding3));
	current_in = read_sint16(current_in, &(result->value));
	current_in = read_uint16(current_in, &(result->padding4));

	return current_in;
}

byte * write_controller_axis_event(SDL_ControllerAxisEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_joystick_id(&(value->which), current_out, len);
	current_out = write_uint8(&(value->axis), current_out, len);
	current_out = write_uint8(&(value->padding1), current_out, len);
	current_out = write_uint8(&(value->padding2), current_out, len);
	current_out = write_uint8(&(value->padding3), current_out, len);
	current_out = write_sint16(&(value->value), current_out, len);
	current_out = write_uint16(&(value->padding4), current_out, len);

	return current_out;
}

byte * read_controller_axis_event_array(byte *in, SDL_ControllerAxisEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_controller_axis_event(current_in, &array[i]);

	return current_in;
}

byte * write_controller_axis_event_array(SDL_ControllerAxisEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_controller_axis_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_controller_axis_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_controller_axis_event(pointer, current_out, len_out);
}

void pointer_deref_controller_axis_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_controller_axis_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_controller_axis_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_controller_axis_event(current_in, pointer);
}

void pointer_deref_controller_axis_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_controller_axis_event(current_in, &value);
	ptr[index] = value;
}

void new_controller_axis_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr = malloc(sizeof(SDL_ControllerAxisEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_controller_axis_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_ControllerAxisEvent *ptr = malloc(sizeof(SDL_ControllerAxisEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_controller_axis_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void controller_axis_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void controller_axis_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void controller_axis_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void controller_axis_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void controller_axis_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_joystick_id(&(ptr->which), current_out, len_out);
}

void controller_axis_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_joystick_id(current_in, &(ptr->which));
}

void controller_axis_event_get_axis_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->axis), current_out, len_out);
}

void controller_axis_event_set_axis_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->axis));
}

void controller_axis_event_get_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding1), current_out, len_out);
}

void controller_axis_event_set_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding1));
}

void controller_axis_event_get_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding2), current_out, len_out);
}

void controller_axis_event_set_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding2));
}

void controller_axis_event_get_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding3), current_out, len_out);
}

void controller_axis_event_set_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding3));
}

void controller_axis_event_get_value_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_sint16(&(ptr->value), current_out, len_out);
}

void controller_axis_event_set_value_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_sint16(current_in, &(ptr->value));
}

void controller_axis_event_get_padding4_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint16(&(ptr->padding4), current_out, len_out);
}

void controller_axis_event_set_padding4_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint16(current_in, &(ptr->padding4));
}

byte * read_controller_button_event(byte *in, SDL_ControllerButtonEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_joystick_id(current_in, &(result->which));
	current_in = read_uint8(current_in, &(result->button));
	current_in = read_uint8(current_in, &(result->state));
	current_in = read_uint8(current_in, &(result->padding1));
	current_in = read_uint8(current_in, &(result->padding2));

	return current_in;
}

byte * write_controller_button_event(SDL_ControllerButtonEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_joystick_id(&(value->which), current_out, len);
	current_out = write_uint8(&(value->button), current_out, len);
	current_out = write_uint8(&(value->state), current_out, len);
	current_out = write_uint8(&(value->padding1), current_out, len);
	current_out = write_uint8(&(value->padding2), current_out, len);

	return current_out;
}

byte * read_controller_button_event_array(byte *in, SDL_ControllerButtonEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_controller_button_event(current_in, &array[i]);

	return current_in;
}

byte * write_controller_button_event_array(SDL_ControllerButtonEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_controller_button_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_controller_button_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_controller_button_event(pointer, current_out, len_out);
}

void pointer_deref_controller_button_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_controller_button_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_controller_button_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_controller_button_event(current_in, pointer);
}

void pointer_deref_controller_button_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_controller_button_event(current_in, &value);
	ptr[index] = value;
}

void new_controller_button_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr = malloc(sizeof(SDL_ControllerButtonEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_controller_button_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_ControllerButtonEvent *ptr = malloc(sizeof(SDL_ControllerButtonEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_controller_button_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void controller_button_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void controller_button_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void controller_button_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void controller_button_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void controller_button_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_joystick_id(&(ptr->which), current_out, len_out);
}

void controller_button_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_joystick_id(current_in, &(ptr->which));
}

void controller_button_event_get_button_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->button), current_out, len_out);
}

void controller_button_event_set_button_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->button));
}

void controller_button_event_get_state_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->state), current_out, len_out);
}

void controller_button_event_set_state_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->state));
}

void controller_button_event_get_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding1), current_out, len_out);
}

void controller_button_event_set_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding1));
}

void controller_button_event_get_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding2), current_out, len_out);
}

void controller_button_event_set_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding2));
}

byte * read_controller_device_event(byte *in, SDL_ControllerDeviceEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_sint32(current_in, &(result->which));

	return current_in;
}

byte * write_controller_device_event(SDL_ControllerDeviceEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_sint32(&(value->which), current_out, len);

	return current_out;
}

byte * read_controller_device_event_array(byte *in, SDL_ControllerDeviceEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_controller_device_event(current_in, &array[i]);

	return current_in;
}

byte * write_controller_device_event_array(SDL_ControllerDeviceEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_controller_device_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_controller_device_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerDeviceEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_controller_device_event(pointer, current_out, len_out);
}

void pointer_deref_controller_device_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerDeviceEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_controller_device_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_controller_device_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerDeviceEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_controller_device_event(current_in, pointer);
}

void pointer_deref_controller_device_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerDeviceEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_controller_device_event(current_in, &value);
	ptr[index] = value;
}

void new_controller_device_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerDeviceEvent *ptr = malloc(sizeof(SDL_ControllerDeviceEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_controller_device_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_ControllerDeviceEvent *ptr = malloc(sizeof(SDL_ControllerDeviceEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_controller_device_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void controller_device_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void controller_device_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void controller_device_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void controller_device_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void controller_device_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_sint32(&(ptr->which), current_out, len_out);
}

void controller_device_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_sint32(current_in, &(ptr->which));
}

byte * read_audio_device_event(byte *in, SDL_AudioDeviceEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_uint32(current_in, &(result->which));
	current_in = read_uint8(current_in, &(result->iscapture));
	current_in = read_uint8(current_in, &(result->padding1));
	current_in = read_uint8(current_in, &(result->padding2));
	current_in = read_uint8(current_in, &(result->padding3));

	return current_in;
}

byte * write_audio_device_event(SDL_AudioDeviceEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_uint32(&(value->which), current_out, len);
	current_out = write_uint8(&(value->iscapture), current_out, len);
	current_out = write_uint8(&(value->padding1), current_out, len);
	current_out = write_uint8(&(value->padding2), current_out, len);
	current_out = write_uint8(&(value->padding3), current_out, len);

	return current_out;
}

byte * read_audio_device_event_array(byte *in, SDL_AudioDeviceEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_audio_device_event(current_in, &array[i]);

	return current_in;
}

byte * write_audio_device_event_array(SDL_AudioDeviceEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_audio_device_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_audio_device_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_audio_device_event(pointer, current_out, len_out);
}

void pointer_deref_audio_device_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_audio_device_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_audio_device_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_audio_device_event(current_in, pointer);
}

void pointer_deref_audio_device_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_audio_device_event(current_in, &value);
	ptr[index] = value;
}

void new_audio_device_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr = malloc(sizeof(SDL_AudioDeviceEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_audio_device_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_AudioDeviceEvent *ptr = malloc(sizeof(SDL_AudioDeviceEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_audio_device_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void audio_device_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void audio_device_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void audio_device_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void audio_device_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void audio_device_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->which), current_out, len_out);
}

void audio_device_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->which));
}

void audio_device_event_get_iscapture_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->iscapture), current_out, len_out);
}

void audio_device_event_set_iscapture_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->iscapture));
}

void audio_device_event_get_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding1), current_out, len_out);
}

void audio_device_event_set_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding1));
}

void audio_device_event_get_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding2), current_out, len_out);
}

void audio_device_event_set_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding2));
}

void audio_device_event_get_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint8(&(ptr->padding3), current_out, len_out);
}

void audio_device_event_set_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint8(current_in, &(ptr->padding3));
}

byte * read_quit_event(byte *in, SDL_QuitEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));

	return current_in;
}

byte * write_quit_event(SDL_QuitEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);

	return current_out;
}

byte * read_quit_event_array(byte *in, SDL_QuitEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_quit_event(current_in, &array[i]);

	return current_in;
}

byte * write_quit_event_array(SDL_QuitEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_quit_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_quit_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_QuitEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_quit_event(pointer, current_out, len_out);
}

void pointer_deref_quit_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_QuitEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_quit_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_quit_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_QuitEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_quit_event(current_in, pointer);
}

void pointer_deref_quit_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_QuitEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_quit_event(current_in, &value);
	ptr[index] = value;
}

void new_quit_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_QuitEvent *ptr = malloc(sizeof(SDL_QuitEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_quit_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_QuitEvent *ptr = malloc(sizeof(SDL_QuitEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_quit_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_QuitEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void quit_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_QuitEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void quit_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_QuitEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void quit_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_QuitEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void quit_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_QuitEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

byte * read_user_event(byte *in, SDL_UserEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_uint32(current_in, &(result->windowID));
	current_in = read_sint32(current_in, &(result->code));
	current_in = read_pointer(current_in, (void **) &(result->data1));
	current_in = read_pointer(current_in, (void **) &(result->data2));

	return current_in;
}

byte * write_user_event(SDL_UserEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_uint32(&(value->windowID), current_out, len);
	current_out = write_sint32(&(value->code), current_out, len);
	current_out = write_pointer((void **) &(value->data1), current_out, len);
	current_out = write_pointer((void **) &(value->data2), current_out, len);

	return current_out;
}

byte * read_user_event_array(byte *in, SDL_UserEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_user_event(current_in, &array[i]);

	return current_in;
}

byte * write_user_event_array(SDL_UserEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_user_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_user_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_user_event(pointer, current_out, len_out);
}

void pointer_deref_user_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_user_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_user_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_user_event(current_in, pointer);
}

void pointer_deref_user_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_user_event(current_in, &value);
	ptr[index] = value;
}

void new_user_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr = malloc(sizeof(SDL_UserEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_user_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_UserEvent *ptr = malloc(sizeof(SDL_UserEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_user_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void user_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void user_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void user_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void user_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void user_event_get_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->windowID), current_out, len_out);
}

void user_event_set_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->windowID));
}

void user_event_get_code_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_sint32(&(ptr->code), current_out, len_out);
}

void user_event_set_code_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_sint32(current_in, &(ptr->code));
}

void user_event_get_data1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_pointer((void **) &(ptr->data1), current_out, len_out);
}

void user_event_set_data1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_pointer(current_in, (void **) &(ptr->data1));
}

void user_event_get_data2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_pointer((void **) &(ptr->data2), current_out, len_out);
}

void user_event_set_data2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_pointer(current_in, (void **) &(ptr->data2));
}

byte * read_syswm_event(byte *in, SDL_SysWMEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_pointer(current_in, (void **) &(result->msg));

	return current_in;
}

byte * write_syswm_event(SDL_SysWMEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_pointer((void **) &(value->msg), current_out, len);

	return current_out;
}

byte * read_syswm_event_array(byte *in, SDL_SysWMEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_syswm_event(current_in, &array[i]);

	return current_in;
}

byte * write_syswm_event_array(SDL_SysWMEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_syswm_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_syswm_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_SysWMEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_syswm_event(pointer, current_out, len_out);
}

void pointer_deref_syswm_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_SysWMEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_syswm_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_syswm_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_SysWMEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_syswm_event(current_in, pointer);
}

void pointer_deref_syswm_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_SysWMEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_syswm_event(current_in, &value);
	ptr[index] = value;
}

void new_syswm_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_SysWMEvent *ptr = malloc(sizeof(SDL_SysWMEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_syswm_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_SysWMEvent *ptr = malloc(sizeof(SDL_SysWMEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_syswm_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_SysWMEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void syswm_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_SysWMEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void syswm_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_SysWMEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void syswm_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_SysWMEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void syswm_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_SysWMEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void syswm_event_get_msg_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_SysWMEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_pointer((void **) &(ptr->msg), current_out, len_out);
}

void syswm_event_set_msg_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_SysWMEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_pointer(current_in, (void **) &(ptr->msg));
}

byte * read_touch_finger_event(byte *in, SDL_TouchFingerEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_touch_id(current_in, &(result->touchId));
	current_in = read_finger_id(current_in, &(result->fingerId));
	current_in = read_float(current_in, &(result->x));
	current_in = read_float(current_in, &(result->y));
	current_in = read_float(current_in, &(result->dx));
	current_in = read_float(current_in, &(result->dy));
	current_in = read_float(current_in, &(result->pressure));

	return current_in;
}

byte * write_touch_finger_event(SDL_TouchFingerEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_touch_id(&(value->touchId), current_out, len);
	current_out = write_finger_id(&(value->fingerId), current_out, len);
	current_out = write_float(&(value->x), current_out, len);
	current_out = write_float(&(value->y), current_out, len);
	current_out = write_float(&(value->dx), current_out, len);
	current_out = write_float(&(value->dy), current_out, len);
	current_out = write_float(&(value->pressure), current_out, len);

	return current_out;
}

byte * read_touch_finger_event_array(byte *in, SDL_TouchFingerEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_touch_finger_event(current_in, &array[i]);

	return current_in;
}

byte * write_touch_finger_event_array(SDL_TouchFingerEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_touch_finger_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_touch_finger_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_touch_finger_event(pointer, current_out, len_out);
}

void pointer_deref_touch_finger_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_touch_finger_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_touch_finger_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_touch_finger_event(current_in, pointer);
}

void pointer_deref_touch_finger_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_touch_finger_event(current_in, &value);
	ptr[index] = value;
}

void new_touch_finger_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr = malloc(sizeof(SDL_TouchFingerEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_touch_finger_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_TouchFingerEvent *ptr = malloc(sizeof(SDL_TouchFingerEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_touch_finger_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void touch_finger_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void touch_finger_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void touch_finger_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void touch_finger_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void touch_finger_event_get_touchId_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_touch_id(&(ptr->touchId), current_out, len_out);
}

void touch_finger_event_set_touchId_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_touch_id(current_in, &(ptr->touchId));
}

void touch_finger_event_get_fingerId_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_finger_id(&(ptr->fingerId), current_out, len_out);
}

void touch_finger_event_set_fingerId_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_finger_id(current_in, &(ptr->fingerId));
}

void touch_finger_event_get_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_float(&(ptr->x), current_out, len_out);
}

void touch_finger_event_set_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_float(current_in, &(ptr->x));
}

void touch_finger_event_get_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_float(&(ptr->y), current_out, len_out);
}

void touch_finger_event_set_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_float(current_in, &(ptr->y));
}

void touch_finger_event_get_dx_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_float(&(ptr->dx), current_out, len_out);
}

void touch_finger_event_set_dx_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_float(current_in, &(ptr->dx));
}

void touch_finger_event_get_dy_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_float(&(ptr->dy), current_out, len_out);
}

void touch_finger_event_set_dy_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_float(current_in, &(ptr->dy));
}

void touch_finger_event_get_pressure_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_float(&(ptr->pressure), current_out, len_out);
}

void touch_finger_event_set_pressure_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_float(current_in, &(ptr->pressure));
}

byte * read_multi_gesture_event(byte *in, SDL_MultiGestureEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_touch_id(current_in, &(result->touchId));
	current_in = read_float(current_in, &(result->dTheta));
	current_in = read_float(current_in, &(result->dDist));
	current_in = read_float(current_in, &(result->x));
	current_in = read_float(current_in, &(result->y));
	current_in = read_uint16(current_in, &(result->numFingers));
	current_in = read_uint16(current_in, &(result->padding));

	return current_in;
}

byte * write_multi_gesture_event(SDL_MultiGestureEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_touch_id(&(value->touchId), current_out, len);
	current_out = write_float(&(value->dTheta), current_out, len);
	current_out = write_float(&(value->dDist), current_out, len);
	current_out = write_float(&(value->x), current_out, len);
	current_out = write_float(&(value->y), current_out, len);
	current_out = write_uint16(&(value->numFingers), current_out, len);
	current_out = write_uint16(&(value->padding), current_out, len);

	return current_out;
}

byte * read_multi_gesture_event_array(byte *in, SDL_MultiGestureEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_multi_gesture_event(current_in, &array[i]);

	return current_in;
}

byte * write_multi_gesture_event_array(SDL_MultiGestureEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_multi_gesture_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_multi_gesture_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_multi_gesture_event(pointer, current_out, len_out);
}

void pointer_deref_multi_gesture_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_multi_gesture_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_multi_gesture_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_multi_gesture_event(current_in, pointer);
}

void pointer_deref_multi_gesture_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_multi_gesture_event(current_in, &value);
	ptr[index] = value;
}

void new_multi_gesture_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr = malloc(sizeof(SDL_MultiGestureEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_multi_gesture_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_MultiGestureEvent *ptr = malloc(sizeof(SDL_MultiGestureEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_multi_gesture_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void multi_gesture_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void multi_gesture_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void multi_gesture_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void multi_gesture_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void multi_gesture_event_get_touchId_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_touch_id(&(ptr->touchId), current_out, len_out);
}

void multi_gesture_event_set_touchId_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_touch_id(current_in, &(ptr->touchId));
}

void multi_gesture_event_get_dTheta_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_float(&(ptr->dTheta), current_out, len_out);
}

void multi_gesture_event_set_dTheta_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_float(current_in, &(ptr->dTheta));
}

void multi_gesture_event_get_dDist_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_float(&(ptr->dDist), current_out, len_out);
}

void multi_gesture_event_set_dDist_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_float(current_in, &(ptr->dDist));
}

void multi_gesture_event_get_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_float(&(ptr->x), current_out, len_out);
}

void multi_gesture_event_set_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_float(current_in, &(ptr->x));
}

void multi_gesture_event_get_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_float(&(ptr->y), current_out, len_out);
}

void multi_gesture_event_set_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_float(current_in, &(ptr->y));
}

void multi_gesture_event_get_numFingers_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint16(&(ptr->numFingers), current_out, len_out);
}

void multi_gesture_event_set_numFingers_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint16(current_in, &(ptr->numFingers));
}

void multi_gesture_event_get_padding_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint16(&(ptr->padding), current_out, len_out);
}

void multi_gesture_event_set_padding_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint16(current_in, &(ptr->padding));
}

byte * read_dollar_gesture_event(byte *in, SDL_DollarGestureEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_touch_id(current_in, &(result->touchId));
	current_in = read_gesture_id(current_in, &(result->gestureId));
	current_in = read_uint32(current_in, &(result->numFingers));
	current_in = read_float(current_in, &(result->error));
	current_in = read_float(current_in, &(result->x));
	current_in = read_float(current_in, &(result->y));

	return current_in;
}

byte * write_dollar_gesture_event(SDL_DollarGestureEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_touch_id(&(value->touchId), current_out, len);
	current_out = write_gesture_id(&(value->gestureId), current_out, len);
	current_out = write_uint32(&(value->numFingers), current_out, len);
	current_out = write_float(&(value->error), current_out, len);
	current_out = write_float(&(value->x), current_out, len);
	current_out = write_float(&(value->y), current_out, len);

	return current_out;
}

byte * read_dollar_gesture_event_array(byte *in, SDL_DollarGestureEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_dollar_gesture_event(current_in, &array[i]);

	return current_in;
}

byte * write_dollar_gesture_event_array(SDL_DollarGestureEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_dollar_gesture_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_dollar_gesture_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_dollar_gesture_event(pointer, current_out, len_out);
}

void pointer_deref_dollar_gesture_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_dollar_gesture_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_dollar_gesture_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_dollar_gesture_event(current_in, pointer);
}

void pointer_deref_dollar_gesture_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_dollar_gesture_event(current_in, &value);
	ptr[index] = value;
}

void new_dollar_gesture_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr = malloc(sizeof(SDL_DollarGestureEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_dollar_gesture_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_DollarGestureEvent *ptr = malloc(sizeof(SDL_DollarGestureEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_dollar_gesture_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void dollar_gesture_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void dollar_gesture_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void dollar_gesture_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void dollar_gesture_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void dollar_gesture_event_get_touchId_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_touch_id(&(ptr->touchId), current_out, len_out);
}

void dollar_gesture_event_set_touchId_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_touch_id(current_in, &(ptr->touchId));
}

void dollar_gesture_event_get_gestureId_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_gesture_id(&(ptr->gestureId), current_out, len_out);
}

void dollar_gesture_event_set_gestureId_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_gesture_id(current_in, &(ptr->gestureId));
}

void dollar_gesture_event_get_numFingers_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->numFingers), current_out, len_out);
}

void dollar_gesture_event_set_numFingers_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->numFingers));
}

void dollar_gesture_event_get_error_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_float(&(ptr->error), current_out, len_out);
}

void dollar_gesture_event_set_error_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_float(current_in, &(ptr->error));
}

void dollar_gesture_event_get_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_float(&(ptr->x), current_out, len_out);
}

void dollar_gesture_event_set_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_float(current_in, &(ptr->x));
}

void dollar_gesture_event_get_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_float(&(ptr->y), current_out, len_out);
}

void dollar_gesture_event_set_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_float(current_in, &(ptr->y));
}

byte * read_drop_event(byte *in, SDL_DropEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_string(current_in, (string *) result->file);
	current_in = read_uint32(current_in, &(result->windowID));

	return current_in;
}

byte * write_drop_event(SDL_DropEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_string((string *) value->file, current_out, len);
	current_out = write_uint32(&(value->windowID), current_out, len);

	return current_out;
}

byte * read_drop_event_array(byte *in, SDL_DropEvent *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_drop_event(current_in, &array[i]);

	return current_in;
}

byte * write_drop_event_array(SDL_DropEvent *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_drop_event(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_drop_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_drop_event(pointer, current_out, len_out);
}

void pointer_deref_drop_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_drop_event(&(ptr[index]), current_out, len_out);
}

void pointer_deref_drop_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_drop_event(current_in, pointer);
}

void pointer_deref_drop_event_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_drop_event(current_in, &value);
	ptr[index] = value;
}

void new_drop_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *ptr = malloc(sizeof(SDL_DropEvent));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_drop_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_DropEvent *ptr = malloc(sizeof(SDL_DropEvent)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_drop_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void drop_event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void drop_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void drop_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->timestamp), current_out, len_out);
}

void drop_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->timestamp));
}

void drop_event_get_file_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_string((string *) ptr->file, current_out, len_out);
}

void drop_event_set_file_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_string(current_in, (string *) ptr->file);
}

void drop_event_get_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->windowID), current_out, len_out);
}

void drop_event_set_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->windowID));
}

byte * read_event(byte *in, SDL_Event *result) {
	return read_pointer(in, (void **) &result);;
}

byte * write_event(SDL_Event *value, byte *out, size_t *len) {
	return write_pointer((void **) &value, out, len);;
}

byte * read_event_array(byte *in, SDL_Event **array, int n) {
	return read_pointer_array(in, (void **) array, n);
}

byte * write_event_array(SDL_Event **array, byte *out, size_t *len, int n) {
	return write_pointer_array((void **) array, out, len, n);
}

void new_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr = malloc(sizeof(SDL_Event));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_event_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	SDL_Event *ptr = malloc(sizeof(SDL_Event)*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

void event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_uint32(&(ptr->type), current_out, len_out);
}

void event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_uint32(current_in, &(ptr->type));
}

void event_get_common_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_common_event(&(ptr->common), current_out, len_out);
}

void event_set_common_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_common_event(current_in, &(ptr->common));
}

void event_get_window_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_window_event(&(ptr->window), current_out, len_out);
}

void event_set_window_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_window_event(current_in, &(ptr->window));
}

void event_get_key_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_keyboard_event(&(ptr->key), current_out, len_out);
}

void event_set_key_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_keyboard_event(current_in, &(ptr->key));
}

void event_get_edit_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_text_editing_event(&(ptr->edit), current_out, len_out);
}

void event_set_edit_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_text_editing_event(current_in, &(ptr->edit));
}

void event_get_text_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_text_input_event(&(ptr->text), current_out, len_out);
}

void event_set_text_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_text_input_event(current_in, &(ptr->text));
}

void event_get_motion_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_mouse_motion_event(&(ptr->motion), current_out, len_out);
}

void event_set_motion_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_mouse_motion_event(current_in, &(ptr->motion));
}

void event_get_button_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_mouse_button_event(&(ptr->button), current_out, len_out);
}

void event_set_button_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_mouse_button_event(current_in, &(ptr->button));
}

void event_get_wheel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_mouse_wheel_event(&(ptr->wheel), current_out, len_out);
}

void event_set_wheel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_mouse_wheel_event(current_in, &(ptr->wheel));
}

void event_get_jaxis_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_joy_axis_event(&(ptr->jaxis), current_out, len_out);
}

void event_set_jaxis_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_joy_axis_event(current_in, &(ptr->jaxis));
}

void event_get_jball_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_joy_ball_event(&(ptr->jball), current_out, len_out);
}

void event_set_jball_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_joy_ball_event(current_in, &(ptr->jball));
}

void event_get_jhat_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_joy_hat_event(&(ptr->jhat), current_out, len_out);
}

void event_set_jhat_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_joy_hat_event(current_in, &(ptr->jhat));
}

void event_get_jbutton_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_joy_button_event(&(ptr->jbutton), current_out, len_out);
}

void event_set_jbutton_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_joy_button_event(current_in, &(ptr->jbutton));
}

void event_get_jdevice_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_joy_device_event(&(ptr->jdevice), current_out, len_out);
}

void event_set_jdevice_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_joy_device_event(current_in, &(ptr->jdevice));
}

void event_get_caxis_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_controller_axis_event(&(ptr->caxis), current_out, len_out);
}

void event_set_caxis_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_controller_axis_event(current_in, &(ptr->caxis));
}

void event_get_cbutton_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_controller_button_event(&(ptr->cbutton), current_out, len_out);
}

void event_set_cbutton_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_controller_button_event(current_in, &(ptr->cbutton));
}

void event_get_cdevice_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_controller_device_event(&(ptr->cdevice), current_out, len_out);
}

void event_set_cdevice_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_controller_device_event(current_in, &(ptr->cdevice));
}

void event_get_adevice_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_audio_device_event(&(ptr->adevice), current_out, len_out);
}

void event_set_adevice_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_audio_device_event(current_in, &(ptr->adevice));
}

void event_get_quit_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_quit_event(&(ptr->quit), current_out, len_out);
}

void event_set_quit_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_quit_event(current_in, &(ptr->quit));
}

void event_get_user_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_user_event(&(ptr->user), current_out, len_out);
}

void event_set_user_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_user_event(current_in, &(ptr->user));
}

void event_get_syswm_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_syswm_event(&(ptr->syswm), current_out, len_out);
}

void event_set_syswm_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_syswm_event(current_in, &(ptr->syswm));
}

void event_get_tfinger_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_touch_finger_event(&(ptr->tfinger), current_out, len_out);
}

void event_set_tfinger_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_touch_finger_event(current_in, &(ptr->tfinger));
}

void event_get_mgesture_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_multi_gesture_event(&(ptr->mgesture), current_out, len_out);
}

void event_set_mgesture_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_multi_gesture_event(current_in, &(ptr->mgesture));
}

void event_get_dgesture_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_dollar_gesture_event(&(ptr->dgesture), current_out, len_out);
}

void event_set_dgesture_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_dollar_gesture_event(current_in, &(ptr->dgesture));
}

void event_get_drop_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_drop_event(&(ptr->drop), current_out, len_out);
}

void event_set_drop_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_drop_event(current_in, &(ptr->drop));
}

//--------------------------------------------------------

void init_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	Uint32 var1;
	current_in = read_uint32(current_in, &var1);

	int retvar = SDL_Init(var1);
	current_out = write_int(&retvar, current_out, len_out);
}

void quit_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Quit();
}

void create_window_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	string var1;
	current_in = read_string(current_in, &var1);
	int var2;
	current_in = read_int(current_in, &var2);
	int var3;
	current_in = read_int(current_in, &var3);
	int var4;
	current_in = read_int(current_in, &var4);
	int var5;
	current_in = read_int(current_in, &var5);
	Uint32 var6;
	current_in = read_uint32(current_in, &var6);

	SDL_Window *retvar = SDL_CreateWindow(var1, var2, var3, var4, var5, var6);
	current_out = write_pointer((void **) &retvar, current_out, len_out);
}

void get_window_surface_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Window *var1;
	current_in = read_pointer(current_in, (void **) &var1);

	SDL_Surface *retvar = SDL_GetWindowSurface(var1);
	current_out = write_pointer((void **) &retvar, current_out, len_out);
}

void load_bmp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	string var1;
	current_in = read_string(current_in, &var1);

	SDL_Surface *retvar = SDL_LoadBMP(var1);
	current_out = write_pointer((void **) &retvar, current_out, len_out);
}

void free_surface_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *var1;
	current_in = read_pointer(current_in, (void **) &var1);

	SDL_FreeSurface(var1);
}

void blit_surface_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *var1;
	current_in = read_pointer(current_in, (void **) &var1);
	SDL_Rect *var2;
	current_in = read_pointer(current_in, (void **) &var2);
	SDL_Surface *var3;
	current_in = read_pointer(current_in, (void **) &var3);
	SDL_Rect *var4;
	current_in = read_pointer(current_in, (void **) &var4);

	int retvar = SDL_BlitSurface(var1, var2, var3, var4);
	current_out = write_int(&retvar, current_out, len_out);
}

void blit_scaled_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *var1;
	current_in = read_pointer(current_in, (void **) &var1);
	SDL_Rect *var2;
	current_in = read_pointer(current_in, (void **) &var2);
	SDL_Surface *var3;
	current_in = read_pointer(current_in, (void **) &var3);
	SDL_Rect *var4;
	current_in = read_pointer(current_in, (void **) &var4);

	int retvar = SDL_BlitScaled(var1, var2, var3, var4);
	current_out = write_int(&retvar, current_out, len_out);
}

void update_window_surface_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Window *var1;
	current_in = read_pointer(current_in, (void **) &var1);

	int retvar = SDL_UpdateWindowSurface(var1);
	current_out = write_int(&retvar, current_out, len_out);
}

void destroy_window_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Window *var1;
	current_in = read_pointer(current_in, (void **) &var1);

	SDL_DestroyWindow(var1);
}

void get_window_size_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Window *var1;
	current_in = read_pointer(current_in, (void **) &var1);
	int *var2 = malloc(sizeof(int));
	int *var3 = malloc(sizeof(int));

	SDL_GetWindowSize(var1, var2, var3);
	current_out = write_int(var2, current_out, len_out);
	free(var2);
	current_out = write_int(var3, current_out, len_out);
	free(var3);
}

void get_error_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	string retvar;
	strcpy((char *)retvar, SDL_GetError());
	current_out = write_string(&retvar, current_out, len_out);
}

void poll_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *var1 = malloc(sizeof(SDL_Event));

	int retvar = SDL_PollEvent(var1);
	current_out = write_int(&retvar, current_out, len_out);
	current_out = write_event(var1, current_out, len_out);
	free(var1);
}

void maxint_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int *var1;
	current_in = read_pointer(current_in, (void **) &var1);
	int var2;
	current_in = read_int(current_in, &var2);

	int retvar = max(var1, var2);
	current_out = write_int(&retvar, current_out, len_out);
}

handler handlers[] = {
	0,
	pointer_deref_int8_Handler,		//1
	pointer_deref_int8_array_Handler,		//2
	pointer_deref_int8_assign_Handler,		//3
	pointer_deref_int8_array_assign_Handler,		//4
	new_int8_Handler,		//5
	new_int8_array_Handler,		//6
	delete_int8_Handler,		//7
	pointer_deref_int16_Handler,		//8
	pointer_deref_int16_array_Handler,		//9
	pointer_deref_int16_assign_Handler,		//10
	pointer_deref_int16_array_assign_Handler,		//11
	new_int16_Handler,		//12
	new_int16_array_Handler,		//13
	delete_int16_Handler,		//14
	pointer_deref_int32_Handler,		//15
	pointer_deref_int32_array_Handler,		//16
	pointer_deref_int32_assign_Handler,		//17
	pointer_deref_int32_array_assign_Handler,		//18
	new_int32_Handler,		//19
	new_int32_array_Handler,		//20
	delete_int32_Handler,		//21
	pointer_deref_int64_Handler,		//22
	pointer_deref_int64_array_Handler,		//23
	pointer_deref_int64_assign_Handler,		//24
	pointer_deref_int64_array_assign_Handler,		//25
	new_int64_Handler,		//26
	new_int64_array_Handler,		//27
	delete_int64_Handler,		//28
	pointer_deref_float_Handler,		//29
	pointer_deref_float_array_Handler,		//30
	pointer_deref_float_assign_Handler,		//31
	pointer_deref_float_array_assign_Handler,		//32
	new_float_Handler,		//33
	new_float_array_Handler,		//34
	delete_float_Handler,		//35
	pointer_deref_double_Handler,		//36
	pointer_deref_double_array_Handler,		//37
	pointer_deref_double_assign_Handler,		//38
	pointer_deref_double_array_assign_Handler,		//39
	new_double_Handler,		//40
	new_double_array_Handler,		//41
	delete_double_Handler,		//42
	pointer_deref_string_Handler,		//43
	pointer_deref_string_array_Handler,		//44
	pointer_deref_string_assign_Handler,		//45
	pointer_deref_string_array_assign_Handler,		//46
	new_string_Handler,		//47
	new_string_array_Handler,		//48
	delete_string_Handler,		//49
	pointer_deref_pointer_Handler,		//50
	pointer_deref_pointer_array_Handler,		//51
	pointer_deref_pointer_assign_Handler,		//52
	pointer_deref_pointer_array_assign_Handler,		//53
	new_pointer_Handler,		//54
	new_pointer_array_Handler,		//55
	delete_pointer_Handler,		//56
	pointer_deref_arrayA_Handler,		//57
	pointer_deref_arrayA_array_Handler,		//58
	pointer_deref_arrayA_assign_Handler,		//59
	pointer_deref_arrayA_array_assign_Handler,		//60
	new_arrayA_Handler,		//61
	new_arrayA_array_Handler,		//62
	delete_arrayA_Handler,		//63
	arrayA_get_id_Handler,		//64
	arrayA_set_id_Handler,		//65
	arrayA_get_values_Handler,		//66
	arrayA_set_values_Handler,		//67
	pointer_deref_arrayB_Handler,		//68
	pointer_deref_arrayB_array_Handler,		//69
	pointer_deref_arrayB_assign_Handler,		//70
	pointer_deref_arrayB_array_assign_Handler,		//71
	new_arrayB_Handler,		//72
	new_arrayB_array_Handler,		//73
	delete_arrayB_Handler,		//74
	arrayB_get_id_Handler,		//75
	arrayB_set_id_Handler,		//76
	arrayB_get_values_Handler,		//77
	arrayB_set_values_Handler,		//78
	pointer_deref_arrayC_Handler,		//79
	pointer_deref_arrayC_array_Handler,		//80
	pointer_deref_arrayC_assign_Handler,		//81
	pointer_deref_arrayC_array_assign_Handler,		//82
	new_arrayC_Handler,		//83
	new_arrayC_array_Handler,		//84
	delete_arrayC_Handler,		//85
	arrayC_get_id_Handler,		//86
	arrayC_set_id_Handler,		//87
	arrayC_get_values_Handler,		//88
	arrayC_set_values_Handler,		//89
	arrayC_get_size_Handler,		//90
	arrayC_set_size_Handler,		//91
	pointer_deref_color_Handler,		//92
	pointer_deref_color_array_Handler,		//93
	pointer_deref_color_assign_Handler,		//94
	pointer_deref_color_array_assign_Handler,		//95
	new_color_Handler,		//96
	new_color_array_Handler,		//97
	delete_color_Handler,		//98
	color_get_r_Handler,		//99
	color_set_r_Handler,		//100
	color_get_g_Handler,		//101
	color_set_g_Handler,		//102
	color_get_b_Handler,		//103
	color_set_b_Handler,		//104
	color_get_a_Handler,		//105
	color_set_a_Handler,		//106
	pointer_deref_palette_Handler,		//107
	pointer_deref_palette_array_Handler,		//108
	pointer_deref_palette_assign_Handler,		//109
	pointer_deref_palette_array_assign_Handler,		//110
	new_palette_Handler,		//111
	new_palette_array_Handler,		//112
	delete_palette_Handler,		//113
	palette_get_ncolors_Handler,		//114
	palette_set_ncolors_Handler,		//115
	palette_get_colors_Handler,		//116
	palette_set_colors_Handler,		//117
	palette_get_version_Handler,		//118
	palette_set_version_Handler,		//119
	palette_get_refcount_Handler,		//120
	palette_set_refcount_Handler,		//121
	pointer_deref_pixel_format_Handler,		//122
	pointer_deref_pixel_format_array_Handler,		//123
	pointer_deref_pixel_format_assign_Handler,		//124
	pointer_deref_pixel_format_array_assign_Handler,		//125
	new_pixel_format_Handler,		//126
	new_pixel_format_array_Handler,		//127
	delete_pixel_format_Handler,		//128
	pixel_format_get_format_Handler,		//129
	pixel_format_set_format_Handler,		//130
	pixel_format_get_palette_Handler,		//131
	pixel_format_set_palette_Handler,		//132
	pixel_format_get_bits_per_pixel_Handler,		//133
	pixel_format_set_bits_per_pixel_Handler,		//134
	pixel_format_get_bytes_per_pixel_Handler,		//135
	pixel_format_set_bytes_per_pixel_Handler,		//136
	pixel_format_get_r_mask_Handler,		//137
	pixel_format_set_r_mask_Handler,		//138
	pixel_format_get_g_mask_Handler,		//139
	pixel_format_set_g_mask_Handler,		//140
	pixel_format_get_b_mask_Handler,		//141
	pixel_format_set_b_mask_Handler,		//142
	pixel_format_get_a_mask_Handler,		//143
	pixel_format_set_a_mask_Handler,		//144
	pixel_format_get_r_loss_Handler,		//145
	pixel_format_set_r_loss_Handler,		//146
	pixel_format_get_g_loss_Handler,		//147
	pixel_format_set_g_loss_Handler,		//148
	pixel_format_get_b_loss_Handler,		//149
	pixel_format_set_b_loss_Handler,		//150
	pixel_format_get_a_loss_Handler,		//151
	pixel_format_set_a_loss_Handler,		//152
	pixel_format_get_r_shift_Handler,		//153
	pixel_format_set_r_shift_Handler,		//154
	pixel_format_get_g_shift_Handler,		//155
	pixel_format_set_g_shift_Handler,		//156
	pixel_format_get_b_shift_Handler,		//157
	pixel_format_set_b_shift_Handler,		//158
	pixel_format_get_a_shift_Handler,		//159
	pixel_format_set_a_shift_Handler,		//160
	pixel_format_get_refcount_Handler,		//161
	pixel_format_set_refcount_Handler,		//162
	pixel_format_get_next_Handler,		//163
	pixel_format_set_next_Handler,		//164
	pointer_deref_rect_Handler,		//165
	pointer_deref_rect_array_Handler,		//166
	pointer_deref_rect_assign_Handler,		//167
	pointer_deref_rect_array_assign_Handler,		//168
	new_rect_Handler,		//169
	new_rect_array_Handler,		//170
	delete_rect_Handler,		//171
	rect_get_x_Handler,		//172
	rect_set_x_Handler,		//173
	rect_get_y_Handler,		//174
	rect_set_y_Handler,		//175
	rect_get_w_Handler,		//176
	rect_set_w_Handler,		//177
	rect_get_h_Handler,		//178
	rect_set_h_Handler,		//179
	pointer_deref_surface_Handler,		//180
	pointer_deref_surface_array_Handler,		//181
	pointer_deref_surface_assign_Handler,		//182
	pointer_deref_surface_array_assign_Handler,		//183
	new_surface_Handler,		//184
	new_surface_array_Handler,		//185
	delete_surface_Handler,		//186
	surface_get_flags_Handler,		//187
	surface_set_flags_Handler,		//188
	surface_get_format_Handler,		//189
	surface_set_format_Handler,		//190
	surface_get_w_Handler,		//191
	surface_set_w_Handler,		//192
	surface_get_h_Handler,		//193
	surface_set_h_Handler,		//194
	surface_get_pitch_Handler,		//195
	surface_set_pitch_Handler,		//196
	surface_get_pixels_Handler,		//197
	surface_set_pixels_Handler,		//198
	surface_get_userdata_Handler,		//199
	surface_set_userdata_Handler,		//200
	surface_get_locked_Handler,		//201
	surface_set_locked_Handler,		//202
	surface_get_lock_data_Handler,		//203
	surface_set_lock_data_Handler,		//204
	surface_get_clip_rect_Handler,		//205
	surface_set_clip_rect_Handler,		//206
	surface_get_map_Handler,		//207
	surface_set_map_Handler,		//208
	surface_get_refcount_Handler,		//209
	surface_set_refcount_Handler,		//210
	pointer_deref_keysym_Handler,		//211
	pointer_deref_keysym_array_Handler,		//212
	pointer_deref_keysym_assign_Handler,		//213
	pointer_deref_keysym_array_assign_Handler,		//214
	new_keysym_Handler,		//215
	new_keysym_array_Handler,		//216
	delete_keysym_Handler,		//217
	keysym_get_scancode_Handler,		//218
	keysym_set_scancode_Handler,		//219
	keysym_get_sym_Handler,		//220
	keysym_set_sym_Handler,		//221
	keysym_get_mod_Handler,		//222
	keysym_set_mod_Handler,		//223
	keysym_get_unused_Handler,		//224
	keysym_set_unused_Handler,		//225
	pointer_deref_common_event_Handler,		//226
	pointer_deref_common_event_array_Handler,		//227
	pointer_deref_common_event_assign_Handler,		//228
	pointer_deref_common_event_array_assign_Handler,		//229
	new_common_event_Handler,		//230
	new_common_event_array_Handler,		//231
	delete_common_event_Handler,		//232
	common_event_get_type_Handler,		//233
	common_event_set_type_Handler,		//234
	common_event_get_timestamp_Handler,		//235
	common_event_set_timestamp_Handler,		//236
	pointer_deref_window_event_Handler,		//237
	pointer_deref_window_event_array_Handler,		//238
	pointer_deref_window_event_assign_Handler,		//239
	pointer_deref_window_event_array_assign_Handler,		//240
	new_window_event_Handler,		//241
	new_window_event_array_Handler,		//242
	delete_window_event_Handler,		//243
	window_event_get_type_Handler,		//244
	window_event_set_type_Handler,		//245
	window_event_get_timestamp_Handler,		//246
	window_event_set_timestamp_Handler,		//247
	window_event_get_windowID_Handler,		//248
	window_event_set_windowID_Handler,		//249
	window_event_get_event_Handler,		//250
	window_event_set_event_Handler,		//251
	window_event_get_padding1_Handler,		//252
	window_event_set_padding1_Handler,		//253
	window_event_get_padding2_Handler,		//254
	window_event_set_padding2_Handler,		//255
	window_event_get_padding3_Handler,		//256
	window_event_set_padding3_Handler,		//257
	window_event_get_data1_Handler,		//258
	window_event_set_data1_Handler,		//259
	window_event_get_data2_Handler,		//260
	window_event_set_data2_Handler,		//261
	pointer_deref_keyboard_event_Handler,		//262
	pointer_deref_keyboard_event_array_Handler,		//263
	pointer_deref_keyboard_event_assign_Handler,		//264
	pointer_deref_keyboard_event_array_assign_Handler,		//265
	new_keyboard_event_Handler,		//266
	new_keyboard_event_array_Handler,		//267
	delete_keyboard_event_Handler,		//268
	keyboard_event_get_type_Handler,		//269
	keyboard_event_set_type_Handler,		//270
	keyboard_event_get_timestamp_Handler,		//271
	keyboard_event_set_timestamp_Handler,		//272
	keyboard_event_get_windowID_Handler,		//273
	keyboard_event_set_windowID_Handler,		//274
	keyboard_event_get_state_Handler,		//275
	keyboard_event_set_state_Handler,		//276
	keyboard_event_get_repeat_Handler,		//277
	keyboard_event_set_repeat_Handler,		//278
	keyboard_event_get_padding2_Handler,		//279
	keyboard_event_set_padding2_Handler,		//280
	keyboard_event_get_padding3_Handler,		//281
	keyboard_event_set_padding3_Handler,		//282
	keyboard_event_get_keysym_Handler,		//283
	keyboard_event_set_keysym_Handler,		//284
	pointer_deref_text_editing_event_Handler,		//285
	pointer_deref_text_editing_event_array_Handler,		//286
	pointer_deref_text_editing_event_assign_Handler,		//287
	pointer_deref_text_editing_event_array_assign_Handler,		//288
	new_text_editing_event_Handler,		//289
	new_text_editing_event_array_Handler,		//290
	delete_text_editing_event_Handler,		//291
	text_editing_event_get_type_Handler,		//292
	text_editing_event_set_type_Handler,		//293
	text_editing_event_get_timestamp_Handler,		//294
	text_editing_event_set_timestamp_Handler,		//295
	text_editing_event_get_windowID_Handler,		//296
	text_editing_event_set_windowID_Handler,		//297
	text_editing_event_get_text_Handler,		//298
	text_editing_event_set_text_Handler,		//299
	text_editing_event_get_start_Handler,		//300
	text_editing_event_set_start_Handler,		//301
	text_editing_event_get_length_Handler,		//302
	text_editing_event_set_length_Handler,		//303
	pointer_deref_text_input_event_Handler,		//304
	pointer_deref_text_input_event_array_Handler,		//305
	pointer_deref_text_input_event_assign_Handler,		//306
	pointer_deref_text_input_event_array_assign_Handler,		//307
	new_text_input_event_Handler,		//308
	new_text_input_event_array_Handler,		//309
	delete_text_input_event_Handler,		//310
	text_input_event_get_type_Handler,		//311
	text_input_event_set_type_Handler,		//312
	text_input_event_get_timestamp_Handler,		//313
	text_input_event_set_timestamp_Handler,		//314
	text_input_event_get_windowID_Handler,		//315
	text_input_event_set_windowID_Handler,		//316
	text_input_event_get_text_Handler,		//317
	text_input_event_set_text_Handler,		//318
	pointer_deref_mouse_motion_event_Handler,		//319
	pointer_deref_mouse_motion_event_array_Handler,		//320
	pointer_deref_mouse_motion_event_assign_Handler,		//321
	pointer_deref_mouse_motion_event_array_assign_Handler,		//322
	new_mouse_motion_event_Handler,		//323
	new_mouse_motion_event_array_Handler,		//324
	delete_mouse_motion_event_Handler,		//325
	mouse_motion_event_get_type_Handler,		//326
	mouse_motion_event_set_type_Handler,		//327
	mouse_motion_event_get_timestamp_Handler,		//328
	mouse_motion_event_set_timestamp_Handler,		//329
	mouse_motion_event_get_windowID_Handler,		//330
	mouse_motion_event_set_windowID_Handler,		//331
	mouse_motion_event_get_which_Handler,		//332
	mouse_motion_event_set_which_Handler,		//333
	mouse_motion_event_get_state_Handler,		//334
	mouse_motion_event_set_state_Handler,		//335
	mouse_motion_event_get_x_Handler,		//336
	mouse_motion_event_set_x_Handler,		//337
	mouse_motion_event_get_y_Handler,		//338
	mouse_motion_event_set_y_Handler,		//339
	mouse_motion_event_get_xrel_Handler,		//340
	mouse_motion_event_set_xrel_Handler,		//341
	mouse_motion_event_get_yrel_Handler,		//342
	mouse_motion_event_set_yrel_Handler,		//343
	pointer_deref_mouse_button_event_Handler,		//344
	pointer_deref_mouse_button_event_array_Handler,		//345
	pointer_deref_mouse_button_event_assign_Handler,		//346
	pointer_deref_mouse_button_event_array_assign_Handler,		//347
	new_mouse_button_event_Handler,		//348
	new_mouse_button_event_array_Handler,		//349
	delete_mouse_button_event_Handler,		//350
	mouse_button_event_get_type_Handler,		//351
	mouse_button_event_set_type_Handler,		//352
	mouse_button_event_get_timestamp_Handler,		//353
	mouse_button_event_set_timestamp_Handler,		//354
	mouse_button_event_get_windowID_Handler,		//355
	mouse_button_event_set_windowID_Handler,		//356
	mouse_button_event_get_which_Handler,		//357
	mouse_button_event_set_which_Handler,		//358
	mouse_button_event_get_button_Handler,		//359
	mouse_button_event_set_button_Handler,		//360
	mouse_button_event_get_state_Handler,		//361
	mouse_button_event_set_state_Handler,		//362
	mouse_button_event_get_clicks_Handler,		//363
	mouse_button_event_set_clicks_Handler,		//364
	mouse_button_event_get_x_Handler,		//365
	mouse_button_event_set_x_Handler,		//366
	mouse_button_event_get_y_Handler,		//367
	mouse_button_event_set_y_Handler,		//368
	pointer_deref_mouse_wheel_event_Handler,		//369
	pointer_deref_mouse_wheel_event_array_Handler,		//370
	pointer_deref_mouse_wheel_event_assign_Handler,		//371
	pointer_deref_mouse_wheel_event_array_assign_Handler,		//372
	new_mouse_wheel_event_Handler,		//373
	new_mouse_wheel_event_array_Handler,		//374
	delete_mouse_wheel_event_Handler,		//375
	mouse_wheel_event_get_type_Handler,		//376
	mouse_wheel_event_set_type_Handler,		//377
	mouse_wheel_event_get_timestamp_Handler,		//378
	mouse_wheel_event_set_timestamp_Handler,		//379
	mouse_wheel_event_get_windowID_Handler,		//380
	mouse_wheel_event_set_windowID_Handler,		//381
	mouse_wheel_event_get_which_Handler,		//382
	mouse_wheel_event_set_which_Handler,		//383
	mouse_wheel_event_get_x_Handler,		//384
	mouse_wheel_event_set_x_Handler,		//385
	mouse_wheel_event_get_y_Handler,		//386
	mouse_wheel_event_set_y_Handler,		//387
	mouse_wheel_event_get_direction_Handler,		//388
	mouse_wheel_event_set_direction_Handler,		//389
	pointer_deref_joy_axis_event_Handler,		//390
	pointer_deref_joy_axis_event_array_Handler,		//391
	pointer_deref_joy_axis_event_assign_Handler,		//392
	pointer_deref_joy_axis_event_array_assign_Handler,		//393
	new_joy_axis_event_Handler,		//394
	new_joy_axis_event_array_Handler,		//395
	delete_joy_axis_event_Handler,		//396
	joy_axis_event_get_type_Handler,		//397
	joy_axis_event_set_type_Handler,		//398
	joy_axis_event_get_timestamp_Handler,		//399
	joy_axis_event_set_timestamp_Handler,		//400
	joy_axis_event_get_which_Handler,		//401
	joy_axis_event_set_which_Handler,		//402
	joy_axis_event_get_axis_Handler,		//403
	joy_axis_event_set_axis_Handler,		//404
	joy_axis_event_get_padding1_Handler,		//405
	joy_axis_event_set_padding1_Handler,		//406
	joy_axis_event_get_padding2_Handler,		//407
	joy_axis_event_set_padding2_Handler,		//408
	joy_axis_event_get_padding3_Handler,		//409
	joy_axis_event_set_padding3_Handler,		//410
	joy_axis_event_get_value_Handler,		//411
	joy_axis_event_set_value_Handler,		//412
	pointer_deref_joy_ball_event_Handler,		//413
	pointer_deref_joy_ball_event_array_Handler,		//414
	pointer_deref_joy_ball_event_assign_Handler,		//415
	pointer_deref_joy_ball_event_array_assign_Handler,		//416
	new_joy_ball_event_Handler,		//417
	new_joy_ball_event_array_Handler,		//418
	delete_joy_ball_event_Handler,		//419
	joy_ball_event_get_type_Handler,		//420
	joy_ball_event_set_type_Handler,		//421
	joy_ball_event_get_timestamp_Handler,		//422
	joy_ball_event_set_timestamp_Handler,		//423
	joy_ball_event_get_which_Handler,		//424
	joy_ball_event_set_which_Handler,		//425
	joy_ball_event_get_ball_Handler,		//426
	joy_ball_event_set_ball_Handler,		//427
	joy_ball_event_get_padding1_Handler,		//428
	joy_ball_event_set_padding1_Handler,		//429
	joy_ball_event_get_padding2_Handler,		//430
	joy_ball_event_set_padding2_Handler,		//431
	joy_ball_event_get_padding3_Handler,		//432
	joy_ball_event_set_padding3_Handler,		//433
	joy_ball_event_get_xrel_Handler,		//434
	joy_ball_event_set_xrel_Handler,		//435
	joy_ball_event_get_yrel_Handler,		//436
	joy_ball_event_set_yrel_Handler,		//437
	pointer_deref_joy_hat_event_Handler,		//438
	pointer_deref_joy_hat_event_array_Handler,		//439
	pointer_deref_joy_hat_event_assign_Handler,		//440
	pointer_deref_joy_hat_event_array_assign_Handler,		//441
	new_joy_hat_event_Handler,		//442
	new_joy_hat_event_array_Handler,		//443
	delete_joy_hat_event_Handler,		//444
	joy_hat_event_get_type_Handler,		//445
	joy_hat_event_set_type_Handler,		//446
	joy_hat_event_get_timestamp_Handler,		//447
	joy_hat_event_set_timestamp_Handler,		//448
	joy_hat_event_get_which_Handler,		//449
	joy_hat_event_set_which_Handler,		//450
	joy_hat_event_get_hat_Handler,		//451
	joy_hat_event_set_hat_Handler,		//452
	joy_hat_event_get_value_Handler,		//453
	joy_hat_event_set_value_Handler,		//454
	joy_hat_event_get_padding1_Handler,		//455
	joy_hat_event_set_padding1_Handler,		//456
	joy_hat_event_get_padding2_Handler,		//457
	joy_hat_event_set_padding2_Handler,		//458
	pointer_deref_joy_button_event_Handler,		//459
	pointer_deref_joy_button_event_array_Handler,		//460
	pointer_deref_joy_button_event_assign_Handler,		//461
	pointer_deref_joy_button_event_array_assign_Handler,		//462
	new_joy_button_event_Handler,		//463
	new_joy_button_event_array_Handler,		//464
	delete_joy_button_event_Handler,		//465
	joy_button_event_get_type_Handler,		//466
	joy_button_event_set_type_Handler,		//467
	joy_button_event_get_timestamp_Handler,		//468
	joy_button_event_set_timestamp_Handler,		//469
	joy_button_event_get_which_Handler,		//470
	joy_button_event_set_which_Handler,		//471
	joy_button_event_get_button_Handler,		//472
	joy_button_event_set_button_Handler,		//473
	joy_button_event_get_state_Handler,		//474
	joy_button_event_set_state_Handler,		//475
	joy_button_event_get_padding1_Handler,		//476
	joy_button_event_set_padding1_Handler,		//477
	joy_button_event_get_padding2_Handler,		//478
	joy_button_event_set_padding2_Handler,		//479
	pointer_deref_joy_device_event_Handler,		//480
	pointer_deref_joy_device_event_array_Handler,		//481
	pointer_deref_joy_device_event_assign_Handler,		//482
	pointer_deref_joy_device_event_array_assign_Handler,		//483
	new_joy_device_event_Handler,		//484
	new_joy_device_event_array_Handler,		//485
	delete_joy_device_event_Handler,		//486
	joy_device_event_get_type_Handler,		//487
	joy_device_event_set_type_Handler,		//488
	joy_device_event_get_timestamp_Handler,		//489
	joy_device_event_set_timestamp_Handler,		//490
	joy_device_event_get_which_Handler,		//491
	joy_device_event_set_which_Handler,		//492
	pointer_deref_controller_axis_event_Handler,		//493
	pointer_deref_controller_axis_event_array_Handler,		//494
	pointer_deref_controller_axis_event_assign_Handler,		//495
	pointer_deref_controller_axis_event_array_assign_Handler,		//496
	new_controller_axis_event_Handler,		//497
	new_controller_axis_event_array_Handler,		//498
	delete_controller_axis_event_Handler,		//499
	controller_axis_event_get_type_Handler,		//500
	controller_axis_event_set_type_Handler,		//501
	controller_axis_event_get_timestamp_Handler,		//502
	controller_axis_event_set_timestamp_Handler,		//503
	controller_axis_event_get_which_Handler,		//504
	controller_axis_event_set_which_Handler,		//505
	controller_axis_event_get_axis_Handler,		//506
	controller_axis_event_set_axis_Handler,		//507
	controller_axis_event_get_padding1_Handler,		//508
	controller_axis_event_set_padding1_Handler,		//509
	controller_axis_event_get_padding2_Handler,		//510
	controller_axis_event_set_padding2_Handler,		//511
	controller_axis_event_get_padding3_Handler,		//512
	controller_axis_event_set_padding3_Handler,		//513
	controller_axis_event_get_value_Handler,		//514
	controller_axis_event_set_value_Handler,		//515
	controller_axis_event_get_padding4_Handler,		//516
	controller_axis_event_set_padding4_Handler,		//517
	pointer_deref_controller_button_event_Handler,		//518
	pointer_deref_controller_button_event_array_Handler,		//519
	pointer_deref_controller_button_event_assign_Handler,		//520
	pointer_deref_controller_button_event_array_assign_Handler,		//521
	new_controller_button_event_Handler,		//522
	new_controller_button_event_array_Handler,		//523
	delete_controller_button_event_Handler,		//524
	controller_button_event_get_type_Handler,		//525
	controller_button_event_set_type_Handler,		//526
	controller_button_event_get_timestamp_Handler,		//527
	controller_button_event_set_timestamp_Handler,		//528
	controller_button_event_get_which_Handler,		//529
	controller_button_event_set_which_Handler,		//530
	controller_button_event_get_button_Handler,		//531
	controller_button_event_set_button_Handler,		//532
	controller_button_event_get_state_Handler,		//533
	controller_button_event_set_state_Handler,		//534
	controller_button_event_get_padding1_Handler,		//535
	controller_button_event_set_padding1_Handler,		//536
	controller_button_event_get_padding2_Handler,		//537
	controller_button_event_set_padding2_Handler,		//538
	pointer_deref_controller_device_event_Handler,		//539
	pointer_deref_controller_device_event_array_Handler,		//540
	pointer_deref_controller_device_event_assign_Handler,		//541
	pointer_deref_controller_device_event_array_assign_Handler,		//542
	new_controller_device_event_Handler,		//543
	new_controller_device_event_array_Handler,		//544
	delete_controller_device_event_Handler,		//545
	controller_device_event_get_type_Handler,		//546
	controller_device_event_set_type_Handler,		//547
	controller_device_event_get_timestamp_Handler,		//548
	controller_device_event_set_timestamp_Handler,		//549
	controller_device_event_get_which_Handler,		//550
	controller_device_event_set_which_Handler,		//551
	pointer_deref_audio_device_event_Handler,		//552
	pointer_deref_audio_device_event_array_Handler,		//553
	pointer_deref_audio_device_event_assign_Handler,		//554
	pointer_deref_audio_device_event_array_assign_Handler,		//555
	new_audio_device_event_Handler,		//556
	new_audio_device_event_array_Handler,		//557
	delete_audio_device_event_Handler,		//558
	audio_device_event_get_type_Handler,		//559
	audio_device_event_set_type_Handler,		//560
	audio_device_event_get_timestamp_Handler,		//561
	audio_device_event_set_timestamp_Handler,		//562
	audio_device_event_get_which_Handler,		//563
	audio_device_event_set_which_Handler,		//564
	audio_device_event_get_iscapture_Handler,		//565
	audio_device_event_set_iscapture_Handler,		//566
	audio_device_event_get_padding1_Handler,		//567
	audio_device_event_set_padding1_Handler,		//568
	audio_device_event_get_padding2_Handler,		//569
	audio_device_event_set_padding2_Handler,		//570
	audio_device_event_get_padding3_Handler,		//571
	audio_device_event_set_padding3_Handler,		//572
	pointer_deref_quit_event_Handler,		//573
	pointer_deref_quit_event_array_Handler,		//574
	pointer_deref_quit_event_assign_Handler,		//575
	pointer_deref_quit_event_array_assign_Handler,		//576
	new_quit_event_Handler,		//577
	new_quit_event_array_Handler,		//578
	delete_quit_event_Handler,		//579
	quit_event_get_type_Handler,		//580
	quit_event_set_type_Handler,		//581
	quit_event_get_timestamp_Handler,		//582
	quit_event_set_timestamp_Handler,		//583
	pointer_deref_user_event_Handler,		//584
	pointer_deref_user_event_array_Handler,		//585
	pointer_deref_user_event_assign_Handler,		//586
	pointer_deref_user_event_array_assign_Handler,		//587
	new_user_event_Handler,		//588
	new_user_event_array_Handler,		//589
	delete_user_event_Handler,		//590
	user_event_get_type_Handler,		//591
	user_event_set_type_Handler,		//592
	user_event_get_timestamp_Handler,		//593
	user_event_set_timestamp_Handler,		//594
	user_event_get_windowID_Handler,		//595
	user_event_set_windowID_Handler,		//596
	user_event_get_code_Handler,		//597
	user_event_set_code_Handler,		//598
	user_event_get_data1_Handler,		//599
	user_event_set_data1_Handler,		//600
	user_event_get_data2_Handler,		//601
	user_event_set_data2_Handler,		//602
	pointer_deref_syswm_event_Handler,		//603
	pointer_deref_syswm_event_array_Handler,		//604
	pointer_deref_syswm_event_assign_Handler,		//605
	pointer_deref_syswm_event_array_assign_Handler,		//606
	new_syswm_event_Handler,		//607
	new_syswm_event_array_Handler,		//608
	delete_syswm_event_Handler,		//609
	syswm_event_get_type_Handler,		//610
	syswm_event_set_type_Handler,		//611
	syswm_event_get_timestamp_Handler,		//612
	syswm_event_set_timestamp_Handler,		//613
	syswm_event_get_msg_Handler,		//614
	syswm_event_set_msg_Handler,		//615
	pointer_deref_touch_finger_event_Handler,		//616
	pointer_deref_touch_finger_event_array_Handler,		//617
	pointer_deref_touch_finger_event_assign_Handler,		//618
	pointer_deref_touch_finger_event_array_assign_Handler,		//619
	new_touch_finger_event_Handler,		//620
	new_touch_finger_event_array_Handler,		//621
	delete_touch_finger_event_Handler,		//622
	touch_finger_event_get_type_Handler,		//623
	touch_finger_event_set_type_Handler,		//624
	touch_finger_event_get_timestamp_Handler,		//625
	touch_finger_event_set_timestamp_Handler,		//626
	touch_finger_event_get_touchId_Handler,		//627
	touch_finger_event_set_touchId_Handler,		//628
	touch_finger_event_get_fingerId_Handler,		//629
	touch_finger_event_set_fingerId_Handler,		//630
	touch_finger_event_get_x_Handler,		//631
	touch_finger_event_set_x_Handler,		//632
	touch_finger_event_get_y_Handler,		//633
	touch_finger_event_set_y_Handler,		//634
	touch_finger_event_get_dx_Handler,		//635
	touch_finger_event_set_dx_Handler,		//636
	touch_finger_event_get_dy_Handler,		//637
	touch_finger_event_set_dy_Handler,		//638
	touch_finger_event_get_pressure_Handler,		//639
	touch_finger_event_set_pressure_Handler,		//640
	pointer_deref_multi_gesture_event_Handler,		//641
	pointer_deref_multi_gesture_event_array_Handler,		//642
	pointer_deref_multi_gesture_event_assign_Handler,		//643
	pointer_deref_multi_gesture_event_array_assign_Handler,		//644
	new_multi_gesture_event_Handler,		//645
	new_multi_gesture_event_array_Handler,		//646
	delete_multi_gesture_event_Handler,		//647
	multi_gesture_event_get_type_Handler,		//648
	multi_gesture_event_set_type_Handler,		//649
	multi_gesture_event_get_timestamp_Handler,		//650
	multi_gesture_event_set_timestamp_Handler,		//651
	multi_gesture_event_get_touchId_Handler,		//652
	multi_gesture_event_set_touchId_Handler,		//653
	multi_gesture_event_get_dTheta_Handler,		//654
	multi_gesture_event_set_dTheta_Handler,		//655
	multi_gesture_event_get_dDist_Handler,		//656
	multi_gesture_event_set_dDist_Handler,		//657
	multi_gesture_event_get_x_Handler,		//658
	multi_gesture_event_set_x_Handler,		//659
	multi_gesture_event_get_y_Handler,		//660
	multi_gesture_event_set_y_Handler,		//661
	multi_gesture_event_get_numFingers_Handler,		//662
	multi_gesture_event_set_numFingers_Handler,		//663
	multi_gesture_event_get_padding_Handler,		//664
	multi_gesture_event_set_padding_Handler,		//665
	pointer_deref_dollar_gesture_event_Handler,		//666
	pointer_deref_dollar_gesture_event_array_Handler,		//667
	pointer_deref_dollar_gesture_event_assign_Handler,		//668
	pointer_deref_dollar_gesture_event_array_assign_Handler,		//669
	new_dollar_gesture_event_Handler,		//670
	new_dollar_gesture_event_array_Handler,		//671
	delete_dollar_gesture_event_Handler,		//672
	dollar_gesture_event_get_type_Handler,		//673
	dollar_gesture_event_set_type_Handler,		//674
	dollar_gesture_event_get_timestamp_Handler,		//675
	dollar_gesture_event_set_timestamp_Handler,		//676
	dollar_gesture_event_get_touchId_Handler,		//677
	dollar_gesture_event_set_touchId_Handler,		//678
	dollar_gesture_event_get_gestureId_Handler,		//679
	dollar_gesture_event_set_gestureId_Handler,		//680
	dollar_gesture_event_get_numFingers_Handler,		//681
	dollar_gesture_event_set_numFingers_Handler,		//682
	dollar_gesture_event_get_error_Handler,		//683
	dollar_gesture_event_set_error_Handler,		//684
	dollar_gesture_event_get_x_Handler,		//685
	dollar_gesture_event_set_x_Handler,		//686
	dollar_gesture_event_get_y_Handler,		//687
	dollar_gesture_event_set_y_Handler,		//688
	pointer_deref_drop_event_Handler,		//689
	pointer_deref_drop_event_array_Handler,		//690
	pointer_deref_drop_event_assign_Handler,		//691
	pointer_deref_drop_event_array_assign_Handler,		//692
	new_drop_event_Handler,		//693
	new_drop_event_array_Handler,		//694
	delete_drop_event_Handler,		//695
	drop_event_get_type_Handler,		//696
	drop_event_set_type_Handler,		//697
	drop_event_get_timestamp_Handler,		//698
	drop_event_set_timestamp_Handler,		//699
	drop_event_get_file_Handler,		//700
	drop_event_set_file_Handler,		//701
	drop_event_get_windowID_Handler,		//702
	drop_event_set_windowID_Handler,		//703
	new_event_Handler,		//704
	new_event_array_Handler,		//705
	delete_event_Handler,		//706
	event_get_type_Handler,		//707
	event_set_type_Handler,		//708
	event_get_common_Handler,		//709
	event_set_common_Handler,		//710
	event_get_window_Handler,		//711
	event_set_window_Handler,		//712
	event_get_key_Handler,		//713
	event_set_key_Handler,		//714
	event_get_edit_Handler,		//715
	event_set_edit_Handler,		//716
	event_get_text_Handler,		//717
	event_set_text_Handler,		//718
	event_get_motion_Handler,		//719
	event_set_motion_Handler,		//720
	event_get_button_Handler,		//721
	event_set_button_Handler,		//722
	event_get_wheel_Handler,		//723
	event_set_wheel_Handler,		//724
	event_get_jaxis_Handler,		//725
	event_set_jaxis_Handler,		//726
	event_get_jball_Handler,		//727
	event_set_jball_Handler,		//728
	event_get_jhat_Handler,		//729
	event_set_jhat_Handler,		//730
	event_get_jbutton_Handler,		//731
	event_set_jbutton_Handler,		//732
	event_get_jdevice_Handler,		//733
	event_set_jdevice_Handler,		//734
	event_get_caxis_Handler,		//735
	event_set_caxis_Handler,		//736
	event_get_cbutton_Handler,		//737
	event_set_cbutton_Handler,		//738
	event_get_cdevice_Handler,		//739
	event_set_cdevice_Handler,		//740
	event_get_adevice_Handler,		//741
	event_set_adevice_Handler,		//742
	event_get_quit_Handler,		//743
	event_set_quit_Handler,		//744
	event_get_user_Handler,		//745
	event_set_user_Handler,		//746
	event_get_syswm_Handler,		//747
	event_set_syswm_Handler,		//748
	event_get_tfinger_Handler,		//749
	event_set_tfinger_Handler,		//750
	event_get_mgesture_Handler,		//751
	event_set_mgesture_Handler,		//752
	event_get_dgesture_Handler,		//753
	event_set_dgesture_Handler,		//754
	event_get_drop_Handler,		//755
	event_set_drop_Handler,		//756
	init_Handler,		//757
	quit_Handler,		//758
	create_window_Handler,		//759
	get_window_surface_Handler,		//760
	load_bmp_Handler,		//761
	free_surface_Handler,		//762
	blit_surface_Handler,		//763
	blit_scaled_Handler,		//764
	update_window_surface_Handler,		//765
	destroy_window_Handler,		//766
	get_window_size_Handler,		//767
	get_error_Handler,		//768
	poll_event_Handler,		//769
	maxint_Handler		//770
};

//--------------------------------------------------------

int read_command(byte *buf) {
	int len;

	if (fread(buf, 2, 1, stdin) != 1) {
		return -1;
	}
	len = (buf[0] << 8) | buf[1];
	
	if (len >= BUF_SIZE) {
		return -1;
	} else {
		int numRead = fread(buf, len, 1, stdin);
		return (numRead == 1) ? len : -1;
	}
}

void write_command(byte *buf, size_t len) {
	byte len_h = (len >> 8) & 255;
	byte len_v = len & 255;
	byte arr[] = { len_h, len_v };

	fwrite(arr, 2, 1, stdout);
	fwrite(buf, len, 1, stdout);
	fflush(stdout);
}

int main() {
	byte input_buffer[BUF_SIZE], output_buffer[BUF_SIZE];

	int len_in = read_command(input_buffer);
	size_t len_out;
	int code;
	read_int(input_buffer, &code);
	
	while (len_in > 0 && code != 0) { 
		handler current_handler = handlers[code];

		(*current_handler)(input_buffer, len_in, output_buffer, &len_out);

		write_command(output_buffer, len_out);
		len_in = read_command(input_buffer);
		read_int(input_buffer, &code);
	}
}