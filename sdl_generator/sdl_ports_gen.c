#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdint.h>

#include <SDL2/SDL.h>

#define BUF_SIZE 60000

typedef unsigned char byte;
typedef unsigned char string[BUF_SIZE];
typedef void (*handler)(byte *in, size_t len_in, byte *out, size_t *len_out);

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

	*result = (((int16_t)*current_in++) << 8) | ((int16_t)*current_in++);

	return current_in;
}

byte * read_int32(byte *in, int *result) {
	byte *current_in = in;

	*result = (((int)*current_in++) << 24) | (((int)*current_in++) << 16) |
						(((int)*current_in++) << 8) | ((int)*current_in++);

	return current_in;
}

byte * read_int64(byte *in, int64_t *result) {
	byte *current_in = in;

	current_in = in;
	*result = (((int64_t)*current_in++) << 56) | (((int64_t)*current_in++) << 48) |
				(((int64_t)*current_in++) << 40) | (((int64_t)*current_in++) << 32) |
				(((int64_t)*current_in++) << 24) | (((int64_t)*current_in++) << 16) |
				(((int64_t)*current_in++) << 8) | ((int64_t)*current_in++);
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

void pointer_deref_int_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_int(ptr, current_out, len_out);
}

void pointer_deref_int_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	current_in = read_int(current_in, &value);
	*ptr = value;
}

void new_int_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int *ptr = malloc(sizeof(int));
	current_out = write_pointer(&ptr, current_out, len_out);
}

void delete_int_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
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

void pointer_deref_float_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	float *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_float(ptr, current_out, len_out);
}

void pointer_deref_float_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	float *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	current_in = read_float(current_in, &value);
	*ptr = value;
}

void new_float_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	float *ptr = malloc(sizeof(float));
	current_out = write_pointer(&ptr, current_out, len_out);
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

void pointer_deref_double_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	double *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_double(ptr, current_out, len_out);
}

void pointer_deref_double_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	double *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	double value;
	current_in = read_double(current_in, &value);
	*ptr = value;
}

void new_double_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	double *ptr = malloc(sizeof(double));
	current_out = write_pointer(&ptr, current_out, len_out);
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

	int strsize = (int) strlen(*str);

	for (long i=0; i<strsize; i++) {
		*current_out++ = (*str)[i]; (*len)++;
	}
	*current_out++ = '\0'; (*len)++;

	return current_out;
}

void pointer_deref_string_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	string *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_out = write_string(ptr, current_out, len_out);
}

void pointer_deref_string_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	string *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	string value;
	current_in = read_string(current_in, &value);
	strcpy(*ptr, value);
}

void new_string_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	string *ptr = malloc(sizeof(string));
	current_out = write_pointer(&ptr, current_out, len_out);
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

	current_in = read_int64(current_in, &p);
	*result = (void *) p;

	return current_in;
}

byte * write_pointer(void **pointer, byte *out, size_t *len) {
	byte *current_out = out;
	uintptr_t p = (uintptr_t) (*pointer);

	current_out = write_int64(&p, current_out, len);

	return current_out;
}

//--------------------------------------------------------

byte * read_uint64(byte *in, Uint64 *result) {
	byte *current_in = in;

	current_in = read_int64(current_in, result);

	return current_in;
}

byte * write_uint64(Uint64 *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_int64(value, current_out, len);

	return current_out;
}

byte * read_uint32(byte *in, Uint32 *result) {
	byte *current_in = in;

	current_in = read_int32(current_in, result);

	return current_in;
}

byte * write_uint32(Uint32 *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_int32(value, current_out, len);

	return current_out;
}

byte * read_uint16(byte *in, Uint16 *result) {
	byte *current_in = in;

	current_in = read_int16(current_in, result);

	return current_in;
}

byte * write_uint16(Uint16 *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_int16(value, current_out, len);

	return current_out;
}

byte * read_uint8(byte *in, Uint8 *result) {
	byte *current_in = in;

	current_in = read_int8(current_in, result);

	return current_in;
}

byte * write_uint8(Uint8 *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_int8(value, current_out, len);

	return current_out;
}

byte * read_sint64(byte *in, Sint64 *result) {
	byte *current_in = in;

	current_in = read_int64(current_in, result);

	return current_in;
}

byte * write_sint64(Sint64 *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_int64(value, current_out, len);

	return current_out;
}

byte * read_sint32(byte *in, Sint32 *result) {
	byte *current_in = in;

	current_in = read_int32(current_in, result);

	return current_in;
}

byte * write_sint32(Sint32 *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_int32(value, current_out, len);

	return current_out;
}

byte * read_sint16(byte *in, Sint16 *result) {
	byte *current_in = in;

	current_in = read_int16(current_in, result);

	return current_in;
}

byte * write_sint16(Sint16 *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_int16(value, current_out, len);

	return current_out;
}

byte * read_sint8(byte *in, Sint8 *result) {
	byte *current_in = in;

	current_in = read_int8(current_in, result);

	return current_in;
}

byte * write_sint8(Sint8 *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_int8(value, current_out, len);

	return current_out;
}

byte * read_window(byte *in, SDL_Window *result) {
	byte *current_in = in;

	current_in = read_pointer(current_in, (void **) &result);

	return current_in;
}

byte * write_window(SDL_Window *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_pointer(&value, current_out, len);

	return current_out;
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

void pointer_deref_color_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_color(pointer, current_out, len_out);
}

void pointer_deref_color_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_color(current_in, pointer);
}

void new_color_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *ptr = malloc(sizeof(SDL_Color));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint8 value;
	value = ptr->r;
	current_out = write_uint8(&value, current_out, len_out);
}

void color_set_r_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->r = value;
}

void color_get_g_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->g;
	current_out = write_uint8(&value, current_out, len_out);
}

void color_set_g_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->g = value;
}

void color_get_b_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->b;
	current_out = write_uint8(&value, current_out, len_out);
}

void color_set_b_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->b = value;
}

void color_get_a_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->a;
	current_out = write_uint8(&value, current_out, len_out);
}

void color_set_a_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Color *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->a = value;
}

byte * read_palette(byte *in, SDL_Palette *result) {
	byte *current_in = in;

	current_in = read_int(current_in, &(result->ncolors));
	current_in = read_pointer(current_in, &(result->colors));
	current_in = read_uint32(current_in, &(result->version));
	current_in = read_int(current_in, &(result->refcount));

	return current_in;
}

byte * write_palette(SDL_Palette *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_int(&(value->ncolors), current_out, len);
	current_out = write_pointer(&(value->colors), current_out, len);
	current_out = write_uint32(&(value->version), current_out, len);
	current_out = write_int(&(value->refcount), current_out, len);

	return current_out;
}

void pointer_deref_palette_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_palette(pointer, current_out, len_out);
}

void pointer_deref_palette_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_palette(current_in, pointer);
}

void new_palette_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *ptr = malloc(sizeof(SDL_Palette));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	int value;
	value = ptr->ncolors;
	current_out = write_int(&value, current_out, len_out);
}

void palette_set_ncolors_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	current_in = read_int(current_in, &value);
	ptr->ncolors = value;
}

void palette_get_colors_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_Color* value;
	value = ptr->colors;
	current_out = write_pointer(&value, current_out, len_out);
}

void palette_set_colors_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_Color* value;
	current_in = read_pointer(current_in, &value);
	ptr->colors = value;
}

void palette_get_version_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->version;
	current_out = write_uint32(&value, current_out, len_out);
}

void palette_set_version_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->version = value;
}

void palette_get_refcount_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	value = ptr->refcount;
	current_out = write_int(&value, current_out, len_out);
}

void palette_set_refcount_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Palette *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	current_in = read_int(current_in, &value);
	ptr->refcount = value;
}

byte * read_pixel_format(byte *in, SDL_PixelFormat *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->format));
	current_in = read_pointer(current_in, &(result->palette));
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
	current_in = read_pointer(current_in, &(result->next));

	return current_in;
}

byte * write_pixel_format(SDL_PixelFormat *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->format), current_out, len);
	current_out = write_pointer(&(value->palette), current_out, len);
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
	current_out = write_pointer(&(value->next), current_out, len);

	return current_out;
}

void pointer_deref_pixel_format_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_pixel_format(pointer, current_out, len_out);
}

void pointer_deref_pixel_format_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_pixel_format(current_in, pointer);
}

void new_pixel_format_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr = malloc(sizeof(SDL_PixelFormat));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->format;
	current_out = write_uint32(&value, current_out, len_out);
}

void pixel_format_set_format_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->format = value;
}

void pixel_format_get_palette_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_Palette* value;
	value = ptr->palette;
	current_out = write_pointer(&value, current_out, len_out);
}

void pixel_format_set_palette_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_Palette* value;
	current_in = read_pointer(current_in, &value);
	ptr->palette = value;
}

void pixel_format_get_bits_per_pixel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->BitsPerPixel;
	current_out = write_uint8(&value, current_out, len_out);
}

void pixel_format_set_bits_per_pixel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->BitsPerPixel = value;
}

void pixel_format_get_bytes_per_pixel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->BytesPerPixel;
	current_out = write_uint8(&value, current_out, len_out);
}

void pixel_format_set_bytes_per_pixel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->BytesPerPixel = value;
}

void pixel_format_get_r_mask_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->Rmask;
	current_out = write_uint32(&value, current_out, len_out);
}

void pixel_format_set_r_mask_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->Rmask = value;
}

void pixel_format_get_g_mask_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->Gmask;
	current_out = write_uint32(&value, current_out, len_out);
}

void pixel_format_set_g_mask_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->Gmask = value;
}

void pixel_format_get_b_mask_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->Bmask;
	current_out = write_uint32(&value, current_out, len_out);
}

void pixel_format_set_b_mask_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->Bmask = value;
}

void pixel_format_get_a_mask_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->Amask;
	current_out = write_uint32(&value, current_out, len_out);
}

void pixel_format_set_a_mask_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->Amask = value;
}

void pixel_format_get_r_loss_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->Rloss;
	current_out = write_uint8(&value, current_out, len_out);
}

void pixel_format_set_r_loss_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->Rloss = value;
}

void pixel_format_get_g_loss_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->Gloss;
	current_out = write_uint8(&value, current_out, len_out);
}

void pixel_format_set_g_loss_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->Gloss = value;
}

void pixel_format_get_b_loss_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->Bloss;
	current_out = write_uint8(&value, current_out, len_out);
}

void pixel_format_set_b_loss_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->Bloss = value;
}

void pixel_format_get_a_loss_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->Aloss;
	current_out = write_uint8(&value, current_out, len_out);
}

void pixel_format_set_a_loss_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->Aloss = value;
}

void pixel_format_get_r_shift_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->Rshift;
	current_out = write_uint8(&value, current_out, len_out);
}

void pixel_format_set_r_shift_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->Rshift = value;
}

void pixel_format_get_g_shift_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->Gshift;
	current_out = write_uint8(&value, current_out, len_out);
}

void pixel_format_set_g_shift_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->Gshift = value;
}

void pixel_format_get_b_shift_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->Bshift;
	current_out = write_uint8(&value, current_out, len_out);
}

void pixel_format_set_b_shift_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->Bshift = value;
}

void pixel_format_get_a_shift_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->Ashift;
	current_out = write_uint8(&value, current_out, len_out);
}

void pixel_format_set_a_shift_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->Ashift = value;
}

void pixel_format_get_refcount_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	value = ptr->refcount;
	current_out = write_int(&value, current_out, len_out);
}

void pixel_format_set_refcount_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	current_in = read_int(current_in, &value);
	ptr->refcount = value;
}

void pixel_format_get_next_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_PixelFormat* value;
	value = ptr->next;
	current_out = write_pointer(&value, current_out, len_out);
}

void pixel_format_set_next_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_PixelFormat *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_PixelFormat* value;
	current_in = read_pointer(current_in, &value);
	ptr->next = value;
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

void pointer_deref_rect_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_rect(pointer, current_out, len_out);
}

void pointer_deref_rect_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_rect(current_in, pointer);
}

void new_rect_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *ptr = malloc(sizeof(SDL_Rect));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	int value;
	value = ptr->x;
	current_out = write_int(&value, current_out, len_out);
}

void rect_set_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	current_in = read_int(current_in, &value);
	ptr->x = value;
}

void rect_get_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	value = ptr->y;
	current_out = write_int(&value, current_out, len_out);
}

void rect_set_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	current_in = read_int(current_in, &value);
	ptr->y = value;
}

void rect_get_w_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	value = ptr->w;
	current_out = write_int(&value, current_out, len_out);
}

void rect_set_w_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	current_in = read_int(current_in, &value);
	ptr->w = value;
}

void rect_get_h_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	value = ptr->h;
	current_out = write_int(&value, current_out, len_out);
}

void rect_set_h_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Rect *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	current_in = read_int(current_in, &value);
	ptr->h = value;
}

byte * read_blit_map(byte *in, struct SDL_BlitMap *result) {
	byte *current_in = in;

	current_in = read_pointer(current_in, (void **) &result);

	return current_in;
}

byte * write_blit_map(struct SDL_BlitMap *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_pointer(&value, current_out, len);

	return current_out;
}

byte * read_surface(byte *in, SDL_Surface *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->flags));
	current_in = read_pointer(current_in, &(result->format));
	current_in = read_int(current_in, &(result->w));
	current_in = read_int(current_in, &(result->h));
	current_in = read_int(current_in, &(result->pitch));
	current_in = read_pointer(current_in, &(result->pixels));
	current_in = read_pointer(current_in, &(result->userdata));
	current_in = read_int(current_in, &(result->locked));
	current_in = read_pointer(current_in, &(result->lock_data));
	current_in = read_rect(current_in, &(result->clip_rect));
	current_in = read_pointer(current_in, &(result->map));
	current_in = read_int(current_in, &(result->refcount));

	return current_in;
}

byte * write_surface(SDL_Surface *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->flags), current_out, len);
	current_out = write_pointer(&(value->format), current_out, len);
	current_out = write_int(&(value->w), current_out, len);
	current_out = write_int(&(value->h), current_out, len);
	current_out = write_int(&(value->pitch), current_out, len);
	current_out = write_pointer(&(value->pixels), current_out, len);
	current_out = write_pointer(&(value->userdata), current_out, len);
	current_out = write_int(&(value->locked), current_out, len);
	current_out = write_pointer(&(value->lock_data), current_out, len);
	current_out = write_rect(&(value->clip_rect), current_out, len);
	current_out = write_pointer(&(value->map), current_out, len);
	current_out = write_int(&(value->refcount), current_out, len);

	return current_out;
}

void pointer_deref_surface_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_surface(pointer, current_out, len_out);
}

void pointer_deref_surface_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_surface(current_in, pointer);
}

void new_surface_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr = malloc(sizeof(SDL_Surface));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->flags;
	current_out = write_uint32(&value, current_out, len_out);
}

void surface_set_flags_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->flags = value;
}

void surface_get_format_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_PixelFormat* value;
	value = ptr->format;
	current_out = write_pointer(&value, current_out, len_out);
}

void surface_set_format_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_PixelFormat* value;
	current_in = read_pointer(current_in, &value);
	ptr->format = value;
}

void surface_get_w_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	value = ptr->w;
	current_out = write_int(&value, current_out, len_out);
}

void surface_set_w_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	current_in = read_int(current_in, &value);
	ptr->w = value;
}

void surface_get_h_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	value = ptr->h;
	current_out = write_int(&value, current_out, len_out);
}

void surface_set_h_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	current_in = read_int(current_in, &value);
	ptr->h = value;
}

void surface_get_pitch_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	value = ptr->pitch;
	current_out = write_int(&value, current_out, len_out);
}

void surface_set_pitch_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	current_in = read_int(current_in, &value);
	ptr->pitch = value;
}

void surface_get_pixels_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	void* value;
	value = ptr->pixels;
	current_out = write_pointer(&value, current_out, len_out);
}

void surface_set_pixels_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	void* value;
	current_in = read_pointer(current_in, &value);
	ptr->pixels = value;
}

void surface_get_userdata_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	void* value;
	value = ptr->userdata;
	current_out = write_pointer(&value, current_out, len_out);
}

void surface_set_userdata_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	void* value;
	current_in = read_pointer(current_in, &value);
	ptr->userdata = value;
}

void surface_get_locked_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	value = ptr->locked;
	current_out = write_int(&value, current_out, len_out);
}

void surface_set_locked_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	current_in = read_int(current_in, &value);
	ptr->locked = value;
}

void surface_get_lock_data_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	void* value;
	value = ptr->lock_data;
	current_out = write_pointer(&value, current_out, len_out);
}

void surface_set_lock_data_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	void* value;
	current_in = read_pointer(current_in, &value);
	ptr->lock_data = value;
}

void surface_get_clip_rect_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_Rect value;
	value = ptr->clip_rect;
	current_out = write_rect(&value, current_out, len_out);
}

void surface_set_clip_rect_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_Rect value;
	current_in = read_rect(current_in, &value);
	ptr->clip_rect = value;
}

void surface_get_map_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	struct SDL_BlitMap* value;
	value = ptr->map;
	current_out = write_pointer(&value, current_out, len_out);
}

void surface_set_map_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	struct SDL_BlitMap* value;
	current_in = read_pointer(current_in, &value);
	ptr->map = value;
}

void surface_get_refcount_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	value = ptr->refcount;
	current_out = write_int(&value, current_out, len_out);
}

void surface_set_refcount_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	int value;
	current_in = read_int(current_in, &value);
	ptr->refcount = value;
}

byte * read_scancode(byte *in, SDL_Scancode *result) {
	byte *current_in = in;

	current_in = read_int(current_in, result);

	return current_in;
}

byte * write_scancode(SDL_Scancode *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_int(value, current_out, len);

	return current_out;
}

byte * read_keycode(byte *in, SDL_Keycode *result) {
	byte *current_in = in;

	current_in = read_sint32(current_in, result);

	return current_in;
}

byte * write_keycode(SDL_Keycode *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_sint32(value, current_out, len);

	return current_out;
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

void pointer_deref_keysym_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_keysym(pointer, current_out, len_out);
}

void pointer_deref_keysym_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_keysym(current_in, pointer);
}

void new_keysym_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *ptr = malloc(sizeof(SDL_Keysym));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	SDL_Scancode value;
	value = ptr->scancode;
	current_out = write_scancode(&value, current_out, len_out);
}

void keysym_set_scancode_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_Scancode value;
	current_in = read_scancode(current_in, &value);
	ptr->scancode = value;
}

void keysym_get_sym_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_Keycode value;
	value = ptr->sym;
	current_out = write_keycode(&value, current_out, len_out);
}

void keysym_set_sym_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_Keycode value;
	current_in = read_keycode(current_in, &value);
	ptr->sym = value;
}

void keysym_get_mod_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint16 value;
	value = ptr->mod;
	current_out = write_uint16(&value, current_out, len_out);
}

void keysym_set_mod_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint16 value;
	current_in = read_uint16(current_in, &value);
	ptr->mod = value;
}

void keysym_get_unused_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->unused;
	current_out = write_uint32(&value, current_out, len_out);
}

void keysym_set_unused_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Keysym *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->unused = value;
}

byte * read_joystick_id(byte *in, SDL_JoystickID *result) {
	byte *current_in = in;

	current_in = read_sint32(current_in, result);

	return current_in;
}

byte * write_joystick_id(SDL_JoystickID *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_sint32(value, current_out, len);

	return current_out;
}

byte * read_syswm_msg(byte *in, SDL_SysWMmsg *result) {
	byte *current_in = in;

	current_in = read_pointer(current_in, (void **) &result);

	return current_in;
}

byte * write_syswm_msg(SDL_SysWMmsg *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_pointer(&value, current_out, len);

	return current_out;
}

byte * read_touch_id(byte *in, SDL_TouchID *result) {
	byte *current_in = in;

	current_in = read_sint64(current_in, result);

	return current_in;
}

byte * write_touch_id(SDL_TouchID *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_sint64(value, current_out, len);

	return current_out;
}

byte * read_finger_id(byte *in, SDL_FingerID *result) {
	byte *current_in = in;

	current_in = read_sint64(current_in, result);

	return current_in;
}

byte * write_finger_id(SDL_FingerID *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_sint64(value, current_out, len);

	return current_out;
}

byte * read_gesture_id(byte *in, SDL_GestureID *result) {
	byte *current_in = in;

	current_in = read_sint64(current_in, result);

	return current_in;
}

byte * write_gesture_id(SDL_GestureID *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_sint64(value, current_out, len);

	return current_out;
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

void pointer_deref_common_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_CommonEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_common_event(pointer, current_out, len_out);
}

void pointer_deref_common_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_CommonEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_common_event(current_in, pointer);
}

void new_common_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_CommonEvent *ptr = malloc(sizeof(SDL_CommonEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void common_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_CommonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void common_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_CommonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void common_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_CommonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
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

void pointer_deref_window_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_window_event(pointer, current_out, len_out);
}

void pointer_deref_window_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_window_event(current_in, pointer);
}

void new_window_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr = malloc(sizeof(SDL_WindowEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void window_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void window_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void window_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void window_event_get_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->windowID;
	current_out = write_uint32(&value, current_out, len_out);
}

void window_event_set_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->windowID = value;
}

void window_event_get_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->event;
	current_out = write_uint8(&value, current_out, len_out);
}

void window_event_set_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->event = value;
}

void window_event_get_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding1;
	current_out = write_uint8(&value, current_out, len_out);
}

void window_event_set_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding1 = value;
}

void window_event_get_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding2;
	current_out = write_uint8(&value, current_out, len_out);
}

void window_event_set_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding2 = value;
}

void window_event_get_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding3;
	current_out = write_uint8(&value, current_out, len_out);
}

void window_event_set_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding3 = value;
}

void window_event_get_data1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	value = ptr->data1;
	current_out = write_sint32(&value, current_out, len_out);
}

void window_event_set_data1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	current_in = read_sint32(current_in, &value);
	ptr->data1 = value;
}

void window_event_get_data2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	value = ptr->data2;
	current_out = write_sint32(&value, current_out, len_out);
}

void window_event_set_data2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_WindowEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	current_in = read_sint32(current_in, &value);
	ptr->data2 = value;
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

void pointer_deref_keyboard_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_keyboard_event(pointer, current_out, len_out);
}

void pointer_deref_keyboard_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_keyboard_event(current_in, pointer);
}

void new_keyboard_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr = malloc(sizeof(SDL_KeyboardEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void keyboard_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void keyboard_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void keyboard_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void keyboard_event_get_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->windowID;
	current_out = write_uint32(&value, current_out, len_out);
}

void keyboard_event_set_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->windowID = value;
}

void keyboard_event_get_state_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->state;
	current_out = write_uint8(&value, current_out, len_out);
}

void keyboard_event_set_state_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->state = value;
}

void keyboard_event_get_repeat_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->repeat;
	current_out = write_uint8(&value, current_out, len_out);
}

void keyboard_event_set_repeat_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->repeat = value;
}

void keyboard_event_get_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding2;
	current_out = write_uint8(&value, current_out, len_out);
}

void keyboard_event_set_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding2 = value;
}

void keyboard_event_get_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding3;
	current_out = write_uint8(&value, current_out, len_out);
}

void keyboard_event_set_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding3 = value;
}

void keyboard_event_get_keysym_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_Keysym value;
	value = ptr->keysym;
	current_out = write_keysym(&value, current_out, len_out);
}

void keyboard_event_set_keysym_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_KeyboardEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_Keysym value;
	current_in = read_keysym(current_in, &value);
	ptr->keysym = value;
}

byte * read_text_editing_event(byte *in, SDL_TextEditingEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_uint32(current_in, &(result->windowID));
	current_in = read_string(current_in, &(result->text));
	current_in = read_sint32(current_in, &(result->start));
	current_in = read_sint32(current_in, &(result->length));

	return current_in;
}

byte * write_text_editing_event(SDL_TextEditingEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_uint32(&(value->windowID), current_out, len);
	current_out = write_string(&(value->text), current_out, len);
	current_out = write_sint32(&(value->start), current_out, len);
	current_out = write_sint32(&(value->length), current_out, len);

	return current_out;
}

void pointer_deref_text_editing_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_text_editing_event(pointer, current_out, len_out);
}

void pointer_deref_text_editing_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_text_editing_event(current_in, pointer);
}

void new_text_editing_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr = malloc(sizeof(SDL_TextEditingEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void text_editing_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void text_editing_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void text_editing_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void text_editing_event_get_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->windowID;
	current_out = write_uint32(&value, current_out, len_out);
}

void text_editing_event_set_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->windowID = value;
}

void text_editing_event_get_text_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	string value;
	strcpy(value, ptr->text);
	current_out = write_string(&value, current_out, len_out);
}

void text_editing_event_set_text_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	string value;
	current_in = read_string(current_in, &value);
	strcpy(ptr->text, value);
}

void text_editing_event_get_start_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	value = ptr->start;
	current_out = write_sint32(&value, current_out, len_out);
}

void text_editing_event_set_start_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	current_in = read_sint32(current_in, &value);
	ptr->start = value;
}

void text_editing_event_get_length_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	value = ptr->length;
	current_out = write_sint32(&value, current_out, len_out);
}

void text_editing_event_set_length_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextEditingEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	current_in = read_sint32(current_in, &value);
	ptr->length = value;
}

byte * read_text_input_event(byte *in, SDL_TextInputEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_uint32(current_in, &(result->windowID));
	current_in = read_string(current_in, &(result->text));

	return current_in;
}

byte * write_text_input_event(SDL_TextInputEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_uint32(&(value->windowID), current_out, len);
	current_out = write_string(&(value->text), current_out, len);

	return current_out;
}

void pointer_deref_text_input_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_text_input_event(pointer, current_out, len_out);
}

void pointer_deref_text_input_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_text_input_event(current_in, pointer);
}

void new_text_input_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *ptr = malloc(sizeof(SDL_TextInputEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void text_input_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void text_input_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void text_input_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void text_input_event_get_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->windowID;
	current_out = write_uint32(&value, current_out, len_out);
}

void text_input_event_set_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->windowID = value;
}

void text_input_event_get_text_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	string value;
	strcpy(value, ptr->text);
	current_out = write_string(&value, current_out, len_out);
}

void text_input_event_set_text_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TextInputEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	string value;
	current_in = read_string(current_in, &value);
	strcpy(ptr->text, value);
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

void pointer_deref_mouse_motion_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_mouse_motion_event(pointer, current_out, len_out);
}

void pointer_deref_mouse_motion_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_mouse_motion_event(current_in, pointer);
}

void new_mouse_motion_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr = malloc(sizeof(SDL_MouseMotionEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void mouse_motion_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void mouse_motion_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void mouse_motion_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void mouse_motion_event_get_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->windowID;
	current_out = write_uint32(&value, current_out, len_out);
}

void mouse_motion_event_set_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->windowID = value;
}

void mouse_motion_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->which;
	current_out = write_uint32(&value, current_out, len_out);
}

void mouse_motion_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->which = value;
}

void mouse_motion_event_get_state_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->state;
	current_out = write_uint32(&value, current_out, len_out);
}

void mouse_motion_event_set_state_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->state = value;
}

void mouse_motion_event_get_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	value = ptr->x;
	current_out = write_sint32(&value, current_out, len_out);
}

void mouse_motion_event_set_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	current_in = read_sint32(current_in, &value);
	ptr->x = value;
}

void mouse_motion_event_get_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	value = ptr->y;
	current_out = write_sint32(&value, current_out, len_out);
}

void mouse_motion_event_set_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	current_in = read_sint32(current_in, &value);
	ptr->y = value;
}

void mouse_motion_event_get_xrel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	value = ptr->xrel;
	current_out = write_sint32(&value, current_out, len_out);
}

void mouse_motion_event_set_xrel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	current_in = read_sint32(current_in, &value);
	ptr->xrel = value;
}

void mouse_motion_event_get_yrel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	value = ptr->yrel;
	current_out = write_sint32(&value, current_out, len_out);
}

void mouse_motion_event_set_yrel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseMotionEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	current_in = read_sint32(current_in, &value);
	ptr->yrel = value;
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

void pointer_deref_mouse_button_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_mouse_button_event(pointer, current_out, len_out);
}

void pointer_deref_mouse_button_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_mouse_button_event(current_in, pointer);
}

void new_mouse_button_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr = malloc(sizeof(SDL_MouseButtonEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void mouse_button_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void mouse_button_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void mouse_button_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void mouse_button_event_get_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->windowID;
	current_out = write_uint32(&value, current_out, len_out);
}

void mouse_button_event_set_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->windowID = value;
}

void mouse_button_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->which;
	current_out = write_uint32(&value, current_out, len_out);
}

void mouse_button_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->which = value;
}

void mouse_button_event_get_button_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->button;
	current_out = write_uint8(&value, current_out, len_out);
}

void mouse_button_event_set_button_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->button = value;
}

void mouse_button_event_get_state_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->state;
	current_out = write_uint8(&value, current_out, len_out);
}

void mouse_button_event_set_state_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->state = value;
}

void mouse_button_event_get_clicks_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->clicks;
	current_out = write_uint8(&value, current_out, len_out);
}

void mouse_button_event_set_clicks_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->clicks = value;
}

void mouse_button_event_get_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	value = ptr->x;
	current_out = write_sint32(&value, current_out, len_out);
}

void mouse_button_event_set_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	current_in = read_sint32(current_in, &value);
	ptr->x = value;
}

void mouse_button_event_get_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	value = ptr->y;
	current_out = write_sint32(&value, current_out, len_out);
}

void mouse_button_event_set_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	current_in = read_sint32(current_in, &value);
	ptr->y = value;
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

void pointer_deref_mouse_wheel_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_mouse_wheel_event(pointer, current_out, len_out);
}

void pointer_deref_mouse_wheel_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_mouse_wheel_event(current_in, pointer);
}

void new_mouse_wheel_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr = malloc(sizeof(SDL_MouseWheelEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void mouse_wheel_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void mouse_wheel_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void mouse_wheel_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void mouse_wheel_event_get_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->windowID;
	current_out = write_uint32(&value, current_out, len_out);
}

void mouse_wheel_event_set_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->windowID = value;
}

void mouse_wheel_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->which;
	current_out = write_uint32(&value, current_out, len_out);
}

void mouse_wheel_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->which = value;
}

void mouse_wheel_event_get_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	value = ptr->x;
	current_out = write_sint32(&value, current_out, len_out);
}

void mouse_wheel_event_set_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	current_in = read_sint32(current_in, &value);
	ptr->x = value;
}

void mouse_wheel_event_get_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	value = ptr->y;
	current_out = write_sint32(&value, current_out, len_out);
}

void mouse_wheel_event_set_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	current_in = read_sint32(current_in, &value);
	ptr->y = value;
}

void mouse_wheel_event_get_direction_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->direction;
	current_out = write_uint32(&value, current_out, len_out);
}

void mouse_wheel_event_set_direction_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MouseWheelEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->direction = value;
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

void pointer_deref_joy_axis_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_joy_axis_event(pointer, current_out, len_out);
}

void pointer_deref_joy_axis_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_joy_axis_event(current_in, pointer);
}

void new_joy_axis_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr = malloc(sizeof(SDL_JoyAxisEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void joy_axis_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void joy_axis_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void joy_axis_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void joy_axis_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoystickID value;
	value = ptr->which;
	current_out = write_joystick_id(&value, current_out, len_out);
}

void joy_axis_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoystickID value;
	current_in = read_joystick_id(current_in, &value);
	ptr->which = value;
}

void joy_axis_event_get_axis_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->axis;
	current_out = write_uint8(&value, current_out, len_out);
}

void joy_axis_event_set_axis_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->axis = value;
}

void joy_axis_event_get_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding1;
	current_out = write_uint8(&value, current_out, len_out);
}

void joy_axis_event_set_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding1 = value;
}

void joy_axis_event_get_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding2;
	current_out = write_uint8(&value, current_out, len_out);
}

void joy_axis_event_set_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding2 = value;
}

void joy_axis_event_get_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding3;
	current_out = write_uint8(&value, current_out, len_out);
}

void joy_axis_event_set_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding3 = value;
}

void joy_axis_event_get_value_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint16 value;
	value = ptr->value;
	current_out = write_sint16(&value, current_out, len_out);
}

void joy_axis_event_set_value_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint16 value;
	current_in = read_sint16(current_in, &value);
	ptr->value = value;
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

void pointer_deref_joy_ball_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_joy_ball_event(pointer, current_out, len_out);
}

void pointer_deref_joy_ball_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_joy_ball_event(current_in, pointer);
}

void new_joy_ball_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr = malloc(sizeof(SDL_JoyBallEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void joy_ball_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void joy_ball_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void joy_ball_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void joy_ball_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoystickID value;
	value = ptr->which;
	current_out = write_joystick_id(&value, current_out, len_out);
}

void joy_ball_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoystickID value;
	current_in = read_joystick_id(current_in, &value);
	ptr->which = value;
}

void joy_ball_event_get_ball_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->ball;
	current_out = write_uint8(&value, current_out, len_out);
}

void joy_ball_event_set_ball_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->ball = value;
}

void joy_ball_event_get_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding1;
	current_out = write_uint8(&value, current_out, len_out);
}

void joy_ball_event_set_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding1 = value;
}

void joy_ball_event_get_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding2;
	current_out = write_uint8(&value, current_out, len_out);
}

void joy_ball_event_set_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding2 = value;
}

void joy_ball_event_get_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding3;
	current_out = write_uint8(&value, current_out, len_out);
}

void joy_ball_event_set_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding3 = value;
}

void joy_ball_event_get_xrel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint16 value;
	value = ptr->xrel;
	current_out = write_sint16(&value, current_out, len_out);
}

void joy_ball_event_set_xrel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint16 value;
	current_in = read_sint16(current_in, &value);
	ptr->xrel = value;
}

void joy_ball_event_get_yrel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint16 value;
	value = ptr->yrel;
	current_out = write_sint16(&value, current_out, len_out);
}

void joy_ball_event_set_yrel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyBallEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint16 value;
	current_in = read_sint16(current_in, &value);
	ptr->yrel = value;
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

void pointer_deref_joy_hat_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_joy_hat_event(pointer, current_out, len_out);
}

void pointer_deref_joy_hat_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_joy_hat_event(current_in, pointer);
}

void new_joy_hat_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr = malloc(sizeof(SDL_JoyHatEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void joy_hat_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void joy_hat_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void joy_hat_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void joy_hat_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoystickID value;
	value = ptr->which;
	current_out = write_joystick_id(&value, current_out, len_out);
}

void joy_hat_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoystickID value;
	current_in = read_joystick_id(current_in, &value);
	ptr->which = value;
}

void joy_hat_event_get_hat_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->hat;
	current_out = write_uint8(&value, current_out, len_out);
}

void joy_hat_event_set_hat_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->hat = value;
}

void joy_hat_event_get_value_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->value;
	current_out = write_uint8(&value, current_out, len_out);
}

void joy_hat_event_set_value_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->value = value;
}

void joy_hat_event_get_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding1;
	current_out = write_uint8(&value, current_out, len_out);
}

void joy_hat_event_set_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding1 = value;
}

void joy_hat_event_get_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding2;
	current_out = write_uint8(&value, current_out, len_out);
}

void joy_hat_event_set_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyHatEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding2 = value;
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

void pointer_deref_joy_button_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_joy_button_event(pointer, current_out, len_out);
}

void pointer_deref_joy_button_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_joy_button_event(current_in, pointer);
}

void new_joy_button_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr = malloc(sizeof(SDL_JoyButtonEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void joy_button_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void joy_button_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void joy_button_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void joy_button_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoystickID value;
	value = ptr->which;
	current_out = write_joystick_id(&value, current_out, len_out);
}

void joy_button_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoystickID value;
	current_in = read_joystick_id(current_in, &value);
	ptr->which = value;
}

void joy_button_event_get_button_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->button;
	current_out = write_uint8(&value, current_out, len_out);
}

void joy_button_event_set_button_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->button = value;
}

void joy_button_event_get_state_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->state;
	current_out = write_uint8(&value, current_out, len_out);
}

void joy_button_event_set_state_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->state = value;
}

void joy_button_event_get_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding1;
	current_out = write_uint8(&value, current_out, len_out);
}

void joy_button_event_set_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding1 = value;
}

void joy_button_event_get_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding2;
	current_out = write_uint8(&value, current_out, len_out);
}

void joy_button_event_set_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding2 = value;
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

void pointer_deref_joy_device_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyDeviceEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_joy_device_event(pointer, current_out, len_out);
}

void pointer_deref_joy_device_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyDeviceEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_joy_device_event(current_in, pointer);
}

void new_joy_device_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyDeviceEvent *ptr = malloc(sizeof(SDL_JoyDeviceEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void joy_device_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void joy_device_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void joy_device_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void joy_device_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	value = ptr->which;
	current_out = write_sint32(&value, current_out, len_out);
}

void joy_device_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_JoyDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	current_in = read_sint32(current_in, &value);
	ptr->which = value;
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

void pointer_deref_controller_axis_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_controller_axis_event(pointer, current_out, len_out);
}

void pointer_deref_controller_axis_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_controller_axis_event(current_in, pointer);
}

void new_controller_axis_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr = malloc(sizeof(SDL_ControllerAxisEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void controller_axis_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void controller_axis_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void controller_axis_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void controller_axis_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoystickID value;
	value = ptr->which;
	current_out = write_joystick_id(&value, current_out, len_out);
}

void controller_axis_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoystickID value;
	current_in = read_joystick_id(current_in, &value);
	ptr->which = value;
}

void controller_axis_event_get_axis_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->axis;
	current_out = write_uint8(&value, current_out, len_out);
}

void controller_axis_event_set_axis_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->axis = value;
}

void controller_axis_event_get_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding1;
	current_out = write_uint8(&value, current_out, len_out);
}

void controller_axis_event_set_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding1 = value;
}

void controller_axis_event_get_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding2;
	current_out = write_uint8(&value, current_out, len_out);
}

void controller_axis_event_set_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding2 = value;
}

void controller_axis_event_get_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding3;
	current_out = write_uint8(&value, current_out, len_out);
}

void controller_axis_event_set_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding3 = value;
}

void controller_axis_event_get_value_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint16 value;
	value = ptr->value;
	current_out = write_sint16(&value, current_out, len_out);
}

void controller_axis_event_set_value_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint16 value;
	current_in = read_sint16(current_in, &value);
	ptr->value = value;
}

void controller_axis_event_get_padding4_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint16 value;
	value = ptr->padding4;
	current_out = write_uint16(&value, current_out, len_out);
}

void controller_axis_event_set_padding4_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerAxisEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint16 value;
	current_in = read_uint16(current_in, &value);
	ptr->padding4 = value;
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

void pointer_deref_controller_button_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_controller_button_event(pointer, current_out, len_out);
}

void pointer_deref_controller_button_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_controller_button_event(current_in, pointer);
}

void new_controller_button_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr = malloc(sizeof(SDL_ControllerButtonEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void controller_button_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void controller_button_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void controller_button_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void controller_button_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoystickID value;
	value = ptr->which;
	current_out = write_joystick_id(&value, current_out, len_out);
}

void controller_button_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoystickID value;
	current_in = read_joystick_id(current_in, &value);
	ptr->which = value;
}

void controller_button_event_get_button_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->button;
	current_out = write_uint8(&value, current_out, len_out);
}

void controller_button_event_set_button_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->button = value;
}

void controller_button_event_get_state_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->state;
	current_out = write_uint8(&value, current_out, len_out);
}

void controller_button_event_set_state_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->state = value;
}

void controller_button_event_get_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding1;
	current_out = write_uint8(&value, current_out, len_out);
}

void controller_button_event_set_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding1 = value;
}

void controller_button_event_get_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding2;
	current_out = write_uint8(&value, current_out, len_out);
}

void controller_button_event_set_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerButtonEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding2 = value;
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

void pointer_deref_controller_device_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerDeviceEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_controller_device_event(pointer, current_out, len_out);
}

void pointer_deref_controller_device_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerDeviceEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_controller_device_event(current_in, pointer);
}

void new_controller_device_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerDeviceEvent *ptr = malloc(sizeof(SDL_ControllerDeviceEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void controller_device_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void controller_device_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void controller_device_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void controller_device_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	value = ptr->which;
	current_out = write_sint32(&value, current_out, len_out);
}

void controller_device_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_ControllerDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	current_in = read_sint32(current_in, &value);
	ptr->which = value;
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

void pointer_deref_audio_device_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_audio_device_event(pointer, current_out, len_out);
}

void pointer_deref_audio_device_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_audio_device_event(current_in, pointer);
}

void new_audio_device_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr = malloc(sizeof(SDL_AudioDeviceEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void audio_device_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void audio_device_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void audio_device_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void audio_device_event_get_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->which;
	current_out = write_uint32(&value, current_out, len_out);
}

void audio_device_event_set_which_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->which = value;
}

void audio_device_event_get_iscapture_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->iscapture;
	current_out = write_uint8(&value, current_out, len_out);
}

void audio_device_event_set_iscapture_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->iscapture = value;
}

void audio_device_event_get_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding1;
	current_out = write_uint8(&value, current_out, len_out);
}

void audio_device_event_set_padding1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding1 = value;
}

void audio_device_event_get_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding2;
	current_out = write_uint8(&value, current_out, len_out);
}

void audio_device_event_set_padding2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding2 = value;
}

void audio_device_event_get_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	value = ptr->padding3;
	current_out = write_uint8(&value, current_out, len_out);
}

void audio_device_event_set_padding3_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_AudioDeviceEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint8 value;
	current_in = read_uint8(current_in, &value);
	ptr->padding3 = value;
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

void pointer_deref_quit_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_QuitEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_quit_event(pointer, current_out, len_out);
}

void pointer_deref_quit_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_QuitEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_quit_event(current_in, pointer);
}

void new_quit_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_QuitEvent *ptr = malloc(sizeof(SDL_QuitEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void quit_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_QuitEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void quit_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_QuitEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void quit_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_QuitEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

byte * read_user_event(byte *in, SDL_UserEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_uint32(current_in, &(result->windowID));
	current_in = read_sint32(current_in, &(result->code));
	current_in = read_pointer(current_in, &(result->data1));
	current_in = read_pointer(current_in, &(result->data2));

	return current_in;
}

byte * write_user_event(SDL_UserEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_uint32(&(value->windowID), current_out, len);
	current_out = write_sint32(&(value->code), current_out, len);
	current_out = write_pointer(&(value->data1), current_out, len);
	current_out = write_pointer(&(value->data2), current_out, len);

	return current_out;
}

void pointer_deref_user_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_user_event(pointer, current_out, len_out);
}

void pointer_deref_user_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_user_event(current_in, pointer);
}

void new_user_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr = malloc(sizeof(SDL_UserEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void user_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void user_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void user_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void user_event_get_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->windowID;
	current_out = write_uint32(&value, current_out, len_out);
}

void user_event_set_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->windowID = value;
}

void user_event_get_code_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	value = ptr->code;
	current_out = write_sint32(&value, current_out, len_out);
}

void user_event_set_code_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Sint32 value;
	current_in = read_sint32(current_in, &value);
	ptr->code = value;
}

void user_event_get_data1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	void* value;
	value = ptr->data1;
	current_out = write_pointer(&value, current_out, len_out);
}

void user_event_set_data1_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	void* value;
	current_in = read_pointer(current_in, &value);
	ptr->data1 = value;
}

void user_event_get_data2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	void* value;
	value = ptr->data2;
	current_out = write_pointer(&value, current_out, len_out);
}

void user_event_set_data2_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_UserEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	void* value;
	current_in = read_pointer(current_in, &value);
	ptr->data2 = value;
}

byte * read_syswm_event(byte *in, SDL_SysWMEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_pointer(current_in, &(result->msg));

	return current_in;
}

byte * write_syswm_event(SDL_SysWMEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_pointer(&(value->msg), current_out, len);

	return current_out;
}

void pointer_deref_syswm_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_SysWMEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_syswm_event(pointer, current_out, len_out);
}

void pointer_deref_syswm_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_SysWMEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_syswm_event(current_in, pointer);
}

void new_syswm_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_SysWMEvent *ptr = malloc(sizeof(SDL_SysWMEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void syswm_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_SysWMEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void syswm_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_SysWMEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void syswm_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_SysWMEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void syswm_event_get_msg_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_SysWMEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_SysWMmsg* value;
	value = ptr->msg;
	current_out = write_pointer(&value, current_out, len_out);
}

void syswm_event_set_msg_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_SysWMEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_SysWMmsg* value;
	current_in = read_pointer(current_in, &value);
	ptr->msg = value;
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

void pointer_deref_touch_finger_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_touch_finger_event(pointer, current_out, len_out);
}

void pointer_deref_touch_finger_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_touch_finger_event(current_in, pointer);
}

void new_touch_finger_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr = malloc(sizeof(SDL_TouchFingerEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void touch_finger_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void touch_finger_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void touch_finger_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void touch_finger_event_get_touchId_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_TouchID value;
	value = ptr->touchId;
	current_out = write_touch_id(&value, current_out, len_out);
}

void touch_finger_event_set_touchId_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_TouchID value;
	current_in = read_touch_id(current_in, &value);
	ptr->touchId = value;
}

void touch_finger_event_get_fingerId_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_FingerID value;
	value = ptr->fingerId;
	current_out = write_finger_id(&value, current_out, len_out);
}

void touch_finger_event_set_fingerId_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_FingerID value;
	current_in = read_finger_id(current_in, &value);
	ptr->fingerId = value;
}

void touch_finger_event_get_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	value = ptr->x;
	current_out = write_float(&value, current_out, len_out);
}

void touch_finger_event_set_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	current_in = read_float(current_in, &value);
	ptr->x = value;
}

void touch_finger_event_get_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	value = ptr->y;
	current_out = write_float(&value, current_out, len_out);
}

void touch_finger_event_set_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	current_in = read_float(current_in, &value);
	ptr->y = value;
}

void touch_finger_event_get_dx_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	value = ptr->dx;
	current_out = write_float(&value, current_out, len_out);
}

void touch_finger_event_set_dx_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	current_in = read_float(current_in, &value);
	ptr->dx = value;
}

void touch_finger_event_get_dy_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	value = ptr->dy;
	current_out = write_float(&value, current_out, len_out);
}

void touch_finger_event_set_dy_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	current_in = read_float(current_in, &value);
	ptr->dy = value;
}

void touch_finger_event_get_pressure_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	value = ptr->pressure;
	current_out = write_float(&value, current_out, len_out);
}

void touch_finger_event_set_pressure_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_TouchFingerEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	current_in = read_float(current_in, &value);
	ptr->pressure = value;
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

void pointer_deref_multi_gesture_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_multi_gesture_event(pointer, current_out, len_out);
}

void pointer_deref_multi_gesture_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_multi_gesture_event(current_in, pointer);
}

void new_multi_gesture_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr = malloc(sizeof(SDL_MultiGestureEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void multi_gesture_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void multi_gesture_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void multi_gesture_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void multi_gesture_event_get_touchId_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_TouchID value;
	value = ptr->touchId;
	current_out = write_touch_id(&value, current_out, len_out);
}

void multi_gesture_event_set_touchId_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_TouchID value;
	current_in = read_touch_id(current_in, &value);
	ptr->touchId = value;
}

void multi_gesture_event_get_dTheta_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	value = ptr->dTheta;
	current_out = write_float(&value, current_out, len_out);
}

void multi_gesture_event_set_dTheta_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	current_in = read_float(current_in, &value);
	ptr->dTheta = value;
}

void multi_gesture_event_get_dDist_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	value = ptr->dDist;
	current_out = write_float(&value, current_out, len_out);
}

void multi_gesture_event_set_dDist_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	current_in = read_float(current_in, &value);
	ptr->dDist = value;
}

void multi_gesture_event_get_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	value = ptr->x;
	current_out = write_float(&value, current_out, len_out);
}

void multi_gesture_event_set_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	current_in = read_float(current_in, &value);
	ptr->x = value;
}

void multi_gesture_event_get_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	value = ptr->y;
	current_out = write_float(&value, current_out, len_out);
}

void multi_gesture_event_set_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	current_in = read_float(current_in, &value);
	ptr->y = value;
}

void multi_gesture_event_get_numFingers_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint16 value;
	value = ptr->numFingers;
	current_out = write_uint16(&value, current_out, len_out);
}

void multi_gesture_event_set_numFingers_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint16 value;
	current_in = read_uint16(current_in, &value);
	ptr->numFingers = value;
}

void multi_gesture_event_get_padding_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint16 value;
	value = ptr->padding;
	current_out = write_uint16(&value, current_out, len_out);
}

void multi_gesture_event_set_padding_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_MultiGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint16 value;
	current_in = read_uint16(current_in, &value);
	ptr->padding = value;
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

void pointer_deref_dollar_gesture_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_dollar_gesture_event(pointer, current_out, len_out);
}

void pointer_deref_dollar_gesture_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_dollar_gesture_event(current_in, pointer);
}

void new_dollar_gesture_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr = malloc(sizeof(SDL_DollarGestureEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void dollar_gesture_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void dollar_gesture_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void dollar_gesture_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void dollar_gesture_event_get_touchId_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_TouchID value;
	value = ptr->touchId;
	current_out = write_touch_id(&value, current_out, len_out);
}

void dollar_gesture_event_set_touchId_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_TouchID value;
	current_in = read_touch_id(current_in, &value);
	ptr->touchId = value;
}

void dollar_gesture_event_get_gestureId_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_GestureID value;
	value = ptr->gestureId;
	current_out = write_gesture_id(&value, current_out, len_out);
}

void dollar_gesture_event_set_gestureId_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_GestureID value;
	current_in = read_gesture_id(current_in, &value);
	ptr->gestureId = value;
}

void dollar_gesture_event_get_numFingers_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->numFingers;
	current_out = write_uint32(&value, current_out, len_out);
}

void dollar_gesture_event_set_numFingers_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->numFingers = value;
}

void dollar_gesture_event_get_error_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	value = ptr->error;
	current_out = write_float(&value, current_out, len_out);
}

void dollar_gesture_event_set_error_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	current_in = read_float(current_in, &value);
	ptr->error = value;
}

void dollar_gesture_event_get_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	value = ptr->x;
	current_out = write_float(&value, current_out, len_out);
}

void dollar_gesture_event_set_x_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	current_in = read_float(current_in, &value);
	ptr->x = value;
}

void dollar_gesture_event_get_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	value = ptr->y;
	current_out = write_float(&value, current_out, len_out);
}

void dollar_gesture_event_set_y_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DollarGestureEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	float value;
	current_in = read_float(current_in, &value);
	ptr->y = value;
}

byte * read_drop_event(byte *in, SDL_DropEvent *result) {
	byte *current_in = in;

	current_in = read_uint32(current_in, &(result->type));
	current_in = read_uint32(current_in, &(result->timestamp));
	current_in = read_string(current_in, &(result->file));
	current_in = read_uint32(current_in, &(result->windowID));

	return current_in;
}

byte * write_drop_event(SDL_DropEvent *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_uint32(&(value->type), current_out, len);
	current_out = write_uint32(&(value->timestamp), current_out, len);
	current_out = write_string(&(value->file), current_out, len);
	current_out = write_uint32(&(value->windowID), current_out, len);

	return current_out;
}

void pointer_deref_drop_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_drop_event(pointer, current_out, len_out);
}

void pointer_deref_drop_event_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_drop_event(current_in, pointer);
}

void new_drop_event_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *ptr = malloc(sizeof(SDL_DropEvent));
	current_out = write_pointer(&ptr, current_out, len_out);
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
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void drop_event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void drop_event_get_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->timestamp;
	current_out = write_uint32(&value, current_out, len_out);
}

void drop_event_set_timestamp_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->timestamp = value;
}

void drop_event_get_file_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	string value;
	strcpy(value, ptr->file);
	current_out = write_string(&value, current_out, len_out);
}

void drop_event_set_file_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	string value;
	current_in = read_string(current_in, &value);
	strcpy(ptr->file, value);
}

void drop_event_get_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->windowID;
	current_out = write_uint32(&value, current_out, len_out);
}

void drop_event_set_windowID_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_DropEvent *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->windowID = value;
}

byte * read_event(byte *in, SDL_Event *result) {
	byte *current_in = in;

	current_in = read_pointer(current_in, (void **) &result);

	return current_in;
}

byte * write_event(SDL_Event *value, byte *out, size_t *len) {
	byte *current_out = out;

	current_out = write_pointer(&value, current_out, len);

	return current_out;
}

void event_get_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	value = ptr->type;
	current_out = write_uint32(&value, current_out, len_out);
}

void event_set_type_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	Uint32 value;
	current_in = read_uint32(current_in, &value);
	ptr->type = value;
}

void event_get_common_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_CommonEvent value;
	value = ptr->common;
	current_out = write_common_event(&value, current_out, len_out);
}

void event_set_common_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_CommonEvent value;
	current_in = read_common_event(current_in, &value);
	ptr->common = value;
}

void event_get_window_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_WindowEvent value;
	value = ptr->window;
	current_out = write_window_event(&value, current_out, len_out);
}

void event_set_window_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_WindowEvent value;
	current_in = read_window_event(current_in, &value);
	ptr->window = value;
}

void event_get_key_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_KeyboardEvent value;
	value = ptr->key;
	current_out = write_keyboard_event(&value, current_out, len_out);
}

void event_set_key_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_KeyboardEvent value;
	current_in = read_keyboard_event(current_in, &value);
	ptr->key = value;
}

void event_get_edit_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_TextEditingEvent value;
	value = ptr->edit;
	current_out = write_text_editing_event(&value, current_out, len_out);
}

void event_set_edit_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_TextEditingEvent value;
	current_in = read_text_editing_event(current_in, &value);
	ptr->edit = value;
}

void event_get_text_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_TextInputEvent value;
	value = ptr->text;
	current_out = write_text_input_event(&value, current_out, len_out);
}

void event_set_text_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_TextInputEvent value;
	current_in = read_text_input_event(current_in, &value);
	ptr->text = value;
}

void event_get_motion_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_MouseMotionEvent value;
	value = ptr->motion;
	current_out = write_mouse_motion_event(&value, current_out, len_out);
}

void event_set_motion_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_MouseMotionEvent value;
	current_in = read_mouse_motion_event(current_in, &value);
	ptr->motion = value;
}

void event_get_button_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_MouseButtonEvent value;
	value = ptr->button;
	current_out = write_mouse_button_event(&value, current_out, len_out);
}

void event_set_button_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_MouseButtonEvent value;
	current_in = read_mouse_button_event(current_in, &value);
	ptr->button = value;
}

void event_get_wheel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_MouseWheelEvent value;
	value = ptr->wheel;
	current_out = write_mouse_wheel_event(&value, current_out, len_out);
}

void event_set_wheel_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_MouseWheelEvent value;
	current_in = read_mouse_wheel_event(current_in, &value);
	ptr->wheel = value;
}

void event_get_jaxis_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoyAxisEvent value;
	value = ptr->jaxis;
	current_out = write_joy_axis_event(&value, current_out, len_out);
}

void event_set_jaxis_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoyAxisEvent value;
	current_in = read_joy_axis_event(current_in, &value);
	ptr->jaxis = value;
}

void event_get_jball_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoyBallEvent value;
	value = ptr->jball;
	current_out = write_joy_ball_event(&value, current_out, len_out);
}

void event_set_jball_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoyBallEvent value;
	current_in = read_joy_ball_event(current_in, &value);
	ptr->jball = value;
}

void event_get_jhat_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoyHatEvent value;
	value = ptr->jhat;
	current_out = write_joy_hat_event(&value, current_out, len_out);
}

void event_set_jhat_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoyHatEvent value;
	current_in = read_joy_hat_event(current_in, &value);
	ptr->jhat = value;
}

void event_get_jbutton_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoyButtonEvent value;
	value = ptr->jbutton;
	current_out = write_joy_button_event(&value, current_out, len_out);
}

void event_set_jbutton_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoyButtonEvent value;
	current_in = read_joy_button_event(current_in, &value);
	ptr->jbutton = value;
}

void event_get_jdevice_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoyDeviceEvent value;
	value = ptr->jdevice;
	current_out = write_joy_device_event(&value, current_out, len_out);
}

void event_set_jdevice_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_JoyDeviceEvent value;
	current_in = read_joy_device_event(current_in, &value);
	ptr->jdevice = value;
}

void event_get_caxis_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_ControllerAxisEvent value;
	value = ptr->caxis;
	current_out = write_controller_axis_event(&value, current_out, len_out);
}

void event_set_caxis_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_ControllerAxisEvent value;
	current_in = read_controller_axis_event(current_in, &value);
	ptr->caxis = value;
}

void event_get_cbutton_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_ControllerButtonEvent value;
	value = ptr->cbutton;
	current_out = write_controller_button_event(&value, current_out, len_out);
}

void event_set_cbutton_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_ControllerButtonEvent value;
	current_in = read_controller_button_event(current_in, &value);
	ptr->cbutton = value;
}

void event_get_cdevice_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_ControllerDeviceEvent value;
	value = ptr->cdevice;
	current_out = write_controller_device_event(&value, current_out, len_out);
}

void event_set_cdevice_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_ControllerDeviceEvent value;
	current_in = read_controller_device_event(current_in, &value);
	ptr->cdevice = value;
}

void event_get_adevice_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_AudioDeviceEvent value;
	value = ptr->adevice;
	current_out = write_audio_device_event(&value, current_out, len_out);
}

void event_set_adevice_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_AudioDeviceEvent value;
	current_in = read_audio_device_event(current_in, &value);
	ptr->adevice = value;
}

void event_get_quit_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_QuitEvent value;
	value = ptr->quit;
	current_out = write_quit_event(&value, current_out, len_out);
}

void event_set_quit_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_QuitEvent value;
	current_in = read_quit_event(current_in, &value);
	ptr->quit = value;
}

void event_get_user_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_UserEvent value;
	value = ptr->user;
	current_out = write_user_event(&value, current_out, len_out);
}

void event_set_user_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_UserEvent value;
	current_in = read_user_event(current_in, &value);
	ptr->user = value;
}

void event_get_syswm_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_SysWMEvent value;
	value = ptr->syswm;
	current_out = write_syswm_event(&value, current_out, len_out);
}

void event_set_syswm_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_SysWMEvent value;
	current_in = read_syswm_event(current_in, &value);
	ptr->syswm = value;
}

void event_get_tfinger_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_TouchFingerEvent value;
	value = ptr->tfinger;
	current_out = write_touch_finger_event(&value, current_out, len_out);
}

void event_set_tfinger_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_TouchFingerEvent value;
	current_in = read_touch_finger_event(current_in, &value);
	ptr->tfinger = value;
}

void event_get_mgesture_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_MultiGestureEvent value;
	value = ptr->mgesture;
	current_out = write_multi_gesture_event(&value, current_out, len_out);
}

void event_set_mgesture_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_MultiGestureEvent value;
	current_in = read_multi_gesture_event(current_in, &value);
	ptr->mgesture = value;
}

void event_get_dgesture_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_DollarGestureEvent value;
	value = ptr->dgesture;
	current_out = write_dollar_gesture_event(&value, current_out, len_out);
}

void event_set_dgesture_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_DollarGestureEvent value;
	current_in = read_dollar_gesture_event(current_in, &value);
	ptr->dgesture = value;
}

void event_get_drop_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_DropEvent value;
	value = ptr->drop;
	current_out = write_drop_event(&value, current_out, len_out);
}

void event_set_drop_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	SDL_DropEvent value;
	current_in = read_drop_event(current_in, &value);
	ptr->drop = value;
}

//--------------------------------------------------------

void SDL_Init_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	Uint32 var1;
	current_in = read_uint32(current_in, &var1);

	int retvar = SDL_Init(var1);
	current_out = write_int(&retvar, current_out, len_out);
}

void SDL_Quit_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Quit();
}

void SDL_CreateWindow_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
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
	current_out = write_pointer(&retvar, current_out, len_out);
}

void SDL_GetWindowSurface_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Window *var1;
	current_in = read_pointer(current_in, &var1);

	SDL_Surface *retvar = SDL_GetWindowSurface(var1);
	current_out = write_pointer(&retvar, current_out, len_out);
}

void SDL_LoadBMP_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	string var1;
	current_in = read_string(current_in, &var1);

	SDL_Surface *retvar = SDL_LoadBMP(var1);
	current_out = write_pointer(&retvar, current_out, len_out);
}

void SDL_FreeSurface_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *var1;
	current_in = read_pointer(current_in, &var1);

	SDL_FreeSurface(var1);
}

void SDL_BlitSurface_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *var1;
	current_in = read_pointer(current_in, &var1);
	SDL_Rect *var2;
	current_in = read_pointer(current_in, &var2);
	SDL_Surface *var3;
	current_in = read_pointer(current_in, &var3);
	SDL_Rect *var4;
	current_in = read_pointer(current_in, &var4);

	int retvar = SDL_BlitSurface(var1, var2, var3, var4);
	current_out = write_int(&retvar, current_out, len_out);
}

void SDL_BlitScaled_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Surface *var1;
	current_in = read_pointer(current_in, &var1);
	SDL_Rect *var2;
	current_in = read_pointer(current_in, &var2);
	SDL_Surface *var3;
	current_in = read_pointer(current_in, &var3);
	SDL_Rect *var4;
	current_in = read_pointer(current_in, &var4);

	int retvar = SDL_BlitScaled(var1, var2, var3, var4);
	current_out = write_int(&retvar, current_out, len_out);
}

void SDL_UpdateWindowSurface_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Window *var1;
	current_in = read_pointer(current_in, &var1);

	int retvar = SDL_UpdateWindowSurface(var1);
	current_out = write_int(&retvar, current_out, len_out);
}

void SDL_DestroyWindow_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Window *var1;
	current_in = read_pointer(current_in, &var1);

	SDL_DestroyWindow(var1);
}

void SDL_GetWindowSize_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Window *var1;
	current_in = read_pointer(current_in, &var1);
	int *var2 = malloc(sizeof(int));
	int *var3 = malloc(sizeof(int));

	SDL_GetWindowSize(var1, var2, var3);
	current_out = write_int(var2, current_out, len_out);
	free(var2);
	current_out = write_int(var3, current_out, len_out);
	free(var3);
}

void SDL_GetError_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	string retvar;
	strcpy(retvar, SDL_GetError());
	current_out = write_string(&retvar, current_out, len_out);
}

void SDL_PollEvent_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	SDL_Event *var1 = malloc(sizeof(SDL_Event));

	int retvar = SDL_PollEvent(var1);
	current_out = write_int(&retvar, current_out, len_out);
	current_out = write_event(var1, current_out, len_out);
	free(var1);
}

handler handlers[] = {
	0,
	pointer_deref_int_Handler,		//1
	pointer_deref_int_assign_Handler,		//2
	new_int_Handler,		//3
	delete_int_Handler,		//4
	pointer_deref_float_Handler,		//5
	pointer_deref_float_assign_Handler,		//6
	new_float_Handler,		//7
	delete_float_Handler,		//8
	pointer_deref_double_Handler,		//9
	pointer_deref_double_assign_Handler,		//10
	new_double_Handler,		//11
	delete_double_Handler,		//12
	pointer_deref_string_Handler,		//13
	pointer_deref_string_assign_Handler,		//14
	new_string_Handler,		//15
	delete_string_Handler,		//16
	pointer_deref_color_Handler,		//17
	pointer_deref_color_assign_Handler,		//18
	new_color_Handler,		//19
	delete_color_Handler,		//20
	color_get_r_Handler,		//21
	color_set_r_Handler,		//22
	color_get_g_Handler,		//23
	color_set_g_Handler,		//24
	color_get_b_Handler,		//25
	color_set_b_Handler,		//26
	color_get_a_Handler,		//27
	color_set_a_Handler,		//28
	pointer_deref_palette_Handler,		//29
	pointer_deref_palette_assign_Handler,		//30
	new_palette_Handler,		//31
	delete_palette_Handler,		//32
	palette_get_ncolors_Handler,		//33
	palette_set_ncolors_Handler,		//34
	palette_get_colors_Handler,		//35
	palette_set_colors_Handler,		//36
	palette_get_version_Handler,		//37
	palette_set_version_Handler,		//38
	palette_get_refcount_Handler,		//39
	palette_set_refcount_Handler,		//40
	pointer_deref_pixel_format_Handler,		//41
	pointer_deref_pixel_format_assign_Handler,		//42
	new_pixel_format_Handler,		//43
	delete_pixel_format_Handler,		//44
	pixel_format_get_format_Handler,		//45
	pixel_format_set_format_Handler,		//46
	pixel_format_get_palette_Handler,		//47
	pixel_format_set_palette_Handler,		//48
	pixel_format_get_bits_per_pixel_Handler,		//49
	pixel_format_set_bits_per_pixel_Handler,		//50
	pixel_format_get_bytes_per_pixel_Handler,		//51
	pixel_format_set_bytes_per_pixel_Handler,		//52
	pixel_format_get_r_mask_Handler,		//53
	pixel_format_set_r_mask_Handler,		//54
	pixel_format_get_g_mask_Handler,		//55
	pixel_format_set_g_mask_Handler,		//56
	pixel_format_get_b_mask_Handler,		//57
	pixel_format_set_b_mask_Handler,		//58
	pixel_format_get_a_mask_Handler,		//59
	pixel_format_set_a_mask_Handler,		//60
	pixel_format_get_r_loss_Handler,		//61
	pixel_format_set_r_loss_Handler,		//62
	pixel_format_get_g_loss_Handler,		//63
	pixel_format_set_g_loss_Handler,		//64
	pixel_format_get_b_loss_Handler,		//65
	pixel_format_set_b_loss_Handler,		//66
	pixel_format_get_a_loss_Handler,		//67
	pixel_format_set_a_loss_Handler,		//68
	pixel_format_get_r_shift_Handler,		//69
	pixel_format_set_r_shift_Handler,		//70
	pixel_format_get_g_shift_Handler,		//71
	pixel_format_set_g_shift_Handler,		//72
	pixel_format_get_b_shift_Handler,		//73
	pixel_format_set_b_shift_Handler,		//74
	pixel_format_get_a_shift_Handler,		//75
	pixel_format_set_a_shift_Handler,		//76
	pixel_format_get_refcount_Handler,		//77
	pixel_format_set_refcount_Handler,		//78
	pixel_format_get_next_Handler,		//79
	pixel_format_set_next_Handler,		//80
	pointer_deref_rect_Handler,		//81
	pointer_deref_rect_assign_Handler,		//82
	new_rect_Handler,		//83
	delete_rect_Handler,		//84
	rect_get_x_Handler,		//85
	rect_set_x_Handler,		//86
	rect_get_y_Handler,		//87
	rect_set_y_Handler,		//88
	rect_get_w_Handler,		//89
	rect_set_w_Handler,		//90
	rect_get_h_Handler,		//91
	rect_set_h_Handler,		//92
	pointer_deref_surface_Handler,		//93
	pointer_deref_surface_assign_Handler,		//94
	new_surface_Handler,		//95
	delete_surface_Handler,		//96
	surface_get_flags_Handler,		//97
	surface_set_flags_Handler,		//98
	surface_get_format_Handler,		//99
	surface_set_format_Handler,		//100
	surface_get_w_Handler,		//101
	surface_set_w_Handler,		//102
	surface_get_h_Handler,		//103
	surface_set_h_Handler,		//104
	surface_get_pitch_Handler,		//105
	surface_set_pitch_Handler,		//106
	surface_get_pixels_Handler,		//107
	surface_set_pixels_Handler,		//108
	surface_get_userdata_Handler,		//109
	surface_set_userdata_Handler,		//110
	surface_get_locked_Handler,		//111
	surface_set_locked_Handler,		//112
	surface_get_lock_data_Handler,		//113
	surface_set_lock_data_Handler,		//114
	surface_get_clip_rect_Handler,		//115
	surface_set_clip_rect_Handler,		//116
	surface_get_map_Handler,		//117
	surface_set_map_Handler,		//118
	surface_get_refcount_Handler,		//119
	surface_set_refcount_Handler,		//120
	pointer_deref_keysym_Handler,		//121
	pointer_deref_keysym_assign_Handler,		//122
	new_keysym_Handler,		//123
	delete_keysym_Handler,		//124
	keysym_get_scancode_Handler,		//125
	keysym_set_scancode_Handler,		//126
	keysym_get_sym_Handler,		//127
	keysym_set_sym_Handler,		//128
	keysym_get_mod_Handler,		//129
	keysym_set_mod_Handler,		//130
	keysym_get_unused_Handler,		//131
	keysym_set_unused_Handler,		//132
	pointer_deref_common_event_Handler,		//133
	pointer_deref_common_event_assign_Handler,		//134
	new_common_event_Handler,		//135
	delete_common_event_Handler,		//136
	common_event_get_type_Handler,		//137
	common_event_set_type_Handler,		//138
	common_event_get_timestamp_Handler,		//139
	common_event_set_timestamp_Handler,		//140
	pointer_deref_window_event_Handler,		//141
	pointer_deref_window_event_assign_Handler,		//142
	new_window_event_Handler,		//143
	delete_window_event_Handler,		//144
	window_event_get_type_Handler,		//145
	window_event_set_type_Handler,		//146
	window_event_get_timestamp_Handler,		//147
	window_event_set_timestamp_Handler,		//148
	window_event_get_windowID_Handler,		//149
	window_event_set_windowID_Handler,		//150
	window_event_get_event_Handler,		//151
	window_event_set_event_Handler,		//152
	window_event_get_padding1_Handler,		//153
	window_event_set_padding1_Handler,		//154
	window_event_get_padding2_Handler,		//155
	window_event_set_padding2_Handler,		//156
	window_event_get_padding3_Handler,		//157
	window_event_set_padding3_Handler,		//158
	window_event_get_data1_Handler,		//159
	window_event_set_data1_Handler,		//160
	window_event_get_data2_Handler,		//161
	window_event_set_data2_Handler,		//162
	pointer_deref_keyboard_event_Handler,		//163
	pointer_deref_keyboard_event_assign_Handler,		//164
	new_keyboard_event_Handler,		//165
	delete_keyboard_event_Handler,		//166
	keyboard_event_get_type_Handler,		//167
	keyboard_event_set_type_Handler,		//168
	keyboard_event_get_timestamp_Handler,		//169
	keyboard_event_set_timestamp_Handler,		//170
	keyboard_event_get_windowID_Handler,		//171
	keyboard_event_set_windowID_Handler,		//172
	keyboard_event_get_state_Handler,		//173
	keyboard_event_set_state_Handler,		//174
	keyboard_event_get_repeat_Handler,		//175
	keyboard_event_set_repeat_Handler,		//176
	keyboard_event_get_padding2_Handler,		//177
	keyboard_event_set_padding2_Handler,		//178
	keyboard_event_get_padding3_Handler,		//179
	keyboard_event_set_padding3_Handler,		//180
	keyboard_event_get_keysym_Handler,		//181
	keyboard_event_set_keysym_Handler,		//182
	pointer_deref_text_editing_event_Handler,		//183
	pointer_deref_text_editing_event_assign_Handler,		//184
	new_text_editing_event_Handler,		//185
	delete_text_editing_event_Handler,		//186
	text_editing_event_get_type_Handler,		//187
	text_editing_event_set_type_Handler,		//188
	text_editing_event_get_timestamp_Handler,		//189
	text_editing_event_set_timestamp_Handler,		//190
	text_editing_event_get_windowID_Handler,		//191
	text_editing_event_set_windowID_Handler,		//192
	text_editing_event_get_text_Handler,		//193
	text_editing_event_set_text_Handler,		//194
	text_editing_event_get_start_Handler,		//195
	text_editing_event_set_start_Handler,		//196
	text_editing_event_get_length_Handler,		//197
	text_editing_event_set_length_Handler,		//198
	pointer_deref_text_input_event_Handler,		//199
	pointer_deref_text_input_event_assign_Handler,		//200
	new_text_input_event_Handler,		//201
	delete_text_input_event_Handler,		//202
	text_input_event_get_type_Handler,		//203
	text_input_event_set_type_Handler,		//204
	text_input_event_get_timestamp_Handler,		//205
	text_input_event_set_timestamp_Handler,		//206
	text_input_event_get_windowID_Handler,		//207
	text_input_event_set_windowID_Handler,		//208
	text_input_event_get_text_Handler,		//209
	text_input_event_set_text_Handler,		//210
	pointer_deref_mouse_motion_event_Handler,		//211
	pointer_deref_mouse_motion_event_assign_Handler,		//212
	new_mouse_motion_event_Handler,		//213
	delete_mouse_motion_event_Handler,		//214
	mouse_motion_event_get_type_Handler,		//215
	mouse_motion_event_set_type_Handler,		//216
	mouse_motion_event_get_timestamp_Handler,		//217
	mouse_motion_event_set_timestamp_Handler,		//218
	mouse_motion_event_get_windowID_Handler,		//219
	mouse_motion_event_set_windowID_Handler,		//220
	mouse_motion_event_get_which_Handler,		//221
	mouse_motion_event_set_which_Handler,		//222
	mouse_motion_event_get_state_Handler,		//223
	mouse_motion_event_set_state_Handler,		//224
	mouse_motion_event_get_x_Handler,		//225
	mouse_motion_event_set_x_Handler,		//226
	mouse_motion_event_get_y_Handler,		//227
	mouse_motion_event_set_y_Handler,		//228
	mouse_motion_event_get_xrel_Handler,		//229
	mouse_motion_event_set_xrel_Handler,		//230
	mouse_motion_event_get_yrel_Handler,		//231
	mouse_motion_event_set_yrel_Handler,		//232
	pointer_deref_mouse_button_event_Handler,		//233
	pointer_deref_mouse_button_event_assign_Handler,		//234
	new_mouse_button_event_Handler,		//235
	delete_mouse_button_event_Handler,		//236
	mouse_button_event_get_type_Handler,		//237
	mouse_button_event_set_type_Handler,		//238
	mouse_button_event_get_timestamp_Handler,		//239
	mouse_button_event_set_timestamp_Handler,		//240
	mouse_button_event_get_windowID_Handler,		//241
	mouse_button_event_set_windowID_Handler,		//242
	mouse_button_event_get_which_Handler,		//243
	mouse_button_event_set_which_Handler,		//244
	mouse_button_event_get_button_Handler,		//245
	mouse_button_event_set_button_Handler,		//246
	mouse_button_event_get_state_Handler,		//247
	mouse_button_event_set_state_Handler,		//248
	mouse_button_event_get_clicks_Handler,		//249
	mouse_button_event_set_clicks_Handler,		//250
	mouse_button_event_get_x_Handler,		//251
	mouse_button_event_set_x_Handler,		//252
	mouse_button_event_get_y_Handler,		//253
	mouse_button_event_set_y_Handler,		//254
	pointer_deref_mouse_wheel_event_Handler,		//255
	pointer_deref_mouse_wheel_event_assign_Handler,		//256
	new_mouse_wheel_event_Handler,		//257
	delete_mouse_wheel_event_Handler,		//258
	mouse_wheel_event_get_type_Handler,		//259
	mouse_wheel_event_set_type_Handler,		//260
	mouse_wheel_event_get_timestamp_Handler,		//261
	mouse_wheel_event_set_timestamp_Handler,		//262
	mouse_wheel_event_get_windowID_Handler,		//263
	mouse_wheel_event_set_windowID_Handler,		//264
	mouse_wheel_event_get_which_Handler,		//265
	mouse_wheel_event_set_which_Handler,		//266
	mouse_wheel_event_get_x_Handler,		//267
	mouse_wheel_event_set_x_Handler,		//268
	mouse_wheel_event_get_y_Handler,		//269
	mouse_wheel_event_set_y_Handler,		//270
	mouse_wheel_event_get_direction_Handler,		//271
	mouse_wheel_event_set_direction_Handler,		//272
	pointer_deref_joy_axis_event_Handler,		//273
	pointer_deref_joy_axis_event_assign_Handler,		//274
	new_joy_axis_event_Handler,		//275
	delete_joy_axis_event_Handler,		//276
	joy_axis_event_get_type_Handler,		//277
	joy_axis_event_set_type_Handler,		//278
	joy_axis_event_get_timestamp_Handler,		//279
	joy_axis_event_set_timestamp_Handler,		//280
	joy_axis_event_get_which_Handler,		//281
	joy_axis_event_set_which_Handler,		//282
	joy_axis_event_get_axis_Handler,		//283
	joy_axis_event_set_axis_Handler,		//284
	joy_axis_event_get_padding1_Handler,		//285
	joy_axis_event_set_padding1_Handler,		//286
	joy_axis_event_get_padding2_Handler,		//287
	joy_axis_event_set_padding2_Handler,		//288
	joy_axis_event_get_padding3_Handler,		//289
	joy_axis_event_set_padding3_Handler,		//290
	joy_axis_event_get_value_Handler,		//291
	joy_axis_event_set_value_Handler,		//292
	pointer_deref_joy_ball_event_Handler,		//293
	pointer_deref_joy_ball_event_assign_Handler,		//294
	new_joy_ball_event_Handler,		//295
	delete_joy_ball_event_Handler,		//296
	joy_ball_event_get_type_Handler,		//297
	joy_ball_event_set_type_Handler,		//298
	joy_ball_event_get_timestamp_Handler,		//299
	joy_ball_event_set_timestamp_Handler,		//300
	joy_ball_event_get_which_Handler,		//301
	joy_ball_event_set_which_Handler,		//302
	joy_ball_event_get_ball_Handler,		//303
	joy_ball_event_set_ball_Handler,		//304
	joy_ball_event_get_padding1_Handler,		//305
	joy_ball_event_set_padding1_Handler,		//306
	joy_ball_event_get_padding2_Handler,		//307
	joy_ball_event_set_padding2_Handler,		//308
	joy_ball_event_get_padding3_Handler,		//309
	joy_ball_event_set_padding3_Handler,		//310
	joy_ball_event_get_xrel_Handler,		//311
	joy_ball_event_set_xrel_Handler,		//312
	joy_ball_event_get_yrel_Handler,		//313
	joy_ball_event_set_yrel_Handler,		//314
	pointer_deref_joy_hat_event_Handler,		//315
	pointer_deref_joy_hat_event_assign_Handler,		//316
	new_joy_hat_event_Handler,		//317
	delete_joy_hat_event_Handler,		//318
	joy_hat_event_get_type_Handler,		//319
	joy_hat_event_set_type_Handler,		//320
	joy_hat_event_get_timestamp_Handler,		//321
	joy_hat_event_set_timestamp_Handler,		//322
	joy_hat_event_get_which_Handler,		//323
	joy_hat_event_set_which_Handler,		//324
	joy_hat_event_get_hat_Handler,		//325
	joy_hat_event_set_hat_Handler,		//326
	joy_hat_event_get_value_Handler,		//327
	joy_hat_event_set_value_Handler,		//328
	joy_hat_event_get_padding1_Handler,		//329
	joy_hat_event_set_padding1_Handler,		//330
	joy_hat_event_get_padding2_Handler,		//331
	joy_hat_event_set_padding2_Handler,		//332
	pointer_deref_joy_button_event_Handler,		//333
	pointer_deref_joy_button_event_assign_Handler,		//334
	new_joy_button_event_Handler,		//335
	delete_joy_button_event_Handler,		//336
	joy_button_event_get_type_Handler,		//337
	joy_button_event_set_type_Handler,		//338
	joy_button_event_get_timestamp_Handler,		//339
	joy_button_event_set_timestamp_Handler,		//340
	joy_button_event_get_which_Handler,		//341
	joy_button_event_set_which_Handler,		//342
	joy_button_event_get_button_Handler,		//343
	joy_button_event_set_button_Handler,		//344
	joy_button_event_get_state_Handler,		//345
	joy_button_event_set_state_Handler,		//346
	joy_button_event_get_padding1_Handler,		//347
	joy_button_event_set_padding1_Handler,		//348
	joy_button_event_get_padding2_Handler,		//349
	joy_button_event_set_padding2_Handler,		//350
	pointer_deref_joy_device_event_Handler,		//351
	pointer_deref_joy_device_event_assign_Handler,		//352
	new_joy_device_event_Handler,		//353
	delete_joy_device_event_Handler,		//354
	joy_device_event_get_type_Handler,		//355
	joy_device_event_set_type_Handler,		//356
	joy_device_event_get_timestamp_Handler,		//357
	joy_device_event_set_timestamp_Handler,		//358
	joy_device_event_get_which_Handler,		//359
	joy_device_event_set_which_Handler,		//360
	pointer_deref_controller_axis_event_Handler,		//361
	pointer_deref_controller_axis_event_assign_Handler,		//362
	new_controller_axis_event_Handler,		//363
	delete_controller_axis_event_Handler,		//364
	controller_axis_event_get_type_Handler,		//365
	controller_axis_event_set_type_Handler,		//366
	controller_axis_event_get_timestamp_Handler,		//367
	controller_axis_event_set_timestamp_Handler,		//368
	controller_axis_event_get_which_Handler,		//369
	controller_axis_event_set_which_Handler,		//370
	controller_axis_event_get_axis_Handler,		//371
	controller_axis_event_set_axis_Handler,		//372
	controller_axis_event_get_padding1_Handler,		//373
	controller_axis_event_set_padding1_Handler,		//374
	controller_axis_event_get_padding2_Handler,		//375
	controller_axis_event_set_padding2_Handler,		//376
	controller_axis_event_get_padding3_Handler,		//377
	controller_axis_event_set_padding3_Handler,		//378
	controller_axis_event_get_value_Handler,		//379
	controller_axis_event_set_value_Handler,		//380
	controller_axis_event_get_padding4_Handler,		//381
	controller_axis_event_set_padding4_Handler,		//382
	pointer_deref_controller_button_event_Handler,		//383
	pointer_deref_controller_button_event_assign_Handler,		//384
	new_controller_button_event_Handler,		//385
	delete_controller_button_event_Handler,		//386
	controller_button_event_get_type_Handler,		//387
	controller_button_event_set_type_Handler,		//388
	controller_button_event_get_timestamp_Handler,		//389
	controller_button_event_set_timestamp_Handler,		//390
	controller_button_event_get_which_Handler,		//391
	controller_button_event_set_which_Handler,		//392
	controller_button_event_get_button_Handler,		//393
	controller_button_event_set_button_Handler,		//394
	controller_button_event_get_state_Handler,		//395
	controller_button_event_set_state_Handler,		//396
	controller_button_event_get_padding1_Handler,		//397
	controller_button_event_set_padding1_Handler,		//398
	controller_button_event_get_padding2_Handler,		//399
	controller_button_event_set_padding2_Handler,		//400
	pointer_deref_controller_device_event_Handler,		//401
	pointer_deref_controller_device_event_assign_Handler,		//402
	new_controller_device_event_Handler,		//403
	delete_controller_device_event_Handler,		//404
	controller_device_event_get_type_Handler,		//405
	controller_device_event_set_type_Handler,		//406
	controller_device_event_get_timestamp_Handler,		//407
	controller_device_event_set_timestamp_Handler,		//408
	controller_device_event_get_which_Handler,		//409
	controller_device_event_set_which_Handler,		//410
	pointer_deref_audio_device_event_Handler,		//411
	pointer_deref_audio_device_event_assign_Handler,		//412
	new_audio_device_event_Handler,		//413
	delete_audio_device_event_Handler,		//414
	audio_device_event_get_type_Handler,		//415
	audio_device_event_set_type_Handler,		//416
	audio_device_event_get_timestamp_Handler,		//417
	audio_device_event_set_timestamp_Handler,		//418
	audio_device_event_get_which_Handler,		//419
	audio_device_event_set_which_Handler,		//420
	audio_device_event_get_iscapture_Handler,		//421
	audio_device_event_set_iscapture_Handler,		//422
	audio_device_event_get_padding1_Handler,		//423
	audio_device_event_set_padding1_Handler,		//424
	audio_device_event_get_padding2_Handler,		//425
	audio_device_event_set_padding2_Handler,		//426
	audio_device_event_get_padding3_Handler,		//427
	audio_device_event_set_padding3_Handler,		//428
	pointer_deref_quit_event_Handler,		//429
	pointer_deref_quit_event_assign_Handler,		//430
	new_quit_event_Handler,		//431
	delete_quit_event_Handler,		//432
	quit_event_get_type_Handler,		//433
	quit_event_set_type_Handler,		//434
	quit_event_get_timestamp_Handler,		//435
	quit_event_set_timestamp_Handler,		//436
	pointer_deref_user_event_Handler,		//437
	pointer_deref_user_event_assign_Handler,		//438
	new_user_event_Handler,		//439
	delete_user_event_Handler,		//440
	user_event_get_type_Handler,		//441
	user_event_set_type_Handler,		//442
	user_event_get_timestamp_Handler,		//443
	user_event_set_timestamp_Handler,		//444
	user_event_get_windowID_Handler,		//445
	user_event_set_windowID_Handler,		//446
	user_event_get_code_Handler,		//447
	user_event_set_code_Handler,		//448
	user_event_get_data1_Handler,		//449
	user_event_set_data1_Handler,		//450
	user_event_get_data2_Handler,		//451
	user_event_set_data2_Handler,		//452
	pointer_deref_syswm_event_Handler,		//453
	pointer_deref_syswm_event_assign_Handler,		//454
	new_syswm_event_Handler,		//455
	delete_syswm_event_Handler,		//456
	syswm_event_get_type_Handler,		//457
	syswm_event_set_type_Handler,		//458
	syswm_event_get_timestamp_Handler,		//459
	syswm_event_set_timestamp_Handler,		//460
	syswm_event_get_msg_Handler,		//461
	syswm_event_set_msg_Handler,		//462
	pointer_deref_touch_finger_event_Handler,		//463
	pointer_deref_touch_finger_event_assign_Handler,		//464
	new_touch_finger_event_Handler,		//465
	delete_touch_finger_event_Handler,		//466
	touch_finger_event_get_type_Handler,		//467
	touch_finger_event_set_type_Handler,		//468
	touch_finger_event_get_timestamp_Handler,		//469
	touch_finger_event_set_timestamp_Handler,		//470
	touch_finger_event_get_touchId_Handler,		//471
	touch_finger_event_set_touchId_Handler,		//472
	touch_finger_event_get_fingerId_Handler,		//473
	touch_finger_event_set_fingerId_Handler,		//474
	touch_finger_event_get_x_Handler,		//475
	touch_finger_event_set_x_Handler,		//476
	touch_finger_event_get_y_Handler,		//477
	touch_finger_event_set_y_Handler,		//478
	touch_finger_event_get_dx_Handler,		//479
	touch_finger_event_set_dx_Handler,		//480
	touch_finger_event_get_dy_Handler,		//481
	touch_finger_event_set_dy_Handler,		//482
	touch_finger_event_get_pressure_Handler,		//483
	touch_finger_event_set_pressure_Handler,		//484
	pointer_deref_multi_gesture_event_Handler,		//485
	pointer_deref_multi_gesture_event_assign_Handler,		//486
	new_multi_gesture_event_Handler,		//487
	delete_multi_gesture_event_Handler,		//488
	multi_gesture_event_get_type_Handler,		//489
	multi_gesture_event_set_type_Handler,		//490
	multi_gesture_event_get_timestamp_Handler,		//491
	multi_gesture_event_set_timestamp_Handler,		//492
	multi_gesture_event_get_touchId_Handler,		//493
	multi_gesture_event_set_touchId_Handler,		//494
	multi_gesture_event_get_dTheta_Handler,		//495
	multi_gesture_event_set_dTheta_Handler,		//496
	multi_gesture_event_get_dDist_Handler,		//497
	multi_gesture_event_set_dDist_Handler,		//498
	multi_gesture_event_get_x_Handler,		//499
	multi_gesture_event_set_x_Handler,		//500
	multi_gesture_event_get_y_Handler,		//501
	multi_gesture_event_set_y_Handler,		//502
	multi_gesture_event_get_numFingers_Handler,		//503
	multi_gesture_event_set_numFingers_Handler,		//504
	multi_gesture_event_get_padding_Handler,		//505
	multi_gesture_event_set_padding_Handler,		//506
	pointer_deref_dollar_gesture_event_Handler,		//507
	pointer_deref_dollar_gesture_event_assign_Handler,		//508
	new_dollar_gesture_event_Handler,		//509
	delete_dollar_gesture_event_Handler,		//510
	dollar_gesture_event_get_type_Handler,		//511
	dollar_gesture_event_set_type_Handler,		//512
	dollar_gesture_event_get_timestamp_Handler,		//513
	dollar_gesture_event_set_timestamp_Handler,		//514
	dollar_gesture_event_get_touchId_Handler,		//515
	dollar_gesture_event_set_touchId_Handler,		//516
	dollar_gesture_event_get_gestureId_Handler,		//517
	dollar_gesture_event_set_gestureId_Handler,		//518
	dollar_gesture_event_get_numFingers_Handler,		//519
	dollar_gesture_event_set_numFingers_Handler,		//520
	dollar_gesture_event_get_error_Handler,		//521
	dollar_gesture_event_set_error_Handler,		//522
	dollar_gesture_event_get_x_Handler,		//523
	dollar_gesture_event_set_x_Handler,		//524
	dollar_gesture_event_get_y_Handler,		//525
	dollar_gesture_event_set_y_Handler,		//526
	pointer_deref_drop_event_Handler,		//527
	pointer_deref_drop_event_assign_Handler,		//528
	new_drop_event_Handler,		//529
	delete_drop_event_Handler,		//530
	drop_event_get_type_Handler,		//531
	drop_event_set_type_Handler,		//532
	drop_event_get_timestamp_Handler,		//533
	drop_event_set_timestamp_Handler,		//534
	drop_event_get_file_Handler,		//535
	drop_event_set_file_Handler,		//536
	drop_event_get_windowID_Handler,		//537
	drop_event_set_windowID_Handler,		//538
	event_get_type_Handler,		//539
	event_set_type_Handler,		//540
	event_get_common_Handler,		//541
	event_set_common_Handler,		//542
	event_get_window_Handler,		//543
	event_set_window_Handler,		//544
	event_get_key_Handler,		//545
	event_set_key_Handler,		//546
	event_get_edit_Handler,		//547
	event_set_edit_Handler,		//548
	event_get_text_Handler,		//549
	event_set_text_Handler,		//550
	event_get_motion_Handler,		//551
	event_set_motion_Handler,		//552
	event_get_button_Handler,		//553
	event_set_button_Handler,		//554
	event_get_wheel_Handler,		//555
	event_set_wheel_Handler,		//556
	event_get_jaxis_Handler,		//557
	event_set_jaxis_Handler,		//558
	event_get_jball_Handler,		//559
	event_set_jball_Handler,		//560
	event_get_jhat_Handler,		//561
	event_set_jhat_Handler,		//562
	event_get_jbutton_Handler,		//563
	event_set_jbutton_Handler,		//564
	event_get_jdevice_Handler,		//565
	event_set_jdevice_Handler,		//566
	event_get_caxis_Handler,		//567
	event_set_caxis_Handler,		//568
	event_get_cbutton_Handler,		//569
	event_set_cbutton_Handler,		//570
	event_get_cdevice_Handler,		//571
	event_set_cdevice_Handler,		//572
	event_get_adevice_Handler,		//573
	event_set_adevice_Handler,		//574
	event_get_quit_Handler,		//575
	event_set_quit_Handler,		//576
	event_get_user_Handler,		//577
	event_set_user_Handler,		//578
	event_get_syswm_Handler,		//579
	event_set_syswm_Handler,		//580
	event_get_tfinger_Handler,		//581
	event_set_tfinger_Handler,		//582
	event_get_mgesture_Handler,		//583
	event_set_mgesture_Handler,		//584
	event_get_dgesture_Handler,		//585
	event_set_dgesture_Handler,		//586
	event_get_drop_Handler,		//587
	event_set_drop_Handler,		//588
	SDL_Init_Handler,		//589
	SDL_Quit_Handler,		//590
	SDL_CreateWindow_Handler,		//591
	SDL_GetWindowSurface_Handler,		//592
	SDL_LoadBMP_Handler,		//593
	SDL_FreeSurface_Handler,		//594
	SDL_BlitSurface_Handler,		//595
	SDL_BlitScaled_Handler,		//596
	SDL_UpdateWindowSurface_Handler,		//597
	SDL_DestroyWindow_Handler,		//598
	SDL_GetWindowSize_Handler,		//599
	SDL_GetError_Handler,		//600
	SDL_PollEvent_Handler		//601
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