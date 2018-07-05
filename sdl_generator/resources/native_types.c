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