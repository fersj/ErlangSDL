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
	current_out = write_pointer(&ptr, current_out, len_out);
}

void new_int8_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	int8_t *ptr = malloc(sizeof(int8_t)*size);
	current_out = write_pointer(&ptr, current_out, len_out);
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
	current_out = write_pointer(&ptr, current_out, len_out);
}

void new_int16_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	int16_t *ptr = malloc(sizeof(int16_t)*size);
	current_out = write_pointer(&ptr, current_out, len_out);
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
	current_out = write_pointer(&ptr, current_out, len_out);
}

void new_int32_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	int *ptr = malloc(sizeof(int)*size);
	current_out = write_pointer(&ptr, current_out, len_out);
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
	current_out = write_pointer(&ptr, current_out, len_out);
}

void new_int64_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	int64_t *ptr = malloc(sizeof(int64_t)*size);
	current_out = write_pointer(&ptr, current_out, len_out);
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
	current_out = write_pointer(&ptr, current_out, len_out);
}

void new_float_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	float *ptr = malloc(sizeof(float)*size);
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
	current_out = write_pointer(&ptr, current_out, len_out);
}

void new_double_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	double *ptr = malloc(sizeof(double)*size);
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
	strcpy(*ptr, value);
}

void pointer_deref_string_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	string *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_string(current_in, &value);
	strcpy(ptr[index], value);
}

void new_string_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	string *ptr = malloc(sizeof(string));
	current_out = write_pointer(&ptr, current_out, len_out);
}

void new_string_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	string *ptr = malloc(sizeof(string)*size);
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
	current_in = read_pointer(current_in, &ptr);
	current_out = write_pointer(ptr, current_out, len_out);
}

void pointer_deref_pointer_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	void **ptr;
	int index;
	current_in = read_pointer(current_in, &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_pointer(&(ptr[index]), current_out, len_out);
}

void pointer_deref_pointer_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	void **ptr, *value;
	current_in = read_pointer(current_in, &ptr);
	current_in = read_pointer(current_in, &value);
	*ptr = value;
}

void pointer_deref_pointer_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	void **ptr, *value;
	int index;
	current_in = read_pointer(current_in, &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_string(current_in, &value);
	ptr[index] = value;
}

void new_pointer_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	void **ptr = malloc(sizeof(void *));
	current_out = write_pointer(&ptr, current_out, len_out);
}

void new_pointer_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	void **ptr = malloc(sizeof(void *)*size);
	current_out = write_pointer(&ptr, current_out, len_out);
}

void delete_pointer_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	void **ptr;
	current_in = read_pointer(current_in, &ptr);
	free(ptr);
}

