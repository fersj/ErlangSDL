byte * read_{{ErlName}}_array(byte *in, {{CName}} *array, int n) {
	byte *current_in = in;

	for (int i=0; i<n; i++)
		current_in = read_{{ErlName}}(current_in, &array[i]);

	return current_in;
}

byte * write_{{ErlName}}_array({{CName}} *array, byte *out, size_t *len, int n) {
	byte *current_out = out;

	for (int i=0; i<n; i++)
		current_out = write_{{ErlName}}(&array[i], current_out, len);

	return current_out;
}

void pointer_deref_{{ErlName}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	{{CName}} *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_out = write_{{ErlName}}(pointer, current_out, len_out);
}

void pointer_deref_{{ErlName}}_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	{{CName}} *ptr;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_out = write_{{ErlName}}(&(ptr[index]), current_out, len_out);
}

void pointer_deref_{{ErlName}}_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	{{CName}} *pointer;
	current_in = read_pointer(current_in, (void **) &pointer);
	current_in = read_{{ErlName}}(current_in, pointer);
}

void pointer_deref_{{ErlName}}_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	{{CName}} *ptr, value;
	int index;
	current_in = read_pointer(current_in, (void **) &ptr);
	current_in = read_int(current_in, &index);
	current_in = read_{{ErlName}}(current_in, &value);
	ptr[index] = value;
}

void new_{{ErlName}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	{{CName}} *ptr = malloc(sizeof({{CName}}));
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_{{ErlName}}_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	{{CName}} *ptr = malloc(sizeof({{CName}})*size);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_{{ErlName}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	{{CName}} *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
}

