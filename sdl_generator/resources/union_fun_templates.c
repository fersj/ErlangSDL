byte * read_{{ErlName}}(byte *in, {{CName}} *result) {
	return read_pointer(in, (void **) &result);;
}

byte * write_{{ErlName}}({{CName}} *value, byte *out, size_t *len) {
	return write_pointer((void **) &value, out, len);;
}

byte * read_{{ErlName}}_array(byte *in, {{CName}} **array, int n) {
	return read_pointer_array(in, (void **) array, n);
}

byte * write_{{ErlName}}_array({{CName}} **array, byte *out, size_t *len, int n) {
	return write_pointer_array((void **) array, out, len, n);
}

void new_{{ErlName}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	{{CName}} *ptr = malloc(sizeof({{CName}}));
	current_out = write_byte(RET_CODE, current_out, len_out);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void new_{{ErlName}}_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	int size;
	current_in = read_int(current_in, &size);
	{{CName}} *ptr = malloc(sizeof({{CName}})*size);
	current_out = write_byte(RET_CODE, current_out, len_out);
	current_out = write_pointer((void **) &ptr, current_out, len_out);
}

void delete_{{ErlName}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	byte *current_in = in, *current_out = out;
	*len_out = 0; current_in+=4;

	{{CName}} *ptr;
	current_in = read_pointer(current_in, (void **) &ptr);
	free(ptr);
	current_out = write_byte(RET_CODE, current_out, len_out);
}

