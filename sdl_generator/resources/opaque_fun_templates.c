byte * read_{{ErlName}}(byte *in, {{CName}} *result) {
	return read_pointer(in, (void **) &result);
}

byte * write_{{ErlName}}({{CName}} *value, byte *out, size_t *len) {
	return write_pointer((void **) &value, out, len);
}

byte * read_{{ErlName}}_array(byte *in, {{CName}} **array, int n) {
	return read_pointer_array(in, (void **) array, n);
}

byte * write_{{ErlName}}_array({{CName}} **array, byte *out, size_t *len, int n) {
	return write_pointer_array((void **) array, out, len, n);
}

void pointer_deref_{{ErlName}}_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	pointer_deref_pointer_array_Handler(in, len_in, out, len_out);
}

void pointer_deref_{{ErlName}}_array_assign_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	pointer_deref_pointer_array_assign_Handler(in, len_in, out, len_out);
}

void new_{{ErlName}}_array_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	new_pointer_array_Handler(in, len_in, out, len_out);
}

void delete_{{ErlName}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {
	delete_pointer_Handler(in, len_in, out, len_out);
}

