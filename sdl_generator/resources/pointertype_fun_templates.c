byte * read_{{ErlName}}(byte *in, {{CName}} *result) {
	return read_pointer(in, result);
}

byte * write_{{ErlName}}({{CName}} *value, byte *out, size_t *len) {
	return write_pointer(value, out, len);
}

byte * read_{{ErlName}}_array(byte *in, {{CName}} *array, int n) {
	return read_pointer_array(in, array, n);
}

byte * write_{{ErlName}}_array({{CName}} *array, byte *out, size_t *len, int n) {
	return write_pointer_array(array, out, len, n);
}

