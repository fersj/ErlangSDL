byte * read_{{ErlName}}(byte *in, {{CName}} *result) {
	return read_int(in, result);
}

byte * write_{{ErlName}}({{CName}} *value, byte *out, size_t *len) {
	return write_int(value, out, len);
}

byte * read_{{ErlName}}_array(byte *in, {{CName}} *array, int n) {
	return read_int_array(in, array, n);
}

byte * write_{{ErlName}}_array({{CName}} *array, byte *out, size_t *len, int n) {
	return write_int_array(array, out, len, n);
}

