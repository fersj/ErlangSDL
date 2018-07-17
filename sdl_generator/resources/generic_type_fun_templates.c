byte * read_{{ErlName}}(byte *in, {{CName}} *result) {
	return read_{{Type}}(in, result);
}

byte * write_{{ErlName}}({{CName}} *value, byte *out, size_t *len) {
	return write_{{Type}}(value, out, len);
}

byte * read_{{ErlName}}_array(byte *in, {{CName}} *array, int n) {
	return read_{{Type}}_array(in, array, n);
}

byte * write_{{ErlName}}_array({{CName}} *array, byte *out, size_t *len, int n) {
	return write_{{Type}}_array(array, out, len, n);
}

