byte * read_{{ErlName}}(byte *in, {{CName}} *result) {
	byte *current_in = in;

	for (int i=0; i<{{Desc}}; i++)
		current_in = read_{{Type}}(current_in, &(result[i]));

	return current_in;
}

byte * write_{{ErlName}}({{CName}} *array, byte *out, size_t *len) {
	byte *current_out = out;

	for (int i=0; i<{{Desc}}; i++)
		current_out = write_{{Type}}(&(array[i]), current_out, len);

	return current_out;
}

