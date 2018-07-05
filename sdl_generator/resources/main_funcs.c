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