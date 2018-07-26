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

void wait_for_input(byte *input_buffer, byte *output_buffer) {
	int len_in = read_command(input_buffer);
	size_t len_out;
	byte *current_in = input_buffer;

	while (len_in > 0 && input_buffer[0] == CALL_CODE) {
		current_in++;

		int opcode;
		read_int(current_in, &opcode);

		handler current_handler = handlers[opcode];
		(*current_handler)(current_in, len_in, output_buffer, &len_out);

		write_command(output_buffer, len_out);

		len_in = read_command(input_buffer);
		current_in = input_buffer;
	}
	
	if (len_in == 0) {
		printf("Closed.");
		exit(EXIT_SUCCESS);
	}
}

int main() {
// #ifdef DEBUG  
// 	log_file = fopen("log.txt", "a");
// 	log_port("Started.\n");
// #endif  

	byte input_buffer[BUF_SIZE], output_buffer[BUF_SIZE];
	wait_for_input(input_buffer, output_buffer);
	
// #ifdef DEBUG  
// 	log_port("Finished.\n");
// 	fclose(log_file);
// #endif  
}

