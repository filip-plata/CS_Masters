#include <sys/ioctl.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#define HELLO_IOCTL_SET_REPEATS _IO('H', 0x00)

static char buf[] = "4\n";

int main(int argc, char **argv) {
	if (argc != 2) {
		fprintf(stderr, "One argument needed.\n");
		return 1;
	}
	
	int f = open("/dev/hello", O_RDWR);
	if (f < 0) {
		perror("open");
		return 1;
	}
	if (write(f, argv[1], strlen(argv[1])) < 0) {
		perror("write");
		return 1;
	}
	return 0;
}
