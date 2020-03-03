#include <unistd.h>

#include "common.h"


bool send_data(int sockfd, const char* data, size_t size) {
    size_t p, sent = 0;
    while ((sent < size) && ((p = write(sockfd , data + sent, size - sent)) >= 0)) {
        sent += p;
    }

    return p >= 0;
}
