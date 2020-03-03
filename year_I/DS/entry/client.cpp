#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <cstdlib>
#include <iostream>
#include <vector>
#include <unistd.h>

#include "common.h"

static const size_t BUFFER_SIZE = 1034;

int connect_to_server(char *ip_v4_addr, int port, int timeout_conf) {
    int sockfd;
    struct timeval tv = {
            .tv_sec = timeout_conf,
            .tv_usec = 0
    };
    struct sockaddr_in server = {
            .sin_family = AF_INET,
            .sin_port = htons(port)
    };

    server.sin_addr.s_addr = inet_addr(ip_v4_addr);

    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (!sockfd)
        return -1;

    setsockopt(sockfd, SOL_SOCKET, SO_RCVTIMEO, (const char*)&tv, sizeof tv);

    if (connect(sockfd , (struct sockaddr *)&server , sizeof(server)) < 0) {
        close(sockfd);
        return -1;
    }

    return sockfd;
}

int process_expression(int sockfd) {
    std::streamsize s = 0;
    std::vector<char> buffer (BUFFER_SIZE,0);

    while (std::cin >> std::noskipws >> buffer[s++]) {
        if (buffer[s - 1] == '\n')
            break;

        if (s == BUFFER_SIZE - 10) {
            if (!send_data(sockfd, buffer.data(), s))
                return 1;
            s = 0;
        }
    }

    buffer[s - 1] = '\n';
    if (!send_data(sockfd, buffer.data(), s))
        return 1;

    while ((s = read(sockfd, buffer.data() , BUFFER_SIZE)) > 0)
        std::cout.write(buffer.data(), s);

    if (s < 0) {
        std::cout << "TIMEOUT" << std::endl;
        return 1;
    } else {
        std::cout << std::endl;
        return 0;
    }
}

int main(int argc, char *argv[]) {
    int sockfd, res;

    if (argc != 4)
        return 1;

    sockfd = connect_to_server(argv[1], atoi(argv[2]), atoi(argv[3]));
    if (sockfd < 0)
        return 1;

    res = process_expression(sockfd);

    close(sockfd);

    return res;
}