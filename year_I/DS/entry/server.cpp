#include<sys/socket.h>
#include<arpa/inet.h>
#include<unistd.h>

#include <stdexcept>
#include <functional>

#include "common.h"
#include "threadpool.h"
#include "bigint.h"

typedef bigint BIG_NUM;

static const size_t BUFFER_SIZE = 4096;


enum class Operator {
    plus,
    minus
};

enum class ExpressionState {
    number,
    operand,
    any,
    invalid,
    done
};

void perform_op(BIG_NUM &state, unsigned long long number, Operator op) {
    switch (op) {
        case Operator::minus:
            state -= number;
            break;
        case Operator::plus:
            state += number;
            break;
        default:
            throw std::invalid_argument("Unknown operator: programming error");
    }
}

void process_connection(int sock_fd) {
    unsigned long long current_num = 0;
    BIG_NUM result(0);
    Operator op = Operator::plus;
    ExpressionState state = ExpressionState::number;
    int read_size;
    char client_message[BUFFER_SIZE];

    while( (read_size = read(sock_fd , client_message , BUFFER_SIZE)) > 0 )
    {
        for (int i = 0; i < read_size; i++) {
            if (client_message[i] == ' ' || client_message[i] == '\t') {
                if (state == ExpressionState::any)
                    state = ExpressionState::operand;
                continue;
            }

            if (isdigit(client_message[i]) && (state == ExpressionState::number || state == ExpressionState::any)) {
                if (current_num == 0 && state == ExpressionState::any) {
                    // a digit was read already and it was a leading zero - error
                    state = ExpressionState::invalid;
                    break;
                }
                current_num *= 10;
                current_num += client_message[i] - '0';
                state = ExpressionState::any;
                continue;
            }

            if (client_message[i] == '+' && (state == ExpressionState::any || state == ExpressionState::operand)) {
                perform_op(result, current_num, op);
                op = Operator::plus;
                state = ExpressionState::number;
                current_num = 0;
                continue;
            }

            if (client_message[i] == '-' && (state == ExpressionState::any || state == ExpressionState::operand)) {
                perform_op(result, current_num, op);
                state = ExpressionState::number;
                op = Operator::minus;
                current_num = 0;
                continue;
            }

            if (client_message[i] == '\n' && state != ExpressionState::number) {
                perform_op(result, current_num, op);
                state = ExpressionState::done;
                break;
            }

            state = ExpressionState::invalid;
            break;
        }

        if (state == ExpressionState::invalid || state == ExpressionState::done)
            break;
    }

    std::string response = (read_size <= 0 || state == ExpressionState::invalid) ? "ERROR" : result.toString();

    send_data(sock_fd, response.data(), response.length());
    close(sock_fd);
}


int main(int argc, char *argv[]) {
    if (argc != 3)
        return 1;

    int socket_desc , client_sock , c, enable = 1, threads = atoi(argv[2]);
    struct sockaddr_in server , client;

    if (threads <= 0)
        return 1;
    ThreadPool pool(threads);

    socket_desc = socket(AF_INET , SOCK_STREAM , 0);
    if (socket_desc == -1)
        return 1;

    if (setsockopt(socket_desc, SOL_SOCKET, SO_REUSEADDR, &enable, sizeof(int)) < 0)
        return 1;

    server.sin_family = AF_INET;
    server.sin_addr.s_addr = INADDR_ANY;
    server.sin_port = htons( atoi(argv[1]) );

    if( bind(socket_desc,(struct sockaddr *)&server , sizeof(server)) < 0)
        return 1;

    listen(socket_desc, threads);
    c = sizeof(struct sockaddr_in);

    while (true) {
        client_sock = accept(socket_desc, (struct sockaddr *) &client, (socklen_t *) &c);
        if (client_sock < 0)
            return 1;

        pool.enqueue(std::bind(process_connection, client_sock));
    }
}