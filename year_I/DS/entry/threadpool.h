#ifndef ENTRY_THREADPOOL_H
#define ENTRY_THREADPOOL_H

#include <future>
#include <vector>
#include <queue>

#include "semaphore.h"

class ThreadPool {
public:
    explicit ThreadPool(size_t);
    void enqueue(std::function<void()>);
    ~ThreadPool();
private:
    // need to keep track of threads so we can join them
    std::vector< std::thread > workers;
    // the task queue
    std::queue< std::function<void()> > tasks;

    std::mutex queue_mutex;
    std::condition_variable condition;
    bool stop;
    semaphore sem;
};



#endif //ENTRY_THREADPOOL_H
