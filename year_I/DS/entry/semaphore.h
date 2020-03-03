#ifndef ENTRY_SEMAPHORE_H
#define ENTRY_SEMAPHORE_H

class semaphore
{
public:
    explicit semaphore(int count) noexcept;
    void post() noexcept;
    void wait() noexcept;
private:
    int m_count;
    std::mutex m_mutex;
    std::condition_variable m_cv;
};

#endif //ENTRY_SEMAPHORE_H
