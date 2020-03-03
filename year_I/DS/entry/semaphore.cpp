#include <cassert>
#include <mutex>
#include <condition_variable>

#include "semaphore.h"

semaphore::semaphore(int count) noexcept
        : m_count(count) { assert(count > -1); }

void semaphore::post() noexcept
{
    {
        std::unique_lock<std::mutex> lock(m_mutex);
        ++m_count;
    }
    m_cv.notify_one();
}

void semaphore::wait() noexcept
{
    std::unique_lock<std::mutex> lock(m_mutex);
    m_cv.wait(lock, [&]() { return m_count != 0; });
    --m_count;
}
