#include <stdexcept>

#include "bigint.h"

inline static div_t pos_div(int num, int denom)
{
    div_t result;
    result.quot = num / denom;
    result.rem = num - denom * result.quot;
    return result;
}

bigint::bigint(unsigned long long l)
    : pos(true) {
    do {
        val.push_back(l % BIG_BASE);
        l = l / BIG_BASE;
    } while (l > 0);
}

const bigint& bigint::operator+=(const bigint& rhs)
{
    if (rhs.val.size() > val.size())
        val.resize(rhs.val.size(), 0);

    for (size_t i = 0; i < val.size(); ++i)
        val[i] = (pos ? val[i] : -val[i]) + (i < rhs.val.size() ? (rhs.pos ? rhs.val[i] : -rhs.val[i]) : 0);

    correct();
    return *this;
}

const bigint& bigint::operator-=(const bigint& rhs)
{
    if (rhs.val.size() > val.size())
        val.resize(rhs.val.size(), 0);

    for (size_t i = 0; i < val.size(); ++i)
        val[i] = (pos ? val[i] : -val[i]) - (i < rhs.val.size() ? (rhs.pos ? rhs.val[i] : -rhs.val[i]) : 0);

    correct();
    return *this;
}

inline void bigint::truncateToBase()
{
    for (size_t i = 0; i < val.size(); ++i) // truncate each
    {
        if (val[i] >= BIG_BASE || val[i] <= -BIG_BASE)
        {
            div_t dt = pos_div(val[i], BIG_BASE);
            val[i] = dt.rem;
            if (i + 1 >= val.size())
                val.push_back(0);
            val[i + 1] += dt.quot;
        }
    }
}

inline bool bigint::equalizeSigns()
{
    bool isPositive = true;
    int i = (int) ((val.size())) - 1;
    for (; i >= 0; --i)
    {
        if (val[i] != 0)
        {
            isPositive = val[i--] > 0;
            break;
        }
    }

    if (isPositive)
    {
        for (; i >= 0; --i)
        {
            if (val[i] < 0)
            {
                int k = 0, index = i + 1;
                for (; (size_t)(index) < val.size() && val[index] == 0; ++k, ++index)
                    ; // count adjacent zeros on left
                {
                    val[index] -= 1;
                    val[i] += BIG_BASE;
                    for (; k > 0; --k)
                        val[i + k] = BIG_UPPER_BOUND;
                }
            }
        }
    }
    else
    {
        for (; i >= 0; --i)
        {
            if (val[i] > 0)
            {
                int k = 0, index = i + 1;
                for (; (size_t)(index) < val.size() && val[index] == 0; ++k, ++index)
                    ; // count adjacent zeros on right
                {
                    val[index] += 1;
                    val[i] -= BIG_BASE;
                    for (; k > 0; --k)
                        val[i + k] = -BIG_UPPER_BOUND;
                }
            }
        }
    }

    return isPositive;
}

void bigint::removeLeadingZeros()
{
    for (int i = (int) (val.size()) - 1; i > 0; --i) // remove leading 0's
    {
        if (val[i] != 0)
        {
            return;
        }
        else
        {
            val.pop_back();
        }
    }
}

inline void bigint::correct()
{
    truncateToBase();

    pos = equalizeSigns();

    if (!pos) {
        for (long long & i : val)
            i = std::abs(i);
    }

    removeLeadingZeros();
}

std::string bigint::toString() {
    size_t s = val.size();
    std::string result = (pos ? "" : "-") + std::to_string(val[s - 1]);

    for (long long i = (long long) s - 2; i >= 0; i--) {
        std::string digit = std::to_string(val[i]);
        result += (std::string(BIG_DIGITS - digit.length(), '0') + digit);
    }

    return result;
}
