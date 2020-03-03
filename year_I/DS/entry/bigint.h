#ifndef ENTRY_BIGINT_H
#define ENTRY_BIGINT_H

/* Simplified portion of InfInt header only library. A few methods coded by myself */

#include <string>
#include <vector>

typedef long long DIGIT_TYPE;

static constexpr DIGIT_TYPE BIG_BASE = 10000;
static constexpr DIGIT_TYPE BIG_UPPER_BOUND = BIG_BASE - 1;
static constexpr DIGIT_TYPE BIG_DIGITS = 4;

class bigint {
public:
    bigint(unsigned long long);

    const bigint& operator +=(const bigint& rhs);
    const bigint& operator -=(const bigint& rhs);

    std::string toString();
private:
    std::vector<DIGIT_TYPE> val;
    bool pos;

    void correct();
    void removeLeadingZeros();
    void truncateToBase();
    bool equalizeSigns();
};

#endif //ENTRY_BIGINT_H
