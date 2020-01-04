#ifndef MAKE_FORMATTER_H
#define MAKE_FORMATTER_H

typedef void (*formatter) (int);

formatter make_formatter(const char *format);

#endif // MAKE_FORMATTER_H
