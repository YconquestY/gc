#ifndef UTIL_H
#define UTIL_H

#include "vmtypes.h"

inline static value_t min(value_t a, value_t b) {
    return a < b ? a : b;
}

inline static value_t max(value_t a, value_t b) {
    return a > b ? a : b;
}

#endif