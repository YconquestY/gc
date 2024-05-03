#ifndef VMTYPES_H
#define VMTYPES_H

#include <stdint.h>
#include <assert.h>

// An unsigned L3 value (virtual pointer, tagged int, etc.)
typedef uint32_t value_t;

// A signed L3 value (very rarely used!)
typedef int32_t svalue_t;

static_assert(sizeof(value_t) == sizeof(svalue_t),
              "unsigned and signed values must have the same size");

#define LOG2_VALUE_BYTES 2
#define VALUE_BYTES (1 << LOG2_VALUE_BYTES)
#define VALUE_BITS (VALUE_BYTES * 8)

static_assert(VALUE_BYTES == sizeof(value_t),
              "invalid definition of VALUE_BYTES");

#endif
