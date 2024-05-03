#ifndef ADDRESS_H
#define ADDRESS_H

#include "vmtypes.h"

// Address conversion (virtual <-> physical)

// Convert a "virtual" (L3VM) address to a "physical" (host computer) one.
inline static value_t* addr_v_to_p(value_t* memory_start, value_t v_addr) {
  return &memory_start[v_addr >> 2];
}

// Convert a "physical" (host computer) address to a "virtuel" (L3VM) one.
inline static value_t addr_p_to_v(value_t* memory_start, value_t* p_addr) {
  return (value_t)(p_addr - memory_start) << 2;
}

#endif
