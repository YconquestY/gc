#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>

#include "memory.h"
#include "fail.h"
#include "block_header.h"
/*
struct memory {
  value_t* start;
  value_t* end;
  value_t* free;
};
*/
char* memory_get_identity() {
  return "no GC (memory is never freed)";
}

memory* memory_new(size_t total_byte_size) {
  value_t* memory_start = (value_t*) calloc(1, total_byte_size);
  if (!memory_start)
    fail("cannot allocate %zd bytes of memory", total_byte_size);
  value_t* memory_end = memory_start + (total_byte_size / sizeof(value_t));

  memory* self = calloc(1, sizeof(memory));
  if (!self)
  fail("cannot allocate memory");
  self->start = memory_start;
  self->end = memory_end;
  return self;
}

void memory_free(memory* self) {
  free(self->start);
  free(self);
}

value_t* memory_get_start(memory* self) {
  return self->start;
}

value_t* memory_get_end(memory* self) {
  return self->end;
}

void memory_set_bitmap(memory* self, value_t* bitmap) {
  self->bitmap = bitmap;
}

void memory_set_heap_start(memory* self, value_t* heap_start) {
  self->free = heap_start;
}

value_t* memory_get_block(memory* self,
                          uint32_t idx,
                          tag_t tag,
                          value_t size,
                          value_t* root) {

  value_t* block = self->heads[idx];
  /// Set corresponding bit in bitmap
  uint32_t bitmap_idx = block - self->free;
  self->bitmap[bitmap_idx >> 5] |= 1 << (bitmap_idx & 0x1F);
  block_set_tag_size(block, tag, size);
  /// TODO: update free list, i.e. split
  //self->free += total_size;
  value_t* next_fb = self->heads[idx][HEADER_SIZE] ;

  return block;

} 

value_t* memory_allocate(memory* self,
                         tag_t tag,
                         value_t size,
                         value_t* root) {
  size = max(size, 1);
  const value_t total_size = size + HEADER_SIZE;
  uint32_t idx = min(size - 1, NUM_HEAD - 1);
  for (uint32_t _idx = idx; _idx < NUM_HEAD; _idx++) {
    if (self->heads[_idx]) {
      return memory_get_block(self, _idx, tag, size, root);
    }
  }

  /// TODO: garbage collection

  for (uint32_t _idx = idx; _idx < idx; _idx++) {
    if (self->heads[_idx]) {
      return memory_get_block(self, _idx, tag, size, root);
    }
  }
  fail("no memory left (block of size %u requested)", size);
}

value_t* memory_copy_of_block(memory* self, value_t* block, value_t* root) {
  tag_t tag = block_tag(block);
  value_t size = block_size(block);
  value_t* copy = memory_allocate(self, tag, size, root);
  memcpy(copy, block, size * sizeof(value_t));
  return copy;
}

void memory_free_block(memory* self, value_t* block) {
  // do nothing
}
