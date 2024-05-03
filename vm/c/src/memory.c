#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

#include "memory.h"
#include "fail.h"
#include "block_header.h"

struct memory {
  value_t* start;
  value_t* end;
  value_t* free;
};

char* memory_get_identity() {
  return "no GC (memory is never freed)";
}

memory* memory_new(size_t total_byte_size) {
  value_t* memory_start = calloc(1, total_byte_size);
  if (!memory_start)
    fail("cannot allocate %zd bytes of memory", total_byte_size);
  value_t* memory_end = memory_start + (total_byte_size / sizeof(value_t));

  memory* self = calloc(1, sizeof(memory));
  if (!self) fail("cannot allocate memory");
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

void memory_set_heap_start(memory* self, value_t* heap_start) {
  self->free = heap_start;
}

value_t* memory_allocate(memory* self,
                         tag_t tag,
                         value_t size,
                         value_t* root) {
  const value_t total_size = size + HEADER_SIZE;
  if (self->free + total_size > self->end)
    fail("no memory left (block of size %u requested)", size);

  value_t* block = &self->free[HEADER_SIZE];
  block_set_tag_size(block, tag, size);
  self->free += total_size;

  return block;
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
