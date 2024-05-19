#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>

#include "address.h"
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
  /// TODO:
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
                          tag_t tag,
                          value_t req_size,
                          value_t* root) {
  uint8_t extra = req_size ? 0 : 1;
  uint32_t capacity = req_size + extra;

  uint32_t idx = min(capacity - 1, NUM_HEAD - 1);
  for ( ; idx < NUM_HEAD; idx++) {
    value_t v_previous = UINT32_MAX,
            v_current = self->heads[idx];
    while (v_current != UINT32_MAX) {
      value_t* p_previous = addr_v_to_p(self->start, v_previous),
             * p_current = addr_v_to_p(self->start, v_current);
      value_t _block_size = block_size(p_current);
      if ((_block_size == capacity) || (_block_size == capacity + 1)) {
        if (_block_size == capacity + 1) {
          extra += 1;
          capacity = req_size + extra;
        }
        uint32_t bitmap_idx = p_current - self->free;
        self->bitmap[bitmap_idx >> 5] |= 1 << (bitmap_idx & 0x1F);
        if (v_previous == UINT32_MAX) { // remove free list head
          self->heads[idx] = *p_current;
        } else { // remove internal free block
          *p_previous = *p_current;
        }
        block_set_extra_tag_size(p_current, extra, tag, req_size);
        return p_current;
      }
      else if (_block_size > capacity + 1) {
        uint32_t bitmap_idx = p_current - self->free;
        self->bitmap[bitmap_idx >> 5] |= 1 << (bitmap_idx & 0x1F);
        /// Remaining words form a new free block.
        value_t* p_new_fb = p_current + capacity + HEADER_SIZE;
        block_set_extra_tag_size(p_new_fb, 0, tag_FreeBlock, block_size(p_current) - capacity - HEADER_SIZE);
        /// insert new free block backto free list, potentially to another head

        block_set_extra_tag_size(current, extra, tag, req_size);
        return current;
      }
      previous = current;
      current  = (value_t*) *current;
    } 
  }

  value_t* block = self->heads[idx];
  uint32_t free_block_size = block_size(block);
  /// Set corresponding bit in bitmap
  uint32_t bitmap_idx = block - self->free;
  self->bitmap[bitmap_idx >> 5] |= 1 << (bitmap_idx & 0x1F);
  block_set_tag_size(block, tag, req_size);
  /// TODO: update free list, i.e. split
  /// remove the current free block from the free list
  if (free_block_size > req_size + 1) {
    /// TODO:
  }
  /// add the new free block -- after the split -- to the free list
  //self->free += total_size;
  value_t* next_fb = self->heads[idx][HEADER_SIZE] ;

  return block;

} 

void memory_collect_garbage(memory* self, value_t* root) {
  /// TODO:
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

  memory_collect_garbage(self, root); /// garbage collection

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
