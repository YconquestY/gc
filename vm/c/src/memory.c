#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <string.h>

#include "address.h"
#include "memory.h"
#include "fail.h"
#include "block_header.h"
#include "util.h"

#define TFB_0 (self->bitmap - 258)
#define TFB_1 (self->bitmap - 258 - 259)

/*
struct memory {
  value_t* start;
  value_t* end;
  value_t* free;
};
*/
char* memory_get_identity() {
  return "Mark & sweep GC";
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
                          value_t req_size) {
  uint8_t extra = req_size ? 0 : 1;
  uint32_t capacity = req_size + extra;

  uint32_t idx = min(capacity - 1, NUM_HEAD - 1);
  for ( ; idx < NUM_HEAD; idx++) {
    if (idx != NUM_HEAD - 1 && capacity == idx) {
      continue;
    }
    value_t v_previous = UINT32_MAX,
            v_current = self->heads[idx];
    while (v_current != UINT32_MAX) {
      value_t* p_previous = addr_v_to_p(self->start, v_previous),
             * p_current = addr_v_to_p(self->start, v_current);
      value_t _block_size = block_size(p_current);
      if (_block_size == capacity) {
        uint32_t bitmap_idx = (uint32_t) (p_current - self->free);
        self->bitmap[bitmap_idx >> 5] |= ((uint32_t) 1) << (bitmap_idx & 0x1F); // side effect
        if (v_previous == UINT32_MAX) { // remove free list head
          self->heads[idx] = *p_current;
        } else { // remove internal free block
          *p_previous = *p_current;
        }
        block_set_tag_size(p_current, tag, req_size);
        return p_current;
      }
      else if (_block_size > capacity + 1) {
        uint32_t bitmap_idx = (uint32_t) (p_current - self->free);
        self->bitmap[bitmap_idx >> 5] |= ((uint32_t) 1) << (bitmap_idx & 0x1F); // side effect
        /// Remaining words form a new free block.
        value_t* p_new_fb = p_current + capacity + HEADER_SIZE;
        value_t new_fb_size = _block_size - capacity - HEADER_SIZE;
        block_set_tag_size(p_new_fb, tag_FreeBlock, new_fb_size);
        /// remove current free block
        if (v_previous == UINT32_MAX) { // free list head
          self->heads[idx] = *p_current;
        } else { // internal free block
          *p_previous = *p_current;
        }
        /// insert new free block backto free list, potentially to another head
        uint32_t _idx = min(new_fb_size - 1, NUM_HEAD - 1);
        *p_new_fb = self->heads[_idx];
        self->heads[_idx] = addr_p_to_v(self->start, p_new_fb);
        /// update allocated block
        block_set_tag_size(p_current, tag, req_size); // size 0, capacity 1 if 0 requested
        return p_current;
      }
      v_previous = v_current;
      v_current = *p_current;
    }
  }
  return NULL;
}

/**
 * @brief 
 * 
 * @param self 
 * @param root 
**/
void memory_dfs(memory* self, value_t* root) {
  value_t* ptr = root;
  for (uint32_t i = 0; i < block_size(root); i++) { // not block capacity!
    /// Verify pointer points inside heap
    value_t v_forage = *ptr;
    if ((v_forage &  0x3)                                || // value looks like a pointer
        v_forage <  addr_p_to_v(self->start, self->free) ||
        v_forage >= addr_p_to_v(self->start, self->end)) {
      ptr++;
      continue;
    }

    value_t* p_forage = addr_v_to_p(self->start, v_forage);
    uint32_t bitmap_idx = (uint32_t) (p_forage - self->free);
    /// check bitmap entry
    if (self->bitmap[bitmap_idx >> 5] & (((uint32_t) 1) << (bitmap_idx & 0x1F))) { // value is almost a pointer
      /* reset bitmap entry
       *
       * A catch is that the current frame, as root, will not be marked. This
       * is not a problem now that we do not sweep top-frame blocks for
       * garbage collection.
       */
      self->bitmap[bitmap_idx >> 5] &= ~(((uint32_t) 1) << (bitmap_idx & 0x1F));
      memory_dfs(self, p_forage);
    }
    ptr++;
  }
}

/**
 * @brief 
 * 
 * @param self 
**/
void memory_sweep(memory* self) {
  /// delete original free list
  value_t* tails[NUM_HEAD];
  for (uint32_t i = 0; i < NUM_HEAD; i++) {
    self->heads[i] = UINT32_MAX;
    tails[i] = &self->heads[i];
  }

  value_t* fb_wip = NULL;
  uint32_t fb_size = 0;
  
  value_t* sweep = &self->free[HEADER_SIZE];
  /// linearly traverse heap
  for ( ; sweep < self->end; ) {
    uint32_t bitmap_idx = (uint32_t) (sweep - self->free);
    bool bit_set = self->bitmap[bitmap_idx >> 5] & (((uint32_t) 1) << (bitmap_idx & 0x1F));
    // allocated reachable block
    if (block_tag(sweep) != tag_FreeBlock && !bit_set) {
      /// set bitmap entry of reachable blocks backto 1
      self->bitmap[bitmap_idx >> 5] |= ((uint32_t) 1) << (bitmap_idx & 0x1F);

      if (fb_wip) {
        /// reset memory content
        memset(fb_wip, 0, fb_size * sizeof(value_t));
        /// insert new (coalesced) free block
        block_set_tag_size(fb_wip, tag_FreeBlock, fb_size);
        uint32_t idx = min(fb_size - 1, NUM_HEAD - 1);
        *tails[idx] = addr_p_to_v(self->start, fb_wip); // insert to tail
        tails[idx] = fb_wip;

        fb_wip = NULL; // reset coalesced free block pointer
        fb_size = 0;   // reset coalesced free block size
      }
    }
    // free block or allocated unreachable block
    else {
      if (!fb_wip) { // new free block WIP
        fb_wip = sweep;
        fb_size = block_capacity(sweep);
      } else {
        fb_size += block_capacity(sweep) + HEADER_SIZE;
      }
      /// reset bitmap entry of allocated unreachable block to 0
      self->bitmap[bitmap_idx >> 5] &= ~(((uint32_t) 1) << (bitmap_idx & 0x1F));
    }
    sweep += block_capacity(sweep) + HEADER_SIZE;
  }
  /* In case the trailing blocks of the heap are all free, perform an
   * additional round of "collection".
   */
  if (fb_wip) {
    memset(fb_wip, 0, fb_size * sizeof(value_t));
    block_set_tag_size(fb_wip, tag_FreeBlock, fb_size);
    uint32_t idx = min(fb_size - 1, NUM_HEAD - 1);
    *tails[idx] = addr_p_to_v(self->start, fb_wip); // insert to tail
    tails[idx] = fb_wip;
  }
  
  for (uint32_t i = 0; i < NUM_HEAD; i++) {
    *tails[i] = UINT32_MAX;
  }
}

/**
 * @brief 
 * 
 * @param self 
 * @param root 
**/
void memory_collect_garbage(memory* self, value_t* root) {
  memory_dfs(self, root); /// mark
  if (root == TFB_0) {
    if (root[1] == addr_p_to_v(self->start, TFB_1)) {
      memory_dfs(self, TFB_1);
    }
  } else if (root == TFB_1) {
    if (root[1] == addr_p_to_v(self->start, TFB_0)) {
      memory_dfs(self, TFB_0);
    }
  } else {
    fail("root frame is not top-frame block");
  }
  memory_sweep(self);     /// sweep
}

value_t* memory_allocate(memory* self,
                         tag_t tag,
                         value_t size,
                         value_t* root) {
  value_t* block = memory_get_block(self, tag, size);
  if (!block) {
    memory_collect_garbage(self, root); /// garbage collection
  } else {
    return block;
  }

  block = memory_get_block(self, tag, size);
  if (!block) {
    fail("no memory left (block of size %u requested)", size);
  } else {
    return block;
  }
}

value_t* memory_copy_of_block(memory* self, value_t* block, value_t* root) {
  tag_t tag = block_tag(block);
  value_t size = block_size(block);
  value_t* copy = memory_allocate(self, tag, size, root);
  memcpy(copy, block, size * sizeof(value_t));
  return copy;
}

void memory_free_block(memory* self, value_t* block) {
  /// reset bitmap entry
  uint32_t bitmap_idx = (uint32_t) (block - self->free);
  self->bitmap[bitmap_idx >> 5] &= ~(((uint32_t) 1) << (bitmap_idx & 0x1F));
  /// reset block tag
  value_t fb_size = block_capacity(block);
  block_set_tag_size(block, tag_FreeBlock, fb_size);
  /// insert block backto free list in ascending address
  uint32_t idx = min(fb_size - 1, NUM_HEAD - 1);
  value_t* ptr = &self->heads[idx];
  while(*ptr < addr_p_to_v(self->start, block) && *ptr != UINT32_MAX) {
    ptr = addr_v_to_p(self->start, *ptr);
  }
  *block = *ptr;
  *ptr = addr_p_to_v(self->start, block);
  /// clear freed block
  memset(&block[1], 0, (fb_size - 1) * sizeof(value_t));
}
