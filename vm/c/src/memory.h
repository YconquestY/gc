#ifndef MEMORY_H
#define MEMORY_H

#include <stdlib.h>
#include "vmtypes.h"

#define NUM_HEAD 32

typedef enum {
  tag_FreeBlock = 0x00,
  tag_Function = 0x01,
  tag_String = 0x02,
  /* A tag takes 6b. Highest header 2b indicates how much block capacity
   * exceeds block size.
   */
  tag_RegisterFrame = 0x3F
} tag_t;

typedef struct memory {
  value_t* start;
  value_t* end;
  value_t* free; // persistent once set
  value_t* bitmap;
  value_t  heads[NUM_HEAD]; // L₃VM virtual address
} memory;

// Returns a string identifying the memory module.
char* memory_get_identity(void);

// Create a memory module with `total_byte_size` memory.
memory* memory_new(size_t total_byte_size);

// Release the memory module.
void memory_free(memory* self);

// Get the address of the beginning of the memory area.
value_t* memory_get_start(memory* self);

// Get the address just after the end of the memory area.
value_t* memory_get_end(memory* self);

/**
 * @brief 
 * 
 * @param self 
 * @param bitmap
**/
void memory_set_bitmap(memory* self, value_t* bitmap);

// Set the heap start, following the code area.
void memory_set_heap_start(memory* self, value_t* heap_start);

/**
 * @brief 
 * 
 * @param self 
 * @param tag 
 * @param size 
 * @param root 
 * @return 
**/
value_t* memory_get_block(memory* self, tag_t tag, value_t size, value_t* root);

/**
 * @brief
 * 
 * @param self
 * @param root
**/
void memory_collect_garbage(memory* self, value_t* root);

// Allocate a block with the given `tag` and `size`, using `root` (the
// frame pointer) in case garbage collection has to be performed.
value_t* memory_allocate(memory* self, tag_t tag, value_t size, value_t* root);

// Copy `block` in a newly-allocated block, using `root` (the frame
// pointer) in case garbage collection has to be performed.
value_t* memory_copy_of_block(memory* self, value_t* block, value_t* root);

// Free `block` (without having to wait for next garbage collection).
void memory_free_block(memory* self, value_t* block);

#endif
