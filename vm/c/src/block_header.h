#ifndef BLOCK_HEADER_H
#define BLOCK_HEADER_H

#include "vmtypes.h"
#include "memory.h"

/* The initial header format is
 *     tag | size
 *      8b |  24b
 * which is not expressive enough when considering garbage collection. Suppose
 * a free block has size 2, and that the requested size is 1. There will be 1
 * word left if we allocate such a free block. However, the left word cannot
 * sustain a new free block, which requires at least 2 words (1 header and 1
 * next-free-block pointer). The solution is the modify header format:
 *     extra | tag | size
 *        2b |  6b |  24b
 * and introduce the notion of "capacity". `extra` field is the number of words
 * that block capacity exceeds block size. Note that block capacity is as most
 * 2 larger than block size, demonstrated below:
 *     requested block: size 0
 *     free list      : only 1 free block of size 2
 * 
 * Note that
 * 1. block capacity is necessary for strings and functions in that they are
 *    represented as block (of characters/closure). Block size matters;
 * 2. a free block or top-frame block must have its `extra` field set to 0.
 */

// Header management

#define HEADER_SIZE 1

/**
 * @brief Pack a block capacity (2 bits),`tag` (6 bits), and `size` (24 bits)
 *      into a header word.
 * @param extra
 * @param tag
 * @param size
 * @return 
**/
inline static value_t header_pack(uint8_t extra, tag_t tag, value_t size) {
  return ((value_t) extra << 30) | ((value_t) tag << 24) | size;
}

/**
 * @brief
 * 
 * @param header
 * @return
**/
inline static uint8_t header_unpack_extra(value_t header) {
  return (uint8_t) (header >> 30);
}

// Extract the tag of `header`.
inline static tag_t header_unpack_tag(value_t header) {
  return (tag_t)((header >> 24) & 0x3F);
}

// Extract the size of `header`.
inline static value_t header_unpack_size(value_t header) {
  return header & 0xFFFFFF;
}

// Get the header of `block`.
inline static value_t block_header(const value_t* block) {
  return block[-HEADER_SIZE];
}

// Set the header of `block`; unused in any snippet
inline static void block_set_header(value_t* block, value_t header) {
  block[-HEADER_SIZE] = header;
}

inline static void block_set_extra_tag_size(value_t* block, uint8_t extra, tag_t tag, value_t size) {
  block[-HEADER_SIZE] = header_pack(extra, tag, size);
}

/**
 * @brief
 * 
 * @param block
 * @return
**/
inline static uint8_t block_extra(const value_t* block) {
  return header_unpack_extra(block_header(block));
}

// Get the tag of `block`.
inline static tag_t block_tag(const value_t* block) {
  return header_unpack_tag(block_header(block));
}

/** 
 * @brief Get the size of `block`.
 * 
 * Used in all cases other than `block_capacity`
 * 
 * @param block
 * @return
**/
inline static value_t block_size(const value_t* block) {
  return header_unpack_size(block_header(block));
}

/**
 * @brief Get the capacity of `block`, where block capacity is the sum of block
 *      size and `extra`.
 * 
 * Used in sweeping phase and `memory_free_block`
 * 
 * @param block
 * @return
**/
inline static value_t block_capacity(const value_t* block) {
  return block_size(block) + (value_t) block_extra(block);
}

#endif
