#ifndef BLOCK_HEADER_H
#define BLOCK_HEADER_H

#include "vmtypes.h"
#include "memory.h"

/* There are 2 challenges regarding block management:
 * 1. impossinlie split;
 *    Suppose the only avalilable free block has size 6. Given a request of
 *    size 5, block allocation will fail because we cannot split a free block
 *    of size 1. A free block comprises at least a header and a pointer to the
 *    next block, which is 2 words in total. In this case, one may reject the
 *    request for perform garbage collection, see
 *    https://edstem.org/eu/courses/1102/discussion/114055?answer=215575
 * 2. request 0.
 *    A request of size 0 should be granted with a block of "size" 1. Otherwise,
 *    the block cannot be freed because it has no place for a pointer.
 * 
 * What makes it worse is that the 2 problems may come togher. We cannot
 * naively set the block header with a size larger than what is requested (by
 * 1 or 2) to, e.g., preserve the accuracy of instruction `BSIZ`. As a
 * solution, we introduct the concept of block capacity, which is the sum of
 * requested size and the "extra" word allocated to the request. One may be
 * tempted to record the "extra" field as block metadata, and the most viable
 * way is to spare 2b from block tag to "extra", i.e.,
 *     initial header → modified header
 *     tag | size       extra | tag | size
 *      8b |  24b          2b |  6b |  24b
 * This is incorrect. Tag must be 8b even though we have only specified 4
 * distinct ones. `~/compiler/src/l3/BlockTag.scala` can dynamically generate
 * new flags (also see
 * https://edstem.org/eu/courses/1102/discussion/114055?comment=217765). 
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
/*
inline static value_t header_pack(uint8_t extra, tag_t tag, value_t size) {
  return ((value_t) extra << 30) | ((value_t) tag << 24) | size;
}
*/
inline static value_t header_pack(tag_t tag, value_t size) {
  return ((value_t) tag << 24) | size;
}

/**
 * @brief
 * 
 * @param header
 * @return
**/
/*
inline static uint8_t header_unpack_extra(value_t header) {
  return (uint8_t) (header >> 30);
}
*/
// Extract the tag of `header`.
/*
inline static tag_t header_unpack_tag(value_t header) {
  return (tag_t)((header >> 24) & 0x3F);
}
*/
inline static tag_t header_unpack_tag(value_t header) {
  return (tag_t)((header >> 24) & 0xFF);
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
/*
inline static void block_set_extra_tag_size(value_t* block, uint8_t extra, tag_t tag, value_t size) {
  block[-HEADER_SIZE] = header_pack(extra, tag, size);
}
*/
inline static void block_set_tag_size(value_t* block, tag_t tag, value_t size) {
  block[-HEADER_SIZE] = header_pack(tag, size);
}

/**
 * @brief
 * 
 * @param block
 * @return
**/
/*
inline static uint8_t block_extra(const value_t* block) {
  return header_unpack_extra(block_header(block));
}
*/
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
  return (block_size(block) == 0) ? (value_t) 1 : block_size(block);
}

#endif
