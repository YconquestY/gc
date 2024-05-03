#ifndef BLOCK_HEADER_H
#define BLOCK_HEADER_H

#include "vmtypes.h"
#include "memory.h"

// Header management

#define HEADER_SIZE 1

// Pack a block `tag` (8 bits) and `size` (24 bits) into a header word.
inline static value_t header_pack(tag_t tag, value_t size) {
  return ((value_t)tag << 24) | size;
}

// Extract the tag of `header`.
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

// Set the header of `block`.
inline static void block_set_header(value_t* block, value_t header) {
  block[-HEADER_SIZE] = header;
}

inline static void block_set_tag_size(value_t* block, tag_t tag, value_t size) {
  block[-HEADER_SIZE] = header_pack(tag, size);
}

// Get the tag of `block`.
inline static tag_t block_tag(const value_t* block) {
  return header_unpack_tag(block_header(block));
}

// Get the size of `block`.
inline static tag_t block_size(const value_t* block) {
  return header_unpack_size(block_header(block));
}

#endif
