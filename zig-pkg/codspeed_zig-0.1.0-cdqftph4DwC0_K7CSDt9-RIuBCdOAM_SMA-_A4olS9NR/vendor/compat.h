// Compatibility shims for cross-platform compilation
// This header provides compatibility macros for different libc implementations

#ifndef COMPAT_H
#define COMPAT_H

// Detect musl libc
// Musl doesn't define a specific macro, but we can detect it by checking for
// the absence of __GLIBC__ on Linux systems, or by checking features.h
#if defined(__linux__) && !defined(__GLIBC__) && !defined(__BIONIC__)
#define COMPAT_MUSL 1
#endif

// Musl libc compatibility
// Musl doesn't have separate *64 functions because all functions are 64-bit
// clean. Map the glibc LFS64 (Large File Support) functions to their standard
// equivalents.
#if defined(COMPAT_MUSL)
#define openat64 openat
#define fopen64 fopen
#define freopen64 freopen
#define lseek64 lseek
#define fseeko64 fseeko
#define ftello64 ftello
#define fgetpos64 fgetpos
#define fsetpos64 fsetpos
#define stat64 stat
#define fstat64 fstat
#define lstat64 lstat
#define fstatat64 fstatat
#define mmap64 mmap
#define off64_t off_t
#define ino64_t ino_t
#endif

#endif  // COMPAT_H
