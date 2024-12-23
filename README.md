# **StarPolyCalc**

`StarPolyCalc` is a tool designed to compute the **Star Polynomial**, adapted from [`ZZPolyCalc`](https://github.com/solccp/zzcalculator). It inherits the core functionalities of `ZZPolyCalc` while introducing optimizations and extensions tailored for star-shaped molecular structures.

Building on the foundation of `ZZPolyCalc`, `StarPolyCalc` focuses on efficiently analyzing and processing star patterns within molecular graphs.

## **Reference**
Original tool: [`ZZPolyCalc`](https://github.com/solccp/zzcalculator)

Installation Requirements
=========================

* A Fortran 2008 compliant compiler (Intel compiler recommended)
* A C compiler
* GNU make

Additionally, though optional, the following are recommended:

* CMake (version 2.8 or newer)
* [xxHash library](https://github.com/Cyan4973/xxHash) for the XXH128 hash or [OpenSSL library](https://github.com/openssl/openssl) for the MD5 or SHA256 hash.

If these hashing libraries are not provided, the code will compile with a less optimized MD5 hashing algorithm.
The XXH128 hash is the fastest, followed by MD5. XXH128 is recommended for regular use. To employ SHA256, an appropriate selection must be manually activated in CMakeLists.txt. (See: Optional modifications at compile time)
