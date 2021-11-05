# Notes on Inkstone standard library

Core parts of the standard library should be available under the `std` namespace. A planned list of them includes:

- `std.buffer` - Byte buffer utilities
- `std.enum` - Enum and tagged unions
- `std.fiber` - Fiber/coroutine executor
- `std.io` - Printing and reading external data
- `std.iter` - Iterators
- `std.log` - Logging stuff
- `std.math` - Math utilities
- `std.num` - Number utilities
- `std.prelude` - A set of default-imported library
- `std.result` - Error handling
- `std.sync` - Sync primitives, channels and more (although we're single-threaded)
- `std.typings` - Type definition and checking

Additional standard library components that can be enabled/disabled should be available as packages. A planned list of them includes:

- `container` - Specialized containers
- `fs` - File systems
- `ink` - Interactive text game framework
- `net` - Network programming
