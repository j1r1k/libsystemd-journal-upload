# libsystemd-journal-upload

Library for building journald log HTTP uploader.

## Notes

- Stores a cursor to last uploaded log record and resumes from that point after interrupt.
- Performs HTTP retries using [retry](https://hackage.haskell.org/package/retry)
- [libsystemd-journal](https://github.com/j1r1k/libsystemd-journal)
## TODO

- Support syslog remote server
