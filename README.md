# StrictLLVM

LLVM compiler for [Strict](https://github.com/KonstantinSeurer/Strict).

## Dependencies

- LLVM

## TODOs

- [x] CLI
- [ ] Build system
  - [x] Source file scanning
  - [ ] Caching
- [ ] Parser
  - [x] Structure parsing
  - [ ] Method body parsing
- [ ] Passes
  - [ ] Pre link validation
  - [ ] Link
  - [ ] Post link validation
  - [ ] Optimize
  - [ ] Lower to IR
- [ ] Module linking
  - [ ] Static linking with lld
  - [ ] Dynamic linking with lld
  - [ ] Standard IR
