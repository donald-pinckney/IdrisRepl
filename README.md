# IdrisRepl

## Building and Running

### Dependencies

- Install [https://github.com/laserpants/baseline-idris](https://github.com/laserpants/baseline-idris)

### Configuration

To change between using `idris` and `idris2` just set `IDRIS_NAME` in `src/Config.idr`. Whichever one you use must be in your PATH.

### Building

Only tested so far on macOS, should probably work fine on Linux as well. No Windows support yet.

- Build: `idris --build idrisrepl.ipkg`
- Run: `./idrisrepl`

## Implementation Status

- [x] Readline support, with history
- [x] Tab completion of commands
- [ ] Tab completion of files
- [ ] Tab completion of Idris terms
- [x] `:h`, `:q`
- [x] Load file command: `:l`
- [x] Updating prompt
- [ ] Bolding prompt
- [ ] Highlighting (somewhat confused about how to parse this)
- [ ] All the rest of the commands!
    - [x] Evaluation, `:eval`
    - [x] `:t`
        - [ ] Broken with integer literals, i.e. `:t 5`
    - [x] `:r`
    - [ ] ...
- [ ] Windows support
- [ ] Testing infrastructure?
