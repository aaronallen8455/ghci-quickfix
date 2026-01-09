# ghci-quickfix 📝

This is a GHC plugin that will write diagnostics to a file during compilation,
which can then be used with `vim`/`nvim`'s quickfix feature. By default, the file
is `errors.err` but this can be customized (see plugin options).

**NOTE:** If you're using this plugin via
[`repl-alliance`](https://github.com/aaronallen8455/repl-alliance), you need to
explicitly enable it by passing `--fplugin-opt ReplAlliance:--quickfix` to GHC
or by setting the environment variable `GHC_QUICKFIX_ENABLED=true`.

## Usage

This plugin is intended to be used with GHCi or adjacent utilities such as
`ghcid` and `ghciwatch` as a development tool, not as a package dependency.

### Stack Projects

To use with a stack project (you may need to add `ghci-quickfix` to your
`extra-deps` first):

```bash
stack repl my-project --package ghci-quickfix --ghci-options='-fplugin GhciQuickfix'
```

### Cabal Projects

To use with a cabal project (you may need to run `cabal update` first):

```bash
cabal repl my-project --build-depends ghci-quickfix --repl-options='-fplugin GhciQuickfix'
```

### Vim/Neovim Integration

After starting your REPL with the plugin enabled, you can load errors in Vim:

```vim
:cfile errors.err
```

Or to update without jumping to the first error:

```vim
:cgetfile errors.err
```

## Plugin Options

Plugin options are passed using the `--fplugin-opt` flag. For example:

```bash
-fplugin GhciQuickfix -fplugin-opt GhciQuickfix:--quickfix-file=my-errors.err
```

### Available Options

- **`--quickfix-file=<path>`**
  Specify the output file path for diagnostics.
  Default: `errors.err`

- **`--quickfix-include-parser-errors`**
  Include parser errors in the quickfix file.
  Default: Parser errors are excluded (HLint typically reports them)

- **`--quickfix-path-replace=<needle>:<replace>`**
  Replace text in file paths in the quickfix output.
  Example: `--quickfix-path-replace=/home/user:/Users/user`
  Can be specified multiple times for multiple replacements.
  Useful for containerized or remote development environments.

- **`--quickfix`**
  Explicitly enable the plugin when using `pluginOffByDefault` (e.g., with repl-alliance).
  Alternative: Set environment variable `GHC_QUICKFIX_ENABLED=true`

## Compatibility

This plugin aims to support the 4 latest GHC major releases (i.e. `9.8.*` through `9.14.*`).
Check the cabal file for the currently supported versions.

## Output Format

The plugin generates quickfix entries in GCC-style format:

```
filename.hs:line:col: severity: message
```

This format is automatically recognized by Vim's quickfix system.
