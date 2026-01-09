# ghci-quickfix

This is a GHC plugin that will write diagnostics to a file during compilation,
which can then be used with vim/nvim's quickfix feature. By default, the file
is `errors.err` but this can be customized (see plugin options).

*NOTE:* If you're using this plugin via
[`repl-alliance`](https://github.com/aaronallen8455/repl-alliance), you need to
explicitly enable it by passing `--fplugin-opt ReplAlliance:--quickfix` to GHC
or by setting the environment variable `GHC_QUICKFIX_ENABLED=true`.

## Usage

This plugin is intended to be used with GHCi or adjacent utilities such as
`ghcid` and `ghciwatch` as a developement tool, not as a package dependency.
Here is an example command for starting a REPL for a stack project with the
`ghci-quickfix` plugin enabled (you may need to add `ghci-quickfix` to your
`extra-deps` first):

```
stack repl my-project --package ghci-quickfix --ghci-options='-fplugin GhciQuickfix'
```

likewise for a cabal project (you may need to run `cabal update` first):

```
cabal repl my-project --build-depends ghci-quickfix --repl-options='-fplugin GhciQuickfix'
```

## Plugin Options

Plugin options are passed using the `--fplugin-opt` GHC flag. For example:
`-fplugin GhciQuickfix -fplugion-opt GhciQuickfix:--quickfix-include-parser-errors`.

- `--quickfix-include-parser-errors`: Include parser errors in the quickfix file. By default, parser errors are excluded as tools like HLint typically report them.
