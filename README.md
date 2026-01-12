# ghci-quickfix 📝

This is a GHC plugin that will write diagnostics to a file during compilation,
which can then be used with `vim`/`nvim`'s quickfix feature. By default, the file
is `errors.err` in the project root directory but this can be customized (see plugin options).

**NOTE:** If you're using this plugin via
[`repl-alliance`](https://github.com/aaronallen8455/repl-alliance), you need to
explicitly enable it by passing `--fplugin-opt ReplAlliance:--quickfix` to GHC
or by setting the environment variable `GHCI_QUICKFIX_ENABLED=true`.

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

## Vim/Neovim Integration

After starting your REPL with the plugin enabled, you can load errors in Vim:

```vim
:cf errors.err
```

The `errors.err` argument can be omitted since it is the default file.

Or to update without jumping to the first error:

```vim
:cg errors.err
```

Navigate between error locations using `:cn` (next) and `:cp` (previous).

To get highlighting for errors and warnings in the editor, you can turn the
quickfix entries into diagnostics. The following snippet can be added to your
`init.lua` file to accomplish this.

```lua
-- [[ Quickfix to diagnostics ]]
-- Configure errorformat to distinguish between warning and error type.
vim.o.errorformat = '%f:%l:%c: %tarning: %m,%f:%l:%c: %trror: %m,' .. vim.o.errorformat

-- Create namespace for quickfix diagnostics
local qf_ns = vim.api.nvim_create_namespace('quickfix_diagnostics')

-- Configure diagnostic display for quickfix namespace
vim.diagnostic.config({
  underline = true,
  virtual_text = false,
  signs = true,
  update_in_insert = false,
}, qf_ns)

-- Helper function to create diagnostic entry from quickfix item
local function create_diagnostic_from_qf_item(item)
  local severity = vim.diagnostic.severity.ERROR

  -- Determine severity based on type field
  if item.type == 'W' or item.type == 'w' then
    severity = vim.diagnostic.severity.WARN
  end

  -- Handle column range - if no end_col, highlight to end of line
  local col_start = (item.col or 1) - 1
  local col_end = nil
  local end_lnum = nil

  if item.end_col and item.end_col > 0 and item.end_lnum then
    col_end = item.end_col
    end_lnum = item.end_lnum - 1
  end

  return {
    lnum = item.lnum - 1,
    col = col_start,
    end_lnum = end_lnum,
    end_col = col_end,
    severity = severity,
    message = item.text or '',
    source = 'quickfix',
  }
end

-- Function to convert quickfix entries to diagnostics and apply to all buffers
local function quickfix_to_diagnostics()
  -- Clear all quickfix diagnostics from all buffers
  vim.diagnostic.reset(qf_ns)

  local qf_list = vim.fn.getqflist()
  local diagnostics_by_buf = {}

  for _, item in ipairs(qf_list) do
    if item.bufnr > 0 and item.lnum > 0 then
      if not diagnostics_by_buf[item.bufnr] then
        diagnostics_by_buf[item.bufnr] = {}
      end
      table.insert(diagnostics_by_buf[item.bufnr], create_diagnostic_from_qf_item(item))
    end
  end

  -- Set diagnostics for each buffer
  for bufnr, diagnostics in pairs(diagnostics_by_buf) do
    vim.diagnostic.set(qf_ns, bufnr, diagnostics, {})
  end
end

-- Function to apply diagnostics for a specific buffer
local function apply_quickfix_diagnostics_for_buffer(bufnr)
  local qf_list = vim.fn.getqflist()
  local diagnostics = {}

  for _, item in ipairs(qf_list) do
    if item.bufnr == bufnr and item.lnum > 0 then
      table.insert(diagnostics, create_diagnostic_from_qf_item(item))
    end
  end

  vim.diagnostic.set(qf_ns, bufnr, diagnostics, {})
end

-- Auto-convert quickfix entries to diagnostics when loading from file
vim.api.nvim_create_autocmd('QuickFixCmdPost', {
  pattern = '[cg]file,[cg]getfile',
  callback = function()
    quickfix_to_diagnostics()
  end,
})

-- Reapply diagnostics when a buffer is read (handles newly opened buffers)
vim.api.nvim_create_autocmd('BufReadPost', {
  pattern = '*',
  callback = function(args)
    local qf_list = vim.fn.getqflist()
    if #qf_list == 0 then
      return
    end

    -- Only apply diagnostics for this specific buffer if it has quickfix entries
    local bufnr = args.buf
    for _, item in ipairs(qf_list) do
      if item.bufnr == bufnr then
        apply_quickfix_diagnostics_for_buffer(bufnr)
        return
      end
    end
  end,
})
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
  Alternative: Set environment variable `GHCI_QUICKFIX_FILE=<path>`

- **`--quickfix-include-parser-errors`**
  Include parser errors in the quickfix file.
  Default: Parser errors are excluded (HLint typically reports them)
  Alternative: Set environment variable `GHCI_QUICKFIX_INCLUDE_PARSER_ERRORS=true`

- **`--quickfix-path-replace=<needle>:<replace>`**
  Replace text in file paths in the quickfix output.
  Example: `--quickfix-path-replace=/home/user:/Users/user`
  Can be specified multiple times for multiple replacements.
  Useful for containerized or remote development environments.
  Alternative: Set environment variable `GHCI_QUICKFIX_PATH_REPLACE=<needle>:<replace>`

- **`--quickfix`**
  Explicitly enable the plugin when using `pluginOffByDefault` (e.g., with repl-alliance).
  Alternative: Set environment variable `GHCI_QUICKFIX_ENABLED=true`

## Output Format

The plugin generates quickfix entries in GCC-style format:

```
filename.hs:line:col: severity: message
```

This format is automatically recognized by Vim's quickfix system.

## Compatibility

This plugin aims to support the 4 latest GHC major releases (i.e. `9.6.*` through `9.12.*`).
Check the cabal file for the currently supported versions.
