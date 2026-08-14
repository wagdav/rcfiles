vim.pack.add({
  "https://github.com/ctrlpvim/ctrlp.vim",
  "https://github.com/kana/vim-altr",
  "https://github.com/kana/vim-operator-user",
  "https://github.com/gpanders/nvim-parinfer",
  "https://github.com/neovim/nvim-lspconfig",
  "https://github.com/Olical/conjure",
  "https://github.com/tpope/vim-fugitive",
  "https://github.com/tpope/vim-surround",
  "https://github.com/tpope/vim-unimpaired",
  "https://github.com/vim-airline/vim-airline",

  -- Clojure
  "https://github.com/tpope/vim-dispatch",
  "https://github.com/clojure-vim/vim-jack-in",
  "https://github.com/radenling/vim-dispatch-neovim",
})

vim.opt.laststatus = 2

vim.opt.textwidth = 0
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.linebreak = true

vim.opt.spellsuggest = "5"

vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.g.maplocalleader = ","
vim.g.netrw_browsex_viewer = "xdg-open"

vim.opt.list = true
vim.opt.listchars = {
  tab = "→ ",
  trail = "•",
}

-- CtrlP
vim.g.ctrlp_user_command = {
  ".git",
  "cd %s && git ls-files -co --exclude-standard",
}

vim.g.ctrlp_root_markers = { ".ctrlp" }

-- Conjure
vim.g["conjure#log#hud#enabled"] = false

-- Map <Esc> to exit terminal-mode
vim.keymap.set("t", "<Esc>", [[<C-\><C-n>]])

-- LSP (see :help lspconfig-all)

-- Format on save
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if client and client:supports_method("textDocument/formatting") then
      vim.api.nvim_create_autocmd("BufWritePre", {
        buffer = args.buf,
        callback = function()
          vim.lsp.buf.format({ bufnr = args.buf, id = client.id, timeout_ms = 2000 })
        end,
      })
    end
  end,
})

vim.lsp.enable("docker_language_server")
vim.lsp.enable("pylsp")

vim.lsp.config('rust_analyzer', {
  cmd = {
    "nix", "shell",
     "nixpkgs#rust-analyzer", "nixpkgs#rustc", "nixpkgs#cargo",
     "--command", "rust-analyzer"
  },
  root_dir = function(bufnr, on_dir)
    local fname = vim.api.nvim_buf_get_name(bufnr)
    on_dir(vim.fs.root(fname, { "Cargo.toml", "rust-project.json", ".git" }))
  end,
})
vim.lsp.enable("rust_analyzer")

vim.lsp.config('zls', {
  cmd = {
    "nix", "shell",
    "nixpkgs#zig", "nixpkgs#zls",
    "--command", "zls"
  },
})
vim.lsp.enable("zls")

vim.lsp.enable("ts_ls")
