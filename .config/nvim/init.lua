vim.pack.add({
  "https://github.com/ctrlpvim/ctrlp.vim",
  "https://github.com/kana/vim-altr",
  "https://github.com/kana/vim-operator-user",
  "https://github.com/gpanders/nvim-parinfer",
  "https://github.com/neovim/nvim-lspconfig",
  "https://github.com/Olical/conjure",
  {
    src = "https://github.com/nvim-treesitter/nvim-treesitter",
    version = "main",
  },
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

-- LSP
vim.lsp.enable("pylsp")
vim.lsp.enable("ts_ls")
vim.lsp.enable("docker_language_server")

-- TreeSitter
require("nvim-treesitter").install({
  "clojure",
  "go",
  "nix",
  "python",
  "rust",
  "typescript",
  "vim",
  "vimdoc",
})

vim.api.nvim_create_autocmd("PackChanged", {
  callback = function()
    pcall(require("nvim-treesitter").update)
  end,
})
