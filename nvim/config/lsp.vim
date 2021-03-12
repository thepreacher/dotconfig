command! LspHover lua vim.lsp.buf.hover()<CR>
command! LspDisable lua vim.lsp.stop_client(vim.lsp.get_active_clients())<CR>

packadd nvim-lspconfig
packadd diagnostic-nvim
packadd nlua.nvim
" LSP config, in lua
lua require("lspconfig")
setlocal omnifunc=v:lua.vim.lsp.omnifunc

" Show errors after 1 second
set updatetime=1000

let g:diagnostic_insert_delay =1
let g:diagnostic_enable_ale = 1
let g:ale_lint_on_save = 1
let g:elm_format_autosave = 1
let g:ale_fix_on_save = 1

" Language Client
let g:LanguageClient_serverCommands = {
    \ 'rust': ['~/.cargo/bin/rustup', 'run', 'stable', 'rls'],
    \ }
