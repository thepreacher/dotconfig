" Languages
" Most of these are from vim-polyglot
" (https://github.com/sheerun/vim-polyglot)
" Language plugin config should go in the appropriate ftplugin file

" JavaScript
let g:javascript_plugin_jsdoc = 1

" Go
" Make sure to call :GoUpdateBinaries on install
let g:go_gopls_enabled = 0
let g:go_highlight_extra_types = 1
let g:go_highlight_functions = 1
let g:go_def_mapping_enabled = 0
let g:go_highlight_function_calls = 1

" Python
let g:python_highlight_all = 1

" Elm
let g:LanguageClient_serverCommands = {
  \ 'elm': ['elm-language-server'],
  \ }

let g:LanguageClient_rootMarkers = {
  \ 'elm': ['elm.json'],
  \ }
