if &compatible
  set nocompatible
endif

function! s:packager_init(packager) abort
  call a:packager.add('kristijanhusak/vim-packager', { 'type': 'opt' })
  call a:packager.add('dracula/vim', { 'name': 'dracula' })
  call a:packager.add('airblade/vim-gitgutter')
  call a:packager.add('kshenoy/vim-signature')
  call a:packager.add('norcalli/nvim-colorizer.lua', {'type': 'opt'})
  call a:packager.add('junegunn/vim-peekaboo')
  call a:packager.add('nelstrom/vim-visual-star-search')
  call a:packager.add('tpope/vim-commentary')
  call a:packager.add('tpope/vim-surround')
  call a:packager.add('tpope/vim-repeat')
  call a:packager.add('tpope/vim-unimpaired')
  call a:packager.add('junegunn/fzf', { 'do': './install --all && ln -s $(pwd) ~/.fzf'})
  call a:packager.add('junegunn/fzf.vim')
  " Helpful character metadata on `ga`
  call a:packager.add('tpope/vim-characterize')
  " Ctrl-a, ctrl-x for dates, times, etc.
  call a:packager.add('tpope/vim-speeddating')
  " Add readline bindings for command line mode
  call a:packager.add('tpope/vim-rsi')
  " Substitution and Coercion
  call a:packager.add('tpope/vim-abolish')
  call a:packager.add('chaoren/vim-wordmotion')
  call a:packager.add('kana/vim-textobj-user')
  call a:packager.add('kana/vim-textobj-entire')
  call a:packager.add('wellle/targets.vim')
  call a:packager.add('SirVer/ultisnips')
  call a:packager.add('adriaanzon/vim-emmet-ultisnips')
  call a:packager.add('tpope/vim-fugitive')
  " Git
  call a:packager.add('tpope/vim-fugitive')
  call a:packager.add('tpope/vim-rhubarb')
  " Unix utilities
  call a:packager.add('tpope/vim-eunuch')
  call a:packager.add('vim-test/vim-test')
  call a:packager.add('dense-analysis/ale')
  call a:packager.add('tpope/vim-projectionist')
  call a:packager.add('srstevenson/vim-picker')
  call a:packager.add('kyazdani42/nvim-web-devicons', {'type': 'opt'})
  call a:packager.add('kyazdani42/nvim-tree.lua', {'type': 'opt'})
  call a:packager.add('stsewd/gx-extended.vim')
  " LSP
  call a:packager.add('neovim/nvim-lspconfig', {'type': 'opt'})
  call a:packager.add('nathunsmitty/diagnostic-nvim', {'type': 'opt'})
  call a:packager.add('lewis6991/gitsigns.nvim', {'requires': 'nvim-lua/plenary.nvim'})
  call a:packager.add('haorenW1025/completion-nvim', {'requires': [
  \ ['nvim-treesitter/completion-treesitter', {'requires': 'nvim-treesitter/nvim-treesitter'}],
  \ {'name': 'steelsojka/completion-buffers', 'opts': {'type': 'opt'}},
  \ 'kristijanhusak/completion-tags',
  \ ]})
  call a:packager.add('steelsojka/completion-buffers', {'type': 'opt'})
  call a:packager.add('tjdevries/nlua.nvim', {'type': 'opt'})
  call a:packager.add('autozimu/LanguageClient-neovim', { 'do': 'bash install.sh' })
  " Treesitter
  call a:packager.add('nvim-treesitter/playground', {'type': 'opt'})
  call a:packager.add('fatih/vim-go', { 'do': ':GoInstallBinaries'})
  " Syntax highlighting for github's hub tool
  call a:packager.add('jez/vim-github-hub')
  " Handlebars
  call a:packager.add('mustache/vim-mustache-handlebars')
  " EJS
  call a:packager.add('briancollins/vim-jst')
  " CSS, Less, Sass
  call a:packager.add('cakebaker/scss-syntax.vim')
  call a:packager.add('groenewege/vim-less')
  " JavaScript
  call a:packager.add('pangloss/vim-javascript')
  call a:packager.add('styled-components/vim-styled-components', {'branch': 'main'})
  call a:packager.add('maxmellon/vim-jsx-pretty')
  " TypeScript
  "call a:packager.add('HerringtonDarkholme/yats.vim')
  " Svelte
  call a:packager.add('evanleck/vim-svelte')
  " JSON
  call a:packager.add('elzr/vim-json')
  " TOML
  call a:packager.add('cespare/vim-toml')
  " GraphQL
  call a:packager.add('jparise/vim-graphql')
  " Ruby
  call a:packager.add('vim-ruby/vim-ruby')
  " Ruby on Rails
  call a:packager.add('tpope/vim-rails', {'type': 'opt'})
  call a:packager.add('tpope/vim-endwise')
  call a:packager.add('tpope/vim-rake', {'type': 'opt'})
  call a:packager.add('tpope/vim-bundler', {'type': 'opt'})
  " Rust
  call a:packager.add('rust-lang/rust.vim')
  " Python
  call a:packager.add('vim-python/python-syntax')
  " Elixir
  call a:packager.add('elixir-editors/vim-elixir')
  " Julia
  call a:packager.add('JuliaEditorSupport/julia-vim')
  " Elm
  call a:packager.add('andys8/vim-elm-syntax')
endfunction

packadd vim-packager
call packager#setup(function('s:packager_init'))
