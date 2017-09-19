let os = substitute(system('uname'), "\n", "", "")

if os == "Linux"
    " Needed on some linux distros.
    " see http://www.adamlowe.me/2009/12/vim-destroys-all-other-rails-editors.html
    filetype off 
endif 

" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" turn ale on
filetype off
call pathogen#infect()
let &runtimepath.=',~/.vim/bundle/ale'
filetype plugin on

" set tab width, 1 tab should be 1 spaces
set tabstop=2
set softtabstop=2
set shiftwidth=2
"set expandtab

set list

" Lose the GUI
if has("gui_running")
    set guioptions=egmrt
    set guifont=Inconsolata\ 8
endif

" Increase or decrease the font size
let s:pattern = '^\(.* \)\([1-9][0-9]*\)$'
let s:minfontsize = 6
let s:maxfontsize = 16
function! AdjustFontSize(amount)
  if has("gui_gtk2") && has("gui_running")
    let fontname = substitute(&guifont, s:pattern, '\1', '')
    let cursize = substitute(&guifont, s:pattern, '\2', '')
    let newsize = cursize + a:amount
    if (newsize >= s:minfontsize) && (newsize <= s:maxfontsize)
      let newfont = fontname . newsize
      let &guifont = newfont
    endif
    set columns=120
  else
    echoerr "You need to run the GTK2 version of Vim to use this function."
  endif
endfunction

function! LargerFont()
  call AdjustFontSize(1)
endfunction
command! LargerFont call LargerFont()

function! SmallerFont()
  call AdjustFontSize(-1)
endfunction
command! SmallerFont call SmallerFont()

nnoremap <C-Up> :call LargerFont()<CR>
nnoremap <C-Down> :call SmallerFont()<CR>

" Show line numbers
set number

" bash-style tab completion
set wildmode=longest,list

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

" watch files for changes and reload them
set autoread
au CursorHold * checktime

" disable audible error bell and visual bell...
set visualbell
set vb t_vb=

set history=50 " keep 50 lines of command line history
set ruler " show the cursor position all the time
set showcmd " display incomplete commands
set incsearch " do incremental searching

" Allow flipping between dirty buffers 
set hidden

" Relative Line Numbers (to the cursor)
set rnu

" Make searches case-insensive unless there is a capitalized char in the
" search
set ignorecase 
set smartcase
"
" Start scrolling when certain distance from edges of window.
set scrolloff=8
set sidescrolloff=15
set sidescroll=1

" map leader to ,
let mapleader = ","

" Shortcut to rapidly toggle whitespace
nmap <leader>l :set list!<CR>
 
" Use the same symbols as TextMate for tabstops and EOLs
set listchars=tab:▸\ ,eol:¬

" For Win32 GUI: remove 't' flag from 'guioptions': no tearoff menu entries
" let &guioptions = substitute(&guioptions, "t", "", "g")

" Don't use Ex mode, use Q for formatting
map Q gq

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse') | set mouse=a | endif

set t_Co=16 " done to ensure Nord color scheme works properly

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
    au!

    " For all text files set 'textwidth' to 78 characters.
    autocmd FileType text setlocal textwidth=120

    " When editing a file, always jump to the last known cursor position.
    " Don't do it when the position is invalid or when inside an event handler
    " (happens when dropping a file on gvim).
    " Also don't do it when the mark is in the first line, that is the default
    " position when opening a file.
    autocmd BufReadPost *
      \ if line("'\"") > 1 && line("'\"") <= line("$") |
      \   exe "normal! g`\"" |
      \ endif

  augroup END

  " Markdown stuff
  autocmd BufRead *.mkd  set ai formatoptions=tcroqn2 comments=n:>
  autocmd BufRead *.md  set ai formatoptions=tcroqn2 comments=n:>
  autocmd BufRead *.markdown  set ai formatoptions=tcroqn2 comments=n:>

	" filetype specific settings
  autocmd FileType jade setlocal commentstring=//-\ %s
  autocmd FileType jade setlocal shiftwidth=2 tabstop=2 softtabstop=2
  autocmd FileType json setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
  autocmd FileType java setlocal shiftwidth=4 tabstop=4 softtabstop=4
  "autocmd FileType java set tags=~/.trunk-tags
  autocmd FileType python setlocal nowrap expandtab shiftwidth=4 tabstop=4 softtabstop=4
	" highlight characters past column 120
	autocmd FileType python highlight Excess ctermbg=DarkGrey guibg=Black
	autocmd FileType python match Excess /\%120v.*/

  " code completion
  "autocmd FileType python set omnifunc=python#completions
  autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
  autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
  autocmd FileType css set omnifunc=csscomplete#CompleteCSS
  autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags
  autocmd FileType php set omnifunc=phpcomplete#CompletePHP
  autocmd FileType c set omnifunc=ccomplete#Complete

  " config files need styles/syntax too?
  autocmd BufNewFile,BufRead .babelrc set filetype=javascript
  autocmd BufNewFile,BufRead .eslintrc,.reduxrc set filetype=json
  autocmd BufNewFile,BufRead gitconfig set filetype=gitconfig

else

  set autoindent " always set autoindenting on

endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif

" Tab mappings
"map <C-t><right> :tabn<cr>
"map <C-t><left> :tabp<cr>
"map <C-t> :tabnew<cr>

" Ack
if os == "Linux"
    let g:ackprg="ack-grep -H --nocolor --nogroup --column"
endif

"Backup control
" Don't write backup file if vim is being called by "crontab -e"
au BufWrite /private/tmp/crontab.* set nowritebackup
" Don't write backup file if vim is being called by "chpass"
au BufWrite /private/etc/pw.* set nowritebackup

" Kill the arrow keys to force HJKL usage.
noremap <Up> <nop>
noremap <Down> <nop>
noremap <Left> <nop>
noremap <Right> <nop>

" Load colorscheme
syntax enable
colorscheme nord
set colorcolumn=120

" spell check
set spell spelllang=en_us

set laststatus=2 "always show status
" Airline
let g:airline_powerline_fonts = 1

" gitgutter
let g:gitgutter_sign_modified = '•'
let g:gitgutter_sign_added = '+'
highlight GitGutterAdd guifg = '#A3E28B'

" python with virtualenv support
let g:virtualenv_auto_activate = 1

" pymode
let g:pymode_rope = 0
let g:pymode_syntax = 1
let g:pymode_syntax_all = 1
let g:pymode_syntax_indent_errors = g:pymode_syntax_all
let g:pymode_lint = 0
let g:pymode_folding = 0
let g:pymode_python = 'python3'
let g:pymode_options_max_line_length = 120

" golang
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_fields = 1
let g:go_highlight_types = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_fmt_command = "goimports"

" ale configuration
let g:ale_sign_error = 'EE'
let g:ale_sign_warning = 'WW'
let g:ale_sign_column_always = 1
let g:ale_python_flake8_args = '--max-line-length=120'
let g:ale_linters = {
\   'python': ['flake8'],
\   'javascript': ['eslint'],
\   'go': ['golint'],
\}
nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)

nnoremap <leader>] :MtaJumpToOtherTag<cr>
let g:mta_filetypes = {
    \ 'html' : 1,
    \ 'xhtml' : 1,
    \ 'xml' : 1,
    \ 'jinja' : 1,
    \ 'javascript.jsx' : 1,
    \}

let g:closetag_filenames = '*.html,*.xhtml,*.xml,*.jsx,*.html.erb'

" Move last thing yanked to system clipboard.
nnoremap <leader>yC :let @+=@"<cr>:echo "copied!"<cr>

" Format Json and set filetype (adapted from
" coderwall.com/p/faceag/format-json-in-vim)
noremap <silent> =JF :%!python -m json.tool<CR> :setfiletype json<CR>

" unite config
if executable('ag')
	" Use ag (the silver searcher)
	" https://github.com/ggreer/the_silver_searcher
	let g:unite_source_grep_command = 'ag'
	let g:unite_source_grep_default_opts =
				\ '-i --line-numbers --nocolor ' .
				\ '--nogroup --hidden --ignore ' .
				\ '''.hg'' --ignore ''.svn'' --ignore' .
				\ ' ''.git'' --ignore ''.bzr'''
	let g:unite_source_grep_recursive_opt = ''
