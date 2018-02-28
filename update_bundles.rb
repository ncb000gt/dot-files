#!/usr/bin/env ruby

git_bundles = [ 
	#language support
	"git://github.com/vim-ruby/vim-ruby.git",
	"https://github.com/derekwyatt/vim-scala.git",
	"git://github.com/oscarh/vimerl.git",
	"https://github.com/elixir-lang/vim-elixir",
	"https://github.com/pangloss/vim-javascript",
	"https://github.com/mxw/vim-jsx",
	"https://github.com/elzr/vim-json",
	"https://github.com/python-mode/python-mode",
	"https://github.com/fatih/vim-go",
	"git://github.com/tpope/vim-markdown.git",
	"https://github.com/artur-shaik/vim-javacomplete2.git",
	"https://github.com/dart-lang/dart-vim-plugin",

	#python utils
	"https://github.com/jmcantrell/vim-virtualenv",

	#git
	"git://github.com/tpope/vim-fugitive.git",
	"https://github.com/airblade/vim-gitgutter.git",

	#theming
	"https://github.com/vim-airline/vim-airline",
	"https://github.com/arcticicestudio/nord-vim",

	#utils
	"git://github.com/tpope/vim-repeat.git", # let plugins use `.`
	"git://github.com/tpope/vim-surround.git", # surround with common start/ends
	"https://github.com/Raimondi/delimitMate", # close brackets, parens, etc.
	"git://github.com/mileszs/ack.vim.git", # ack support
	"https://github.com/tomtom/tcomment_vim", # commenting
	"git://github.com/godlygeek/tabular.git", # align by regex
	"https://github.com/tpope/vim-abolish", # handle string abbrev & replacement
	"https://github.com/pbrisbin/vim-mkdir", # mkdir -p
	"https://github.com/ap/vim-css-color", # bg highlight of css colors
	"https://github.com/w0rp/ale", # async linting
	"https://github.com/Valloric/MatchTagAlways", # html style tag matching
	"https://github.com/alvan/vim-closetag", # automatically create closing tag
	"https://github.com/editorconfig/editorconfig-vim", # uniform editor configs
	"https://github.com/Shougo/denite.nvim", # user interfaces
	"https://github.com/airblade/vim-rooter", # set vim working dir
	"https://github.com/Valloric/YouCompleteMe", # code completion
]

vim_org_scripts = [
    ["IndexedSearch", "7062",  "plugin"],
    ["gist",          "15452", "plugin"],
]

require 'fileutils'
require 'open-uri'

if FileUtils.pwd().split('/').last() == 'dot-files'
    vim_bundles_dir = "vim_bundles"
    bundles_dir = File.join(File.dirname(__FILE__), vim_bundles_dir)

    git_bundles.each do |url|
        dir = url.split('/').last.sub(/\.git$/, '')
        module_dir = File.join(vim_bundles_dir, dir)

        if Dir.exists?(module_dir) and not Dir[module_dir+'/*'].empty?
            puts "-- Updating #{dir} module"
            FileUtils.cd(module_dir)
            `git pull origin master`
            dots = module_dir.split('/').map {|i| '..'}
            dots = dots.join('/')
            FileUtils.cd(dots)

        elsif Dir.exists?(module_dir) and Dir[module_dir+'/*'].empty?
            puts "-- Initializing module"
            `git submodule update --init --recursive #{bundles_dir}/#{dir}`

        else 
            puts "-- Unpacking #{url} into #{dir}"
            `git submodule add --force #{url} #{bundles_dir}/#{dir}`

        end

        FileUtils.rm_rf(File.join(dir, ".git"))
    end

    vim_org_scripts.each do |name, script_id, script_type|
        puts "-- Downloading #{name}"
        local_file = File.join(bundles_dir, name, script_type, "#{name}.vim")
        FileUtils.rm_rf File.dirname(local_file)
        FileUtils.mkdir_p(File.dirname(local_file))
        File.open(local_file, "w") do |file|
            file << open("https://vim.sourceforge.io/scripts/download_script.php?src_id=#{script_id}").read
        end
    end
else
    puts "You must be in your dot-files directory to run this script."
end
