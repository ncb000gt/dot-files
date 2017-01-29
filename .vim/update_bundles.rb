#!/usr/bin/env ruby

git_bundles = [ 
	#language support
	"git://github.com/vim-ruby/vim-ruby.git",
	"git://github.com/kchmck/vim-coffee-script.git",
	"https://github.com/derekwyatt/vim-scala.git",
	"git://github.com/oscarh/vimerl.git",
	"https://github.com/pangloss/vim-javascript",
	"git://github.com/digitaltoad/vim-jade.git",
	"https://github.com/mxw/vim-jsx",
	"https://github.com/elixir-lang/vim-elixir",
	"https://github.com/python-mode/python-mode",
	"git://github.com/tpope/vim-markdown.git",
	"https://github.com/fatih/vim-go",

	#python utils
	"https://github.com/davidhalter/jedi-vim.git", # completion

	#git
	"git://github.com/tpope/vim-fugitive.git",
	"https://github.com/airblade/vim-gitgutter.git",

	#theming
	"git://github.com/altercation/vim-colors-solarized.git",
	"https://github.com/vim-airline/vim-airline",
	"https://github.com/morhetz/gruvbox.git",
	"https://github.com/arcticicestudio/nord-vim",

	#utils
	"git://github.com/tpope/vim-repeat.git", # let plugins use `.`
	"git://github.com/tpope/vim-surround.git", # surround with common start/ends
	"git://github.com/mileszs/ack.vim.git", # ack support
	"git://github.com/tsaleh/vim-tcomment.git", # commenting
	"git://github.com/godlygeek/tabular.git", # align by regex
	"https://github.com/tpope/vim-abolish", # handle string abbrev & replacement
]

vim_org_scripts = [
    ["IndexedSearch", "7062",  "plugin"],
    ["gist",          "15452", "plugin"],
]

require 'fileutils'
require 'open-uri'

if FileUtils.pwd().split('/').last() == '.vim'
    bundles_dir = File.join(File.dirname(__FILE__), ".vim", "bundle")

    git_bundles.each do |url|
        dir = url.split('/').last.sub(/\.git$/, '')
        module_dir = File.join('bundle', dir)
        if Dir.exists?(module_dir) and not Dir[module_dir+'/*'].empty?
            puts "  Updating #{dir} module"
            FileUtils.cd(module_dir)
            `git pull origin master`
            dots = module_dir.split('/').map {|i| '..'}
            dots = dots.join('/')
            FileUtils.cd(dots)
        elsif Dir.exists?(module_dir) and Dir[module_dir+'/*'].empty?
            puts "  Initializing module"
            FileUtils.cd('..')
            `git submodule init #{bundles_dir}/#{dir}`
            `git submodule update #{bundles_dir}/#{dir}`
            FileUtils.cd('.vim')
        else 
            puts "  Unpacking #{url} into #{dir}"
            FileUtils.cd('..')
            `git submodule add #{url} #{bundles_dir}/#{dir}`
            FileUtils.cd('.vim')
        end
        FileUtils.rm_rf(File.join(dir, ".git"))
    end

    FileUtils.cd(File.join("..", bundles_dir))
    vim_org_scripts.each do |name, script_id, script_type|
        puts "  Downloading #{name}"
        local_file = File.join(name, script_type, "#{name}.vim")
        FileUtils.rm_rf File.dirname(local_file)
        FileUtils.mkdir_p(File.dirname(local_file))
        File.open(local_file, "w") do |file|
            file << open("http://www.vim.org/scripts/download_script.php?src_id=#{script_id}").read
        end
    end
else
    puts "You must be in your .vim directory to run this script."
end
