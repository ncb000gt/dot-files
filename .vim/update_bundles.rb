#!/usr/bin/env ruby

git_bundles = [ 
    "git://github.com/astashov/vim-ruby-debugger.git",
    "git://github.com/msanders/snipmate.vim.git",
    "git://github.com/scrooloose/nerdtree.git",
    "git://github.com/timcharper/textile.vim.git",
    "git://github.com/tpope/vim-cucumber.git",
    "git://github.com/tpope/vim-fugitive.git",
    "git://github.com/tpope/vim-git.git",
    "git://github.com/tpope/vim-haml.git",
    "git://github.com/tpope/vim-markdown.git",
    "git://github.com/tpope/vim-rails.git",
    "git://github.com/tpope/vim-repeat.git",
    "git://github.com/tpope/vim-surround.git",
    "git://github.com/tpope/vim-vividchalk.git",
    "git://github.com/tsaleh/vim-align.git",
    "git://github.com/tsaleh/vim-shoulda.git",
    "git://github.com/tsaleh/vim-supertab.git",
    "git://github.com/tsaleh/vim-tcomment.git",
    "git://github.com/vim-ruby/vim-ruby.git",
    "git://github.com/kchmck/vim-coffee-script.git",
    "git://github.com/ewiplayer/vim-scala.git",
    "git://github.com/mileszs/ack.vim.git",
    "git://github.com/godlygeek/csapprox.git",
    "git://github.com/vim-scripts/LustyExplorer.git",
]

vim_org_scripts = [
    ["IndexedSearch", "7062",  "plugin"],
    ["gist",          "15452", "plugin"],
    ["jquery",        "12107", "syntax"],
]

require 'fileutils'
require 'open-uri'

if FileUtils.pwd().split('/').last() == '.vim'
    bundles_dir = File.join(File.dirname(__FILE__), ".vim", "bundle")

    git_bundles.each do |url|
        dir = url.split('/').last.sub(/\.git$/, '')
        module_dir = File.join('bundle', dir)
        if Dir.exists?(module_dir)
            puts "  Updating #{dir} module"
            FileUtils.cd(module_dir)
            `git pull origin master`
            dots = module_dir.split('/').map {|i| '..'}
            dots = dots.join('/')
            FileUtils.cd(dots)
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
