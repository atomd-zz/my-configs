#!/bin/bash

cd $(dirname "$0")

link() {
    if [ ! -h $HOME/.$1 ]; then
       ln -s "`pwd`/dotfiles/$1" "$HOME/.$1"
    fi
}

link2fdr() {
    if [ ! -h $HOME/.$2/$1 ]; then
       mkdir -p "$HOME/.$2"
       ln -s "`pwd`/dotfiles/$1" "$HOME/.$2/$1"
    fi
}

echo "init vim ...."
if [ ! -d dotfiles/vim/bundle/vundle ]; then
    git clone https://github.com/gmarik/vundle.git dotfiles/vim/bundle/vundle
fi
sudo apt-get install exuberant-ctags
link vim
link vimrc
vim +BundleInstall +qall

echo "init emacs ...."
link emacs.d

# echo "git submodule update --init for jedi-vim"
# JEDI="dotfiles/vim/bundle/jedi-vim"
# if [ ! -d $JEDI ]; then
#     git --git-dir=$JEDI submodule update --init
# fi

echo "init pypy config"
link pydistutils.cfg
link2fdr pip.conf pip

echo "init git ..."
IGN="$HOME/.templates.gitignore"
if [ ! -d $IGN ]; then
    git clone https://github.com/github/gitignore.git $IGN
fi

#link gitignore
cat "$IGN/Global/Archives.gitignore"    \
    "$IGN/Global/CVS.gitignore"         \
    "$IGN/Global/Emacs.gitignore"       \
    "$IGN/Global/Linux.gitignore"       \
    "$IGN/Global/Mercurial.gitignore"   \
    "$IGN/Global/Tags.gitignore"        \
    "$IGN/Global/vim.gitignore"         \
    "$IGN/Autotools.gitignore"          \
    dotfiles/gitignore                  \
    > "$HOME/.gitignore"

link gitattributes
link gitconfig

echo "init hg ..."
link hgrc
#link hg-prompt.py

echo "init zsh ..."
link zshrc

# echo "init xmodmap ..."
# link Xmodmap

echo "init bash_aliases ..."
link bash_aliases

echo "init python env ..."
if which easy_install > /dev/null; then
    echo "easy_install has already installed"
else
    apt-get install python-setuptools
fi

if which pip > /dev/null; then
    echo "pip has already installed"
else
    sudo apt-get install python-pip python-dev build-essential
    sudo pip install --upgrade pip
fi

if which virtualenv > /dev/null; then
    echo "virtualenv has already installed"
    sudo pip install --upgrade virtualenv
else
    sudo pip install virtualenv
fi

if [ ! -d ~/.oh-my-zsh ]; then
    git clone https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
fi

link tmux.conf
if which tmux > /dev/null; then
    echo "tmux has already installed"
else
    sudo apt-get install tmux
fi

if which zsh > /dev/null; then
    echo "zsh has already installed"
else
    echo "install zsh"
    sudo apt-get install zsh
fi

if ! echo $SHELL | grep -q zsh; then
    echo 'You need: chsh -s `which zsh`'
fi
