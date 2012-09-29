#!/bin/bash

cd $(dirname "$0")

link() {
    if [ ! -h $HOME/.$1 ]; then
       ln -s "`pwd`/dotfiles/$1" "$HOME/.$1"
    fi
}

echo "init vim ...."
if [ ! -d dotfiles/vim/bundle/vundle ]; then
    git clone https://github.com/gmarik/vundle.git dotfiles/vim/bundle/vundle
fi
link vim
link vimrc
vim +BundleInstall +qall

echo "init git ..."
link gitconfig
link gitignore

echo "init hg ..."
link hgrc
#link hg-prompt.py

echo "init zsh ..."
link zshrc

echo "init python env ..."

if which easy_install > /dev/null; then
    echo "easy_install has already installed"
else
    apt-get install python-setuptools
fi
if which pip > /dev/null; then
    echo "pip has already installed"
else
    sudo easy_install pip
fi
if which virtualenv > /dev/null; then
    echo "virtualenv has already installed"
else
    sudo pip install virtualenv
fi

if [ ! -d $HOME/Workspace ]; then
    echo "create Workspace"
    mkdir "$HOME/Workspace"
fi

if [ ! -d ~/.oh-my-zsh ]; then
    git clone https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
fi

if which zsh > /dev/null; then
    echo "zsh has already installed"
else
    echo "install zsh"
    sudo apt-get install zsh
fi
sudo chsh -s `which zsh`

