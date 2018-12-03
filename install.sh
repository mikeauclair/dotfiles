#!/usr/bin/env zsh
setopt EXTENDED_GLOB
mkdir -p "${HOME}/.emacs.d"
ln -s "${HOME}/.dotfiles/init.el" "${HOME}/.emacs.d/init.el"
git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
ln -s "${HOME}/.dotfiles/zshrc" "${HOME}/.zshrc"
ln -s "${HOME}/.dotfiles/.zprezto/runcoms/zlogin" "${HOME}/.zlogin"
cp "${HOME}/.dotfiles/.zprezto/runcoms/zpreztorc" "${HOME}/.dotfiles/.zpreztorc"
