#!/usr/bin/env zsh
mkdir -p "${HOME}/.emacs.d"
ln -s "${HOME}/.dotfiles/init.el" "${HOME}/.emacs.d/init.el"
git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
ln -s "${HOME}/.dotfiles/zshrc" "${HOME}/.zshrc"
ln -s "${HOME}/.zprezto/runcoms/zlogin" "${HOME}/.zlogin"
cp "${HOME}/.zprezto/runcoms/zpreztorc" "${HOME}/.zpreztorc"
