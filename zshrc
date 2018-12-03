ZDOTDIR="$HOME/.dotfiles"
DISABLE_AUTO_UPDATE="true"
DISABLE_LS_COLORS="true"

export PATH="/usr/local/bin:$HOME/bin:$PATH"

if [ -f "$HOME/.dotfiles/.zprezto/init.zsh" ]; then
    source "$HOME/.dotfiles/.zprezto/init.zsh"
fi

c() { cd ~/code/$1; }
_c() { _files -W ~/code -/; }
compdef _c c

cdh() { cd ~/code/devoted/$1; }
_cdh() { _files -W ~/code/devoted -/; }
compdef _cdh cdh

alias ga='git add'
alias gg='git log --graph'
alias gs='git status'
alias gd='git diff'
alias gc='git commit -v'
alias gco='git checkout'
alias gp='git push'

function abspath() {
    # generate absolute path from relative path
    # $1     : relative filename
    # return : absolute path
    # From http://stackoverflow.com/a/23002317/514210
    if [[ -d "$1" ]]; then
        # dir
        (cd "$1"; pwd)
    elif [[ -f "$1" ]]; then
        # file
        if [[ $1 == */* ]]; then
            echo "$(cd "${1%/*}"; pwd)/${1##*/}"
        else
            echo "$(pwd)/$1"
        fi
    fi
}

function xemacs {
  emacs $(abspath $1) </dev/null &>/dev/null &
}

alias emacs="emacs -nw"
export EDITOR='emacs '
alias diffmerge='/Applications/DiffMerge.app/Contents/Resources/diffmerge.sh'
alias reload=". ~/.zshrc && echo 'ZSH config reloaded from ~/.zshrc'"
# add plugin's bin directory to path
export GOPATH="$HOME/code/golang"

