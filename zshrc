# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="jester"

# Aliases
alias edit="emacsclient -n"

# Plugins
plugins=(git)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
export PATH=$PATH:$HOME/.oh-my-zsh/bin:$HOME/bin
export PATH=$PATH:/usr/local/share/npm/bin
export PATH=$PATH:$HOME/.cask/bin
export PATH=$PATH:/usr/local/anaconda3/bin

# Turn off stupid ZSH auto-correct
unsetopt correct_all

# Bookmarking
export MARKPATH=$HOME/.marks

function jump { 
    cd -P $MARKPATH/$1 2>/dev/null || echo "No such mark: $1"
}
function mark { 
    mkdir -p $MARKPATH; ln -s $(pwd) $MARKPATH/$1
}
function unmark { 
    rm -i $MARKPATH/$1 
}
function marks {
    (t="$(printf "\t")"; cd $MARKPATH && stat -f"%N$t%SY" * | column -ts"$t")
}
function mark_complete {
    local a
    read -cA a
    if [[ -n $a ]]; then
      reply=($(ls $MARKPATH))
    fi
}
compctl -K mark_complete jump
compctl -K mark_complete unmark

# Syntax highlighting
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Z
. `brew --prefix`/etc/profile.d/z.sh

# Go
export GOPATH=$HOME/Code/Public/go

# added by travis gem
[ -f /Users/jim/.travis/travis.sh ] && source /Users/jim/.travis/travis.sh

# tabtab source for packages
# uninstall by removing these lines
[[ -f ~/.config/tabtab/__tabtab.zsh ]] && . ~/.config/tabtab/__tabtab.zsh || true
