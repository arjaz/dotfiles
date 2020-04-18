export ZSH=/home/arjaz/.oh-my-zsh

ZSH_THEME="typewritten"

export UPDATE_ZSH_DAYS=13
plugins=(
  git
  zsh-autosuggestions
)

source $ZSH/oh-my-zsh.sh

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='nvim'
fi

alias vim=nvim
alias v=nvim
alias view='nvim -R'
alias :q=exit
alias p=python
alias btw=neofetch

test -r "~/.dir_colors" && eval $(dircolors ~/.dir_colors)

export PATH="$HIME/.cabal/bin:$PATH"
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
