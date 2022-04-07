# 
# ~/.zshrc
#

[[ ! -v SSH_AUTH_SOCK ]] && eval $(ssh-agent) && ssh-add

if [[ $OSTYPE =~ "linux-gnu" ]]; then
  source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh
  source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
;fi

if [[ $OSTYPE =~ "darwin" ]]; then
  source /usr/local/share/zsh-autosuggestions/zsh-autosuggestions.zsh
  source /usr/local/etc/profile.d/z.sh

  if type brew &>/dev/null; then
    FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
    FPATH="$(brew --prefix)/share/zsh-completions:${FPATH}"
  ;fi
;fi

zstyle ":completion:*" matcher-list "m:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=* r:|=*"

autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
RPROMPT="${vcs_info_msg_0_} %8F%*%f %(!.%F{red}%n%f.%n)"
zstyle ":vcs_info:git:*" formats "%8F%s:%b%f"

PROMPT="%F{yellow}%m%f:%F{green}%1~%f %# "
TMOUT=1; TRAPALRM() { zle reset-prompt }
precmd() { print "" }

autoload -Uz compinit && compinit -u

setopt APPEND_HISTORY
setopt AUTO_CD
setopt CORRECT
setopt CORRECT_ALL
setopt EXTENDED_GLOB
setopt EXTENDED_HISTORY
setopt GLOB_DOTS
setopt HIST_EXPIRE_DUPS_FIRST 
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_REDUCE_BLANKS
setopt INC_APPEND_HISTORY
setopt NO_BEEP
setopt NO_CASE_GLOB
setopt SHARE_HISTORY

bindkey "^[OA" history-search-backward
bindkey "^[OB" history-search-forward

alias ll="exa -l --all --group"

for suffix in "html" "css" "js" "json";
  do alias -s $suffix="emacs"
;done && unset suffix

export LC_MESSAGES="en_US.UTF-8"
export TERM="xterm-256color"

HISTFILE="$HOME/.zsh_history"
SAVEHIST=5000
HISTSIZE=2000

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
