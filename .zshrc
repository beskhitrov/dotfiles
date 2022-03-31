EXA=`which exa`
NVIM=`which nvim`
TIDY=`which tidy`

# Prompt: %n:%~ %# _
PROMPT='%F{yellow}%m%f:%F{green}%1~%f %# '

TMOUT=1
TRAPALRM() { zle reset-prompt }

precmd() { print "" }

export LC_MESSAGES=en_US

# vi-mode
bindkey -v
export KEYTIMEOUT=1

# Tab-completion
autoload -Uz compinit && compinit -u

# No beep
setopt NO_BEEP

# Color
#export CLICOLOR=1
export TERM=xterm-256color

# Aliases
alias egrep='egrep --color=auto'
alias vi=$NVIM
alias ll='$EXA -l --all --group'
alias cat='/usr/bin/batcat'
alias tm='$TIDY --markup yes'

# Suffix aliases
alias -s md='view'

# Key bindings
bindkey '^[[A' up-line-or-search
bindkey '^k' up-line-or-search
bindkey '^[[B' down-line-or-search

# Navigation
setopt AUTO_CD

# Correction
setopt CORRECT
setopt CORRECT_ALL

# History
setopt HIST_EXPIRE_DUPS_FIRST 
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_REDUCE_BLANKS
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY

# Globbing
setopt NO_CASE_GLOB
setopt EXTENDED_GLOB

# autosuggestions
#source /usr/local/share/zsh-autosuggestions/zsh-autosuggestions.zsh

#
#source /usr/local/etc/profile.d/z.sh

#autoload -U compinit
#zstyle ':completion:*' menu select
#zmodload zsh/complist
#compinit
#_comp_options+=(globdots)

#if type brew &>/dev/null; then
#	FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
#	FPATH="$(brew --prefix)/share/zsh-completions:${FPATH}"

autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
RPROMPT='${vcs_info_msg_0_} %8F%*%f %n'
zstyle ':vcs_info:git:*' formats '%8F%s:%b%f'

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

eval "$(ssh-agent -s)"
`which ssh-add`

# https://github.com/rupa/z
. ~/bin/z.sh
