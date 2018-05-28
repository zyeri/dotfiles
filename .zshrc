# ~/.zshrc

source $HOME/.zplug/init.zsh

zplug themes/mh, from:oh-my-zsh, as:theme
zplug plugins/git, from:oh-my-zsh

zplug zsh-users/zsh-completions
zplug zsh-users/zsh-history-substring-search
zplug zsh-users/zsh-syntax-highlighting, defer:2

zplug load

ttyctl -f

setopt PROMPT_SUBST
setopt auto_pushd
setopt pushd_ignore_dups
setopt long_list_jobs
setopt interactivecomments
setopt extended_glob
setopt equals
setopt complete_aliases
setopt multios
setopt autocd

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history

# completion
zmodload -i zsh/complist

setopt auto_menu
setopt complete_in_word
setopt always_to_end

zstyle ":completion:*" menu select=2
bindkey -M menuselect '^O' accept-and-infer-next-history
zstyle ':completion:*:*:*:*:*' menu select

zstyle ':completion:*' rehash true
zstyle ':completion:*' use-cache true
zstyle ':completion:*' insert-tab pending

# fuzzy completion
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*approximate:*' max-errors 1 numeric

# use a cache to speed things up
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path ~/.zsh/cache

zstyle ':completion:*:default' group-name ''
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true
zstyle ':completion:*' menu select
zstyle ':completion:*' verbose yes

zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX + $#SUFFIX) / 3 )) )'
zstyle ':completion::approximate*:*' prefix-needed false

zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always

# bindings
bindkey -v

# C-x C-e to edit current line in $EDITOR
autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

bindkey -M viins '^W' backward-kill-word
bindkey -M viins '^H' backward-delete-char

bindkey '^P' history-substring-search-up
bindkey '^N' history-substring-search-down

bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

bindkey -M vicmd '/' history-incremental-pattern-search-backward
bindkey -M vicmd '?' history-incremental-pattern-search-forward

# aliases

# always sort directories first
alias ls='ls --color=auto --group-directories-first'
alias tree='tree --dirsfirst'
alias diff='diff --color=always'

# short forms with useful flags
alias l='ls -h -S -I ".git"'
alias t='tree -I ".git"'

# ps with cgroups
alias psc='ps xawf -eo pid,user,cgroup,args'

# misc

# commands/functions
ranger-cd () {
    tempfile="$(mktemp -t tmp.XXXXXX)"
    /usr/bin/ranger --choosedir="$tempfile" "${@:-$(pwd)}"
    test -f "$tempfile" &&
    if [ "$(cat -- "$tempfile")" != "$(echo -n `pwd`)" ]; then
        cd -- "$(cat "$tempfile")"
    fi
    rm -f -- "$tempfile"
}

fkill () {
  pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')

  if [ "x$pid" != "x" ]
  then
    kill -${1:-9} $pid
  fi
}

fuck () {
    TF_PREVIOUS=$(fc -ln 1 | tail -n 1);
    TF_CMD=$(
        TF_ALIAS=fuck
        TF_SHELL_ALIAS=$(alias)
        PYTHONIOENCODING=utf-8
        thefuck $TF_PREVIOUS THEFUCK_ARGUMENT_PLACEHOLDER $*
    ) && eval $TF_CMD
    test -n "$TF_CMD" && print -s $TF_CMD
}


# termite ctrl+shift+t support
if [[ $TERM == xterm-termite ]] ; then
    . /etc/profile.d/vte.sh
    __vte_osc7
fi

export NPM_PACKAGES="${HOME}/.npm-packages"
export GOPATH="${HOME}/src/go"
typeset -U path

path=(
    $HOME/bin
    $HOME/.zplug/bin
    $HOME/.cargo/bin
    $NPM_PACKAGES/bin
    $path[@]
)

fpath=(
    $HOME/.zsh/zfunc
    $fpath
)

# pager
export PAGER=less
export LESS='-R -f -X -i -P ?f%f:(stdin). ?lb%lb?L/%L.. [?eEOF:?pb%pb\%..]'
export LESSCHARSET='utf-8'
export MANPAGER="/usr/bin/nvim -c 'set ft=man' -"

# history
export HISTFILE=~/.zsh_history
export HISTSIZE=10000
export SAVEHIST=100000
export LISTMAX=100

export DIRSTACKSIZE=20

# disable history in root
if [[ $UID == 0 ]] ; then
    unset HISTFILE
    export SAVEHIST=0
fi

# system editor
export EDITOR=/usr/bin/nvim

# fix tmux lag
export KEYTIMEOUT=1

unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
    export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
fi

# Set GPG TTY
export GPG_TTY=$(tty)

# Refresh gpg-agent tty in case user switches into an X session
gpg-connect-agent updatestartuptty /bye >/dev/null

# fzf
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh

export RANGER_LOAD_DEFAULT_RC=false

BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

