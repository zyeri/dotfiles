#!/usr/bin/env zsh

export RANGER_LOAD_DEFAULT_RC=false

export PATH="$PATH:/home/alex/bin"

export EDITOR=nvim

export LANG=en_US.UTF-8

export RC_ALL=en_US.UTF-8

# export KEYTIMEOUT=1

# export TERM=xterm-256color

export RANGER_LOAD_DEFAULT_RC=false

export GDK_SCALE=1

export QT_SCALE_FACTOR=1

export BROWSER="chromium"

export NVIM_LISTEN_ADDRESS=/tmp/nvim.socket


# if [[ $TERM == xterm-termite ]]; then
#     . /etc/profile.d/vte.sh
#     __vte_osc7
# fi


export FZF_DEFAULT_COMMAND='ag -g ""'
export FZF_DEFAULT_OPTS="--color=dark --inline-info --multi --ansi"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export _JAVA_AWT_WM_NONREPARENTING=1
