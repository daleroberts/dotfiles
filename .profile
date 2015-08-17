export LANG='UTF-8'
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LD_LIBRARY_PATH=~/.local/lib:~/.local/lib64
export LM_LICENSE_FILE=7498@sflm0
#export MANPAGER="col -b | vim -c 'set ft=man ts=8 nomod nolist nonu noma' -c 'noremap q ZQ' -"
export PATH=~/.local/bin:$PATH
export PS1='\h$ '
export PYTHONSTARTUP=~/.pythonrc
export TERM=xterm-256color
export EDITOR='vi'

alias ls='ls -c -F --color=never'
alias vi='vim -u ~/.dotfiles/.virc'

check_status() {
if [ -n "$TMUX" ]; then     
	local repo=$(git rev-parse --show-toplevel 2> /dev/null)
	if [[ -e "$repo" ]]; then
		BRANCH=$(git branch --no-color | sed -e '/^[^*]/d' -e 's/\(\*\)\s*\(\w.*\)/\2\1/g')
		tmux set-window-option status-right "${BRANCH} %H:%M:%S " > /dev/null
	else
		tmux set-window-option status-right "%H:%M:%S " > /dev/null
	fi
fi
}

PROMPT_COMMAND="check_status; $PROMPT_COMMAND"

if [ "`type -t module`" = 'function' ]; then
	module load python/2.7.6
	module load git/2.1.0
	module load tmux
fi

umask 002
