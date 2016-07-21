set -o noclobber

shopt -s checkwinsize
shopt -s histappend
shopt -s cmdhist
shopt -s autocd
shopt -s dirspell
shopt -s cdspell
shopt -s cdable_vars

bind "set completion-ignore-case on"
bind "set completion-map-case on"
bind "set show-all-if-ambiguous on"

PROMPT_COMMAND='history -a'
HISTSIZE=500000
HISTFILESIZE=100000
HISTCONTROL="erasedups:ignoreboth"
HISTTIMEFORMAT='%F %T '
CDPATH=".:~:/short/v10/dxr251"

export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"
export LANG='UTF-8'
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LD_LIBRARY_PATH=~/.local/lib:~/.local/lib64
export LM_LICENSE_FILE=7498@sflm0
export PATH=~/bin:$PATH
export PS1='\h$ '
export PYTHONSTARTUP=~/.pythonrc
export TERM=xterm-256color
export EDITOR='vi'
export MODULEPATH=/g/data2/v10/public/modules/modulefiles:/projects/u46/opt/modules/modulefiles:$MODULEPATH
export PYTHONPATH=~/bin:$PYTHONPATH

alias ls='ls -h -1 -c -F -t -r --color=never'
alias vi='vim -u ~/.dotfiles/.virc'
alias q='qstat -1 -n -u dxr251'
alias qq='nci_account -v'
alias qqq='qstat | grep `whoami` | cut -d . -f 1 | xargs qps'
alias jup='jupyter notebook --no-browser --port=9001'
alias c='rm *.e* *.o*'

module refresh

umask 002
