# force system-wide profile (for non-interactive logins)
. /etc/profile

export PATH=~/.bin:$PATH

export CLICOLOR=cons25
export EDITOR=emacs
export FIGNORE=.svn

PROMPT_COLOR='\[\e[32m\]'
REMOVE_COLOR='\[\e[0m\]'
export PS1="[${PROMPT_COLOR}\u@\h${REMOVE_COLOR} \W]\$ "