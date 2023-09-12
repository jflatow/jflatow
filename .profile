# force system-wide profile (for non-interactive logins)
. /etc/profile

export PATH=~/.bin:~/.cargo/bin:~/.deno/bin:$PATH
export CLICOLOR=cons25
export EDITOR=emacs

if [ -n "$BASH_VERSION" ]; then
    PROMPT_COLOR='\[\e[32m\]'
    REMOVE_COLOR='\[\e[0m\]'
    export PS1="[${PROMPT_COLOR}\u@\h${REMOVE_COLOR} \W]\$ "
elif [ -n "$ZSH_VERSION" ]; then
    PROMPT_COLOR='%F{green}'
    REMOVE_COLOR='%f'
    export PROMPT="[%{$PROMPT_COLOR%}%n@%m%{$REMOVE_COLOR%} %~]\$ "
fi
