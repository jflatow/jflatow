# force system-wide profile (for non-interactive logins)
. /etc/profile

export GOROOT=/usr/local/src/lang/Go/go
export GOPATH=/usr/local/src/lang/Go/go-workspace
export PATH=~/.bin:~/.cargo/bin:$PATH:$GOROOT/bin:$GOPATH/bin

export CLICOLOR=cons25
export EDITOR=emacs
export FIGNORE=.svn

PROMPT_COLOR='\[\e[32m\]'
REMOVE_COLOR='\[\e[0m\]'
export PS1="[${PROMPT_COLOR}\u@\h${REMOVE_COLOR} \W]\$ "

export GREP_OPTIONS='--color=auto --exclude-dir=.git --exclude-dir=node_modules'

# only if/when... (revisit me)
. ~/.cargo/env || true
. ~/.nvm/nvm.sh || true

# only if/when google-cloud-sdk (revisit me)
. /usr/local/src/util/google-cloud-sdk/completion.bash.inc || true
. /usr/local/src/util/google-cloud-sdk/path.bash.inc || true
