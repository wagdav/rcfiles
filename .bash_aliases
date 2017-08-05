alias archive='cd $HOME/Dropbox/archive'
alias do_ledger="cd $HOME/personal/ledger && export LEDGER_FILE=ledger.dat"

alias lq='ls -Q'
alias ll='ls -alh'

# todo-txt
alias t="todo-txt -d $HOME/.todo-txt/config -t"

if [ -f /usr/share/bash-completion/completions/todo-txt ]; then
    complete -F _todo t
    . /usr/share/bash-completion/completions/todo-txt
fi
