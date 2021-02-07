# todo-txt
alias t="todo-txt -d $HOME/.todo-txt/config -t"

if [ -f /usr/share/bash-completion/completions/todo-txt ]; then
    complete -F _todo t
    . /usr/share/bash-completion/completions/todo-txt
fi
