if [ -f /etc/profile ]; then
    . /etc/profile
fi

export PATH=$HOME/.local/bin:$PATH

if [ -d "$HOME/.oh-my-zsh" ]; then
    ZSH_THEME="robbyrussell"

    plugins=(
      autojump
      aws
      dirhistory
      fzf
      gh
      git
      gradle
      kubectl
      pass
      pyenv
      terraform
    )

    export ZSH=$HOME/.oh-my-zsh
    source $ZSH/oh-my-zsh.sh
fi

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export EDITOR='nvim'

alias t="todo-txt -d $HOME/.todo-txt/config -t"
alias fd=fdfind

# AWS CLI
if [ -f "$HOME/.nix-profile/bin/aws_completer" ]; then
    autoload bashcompinit && bashcompinit
    autoload -Uz compinit && compinit
    complete -C "$HOME/.nix-profile/bin/aws_completer" aws
fi

# AWS-Vault
if [ -f "$HOME/.local/bin/aws-vault" ]; then
    export AWS_VAULT_BACKEND=pass
    export AWS_VAULT_PASS_PREFIX=aws-vault
    export AWS_ASSUME_ROLE_TTL=1h
    export AWS_SESSION_TTL=8h
    eval "$($HOME/.local/bin/aws-vault --completion-script-zsh)"
fi

# Yarn
if [ -f "$HOME/.yarn/bin/yarn" ]; then
    export PATH="$HOME/.yarn/bin":$PATH
fi

# Go
if [ -f "$HOME/.go/bin/go" ]; then
  export PATH=$PATH:"$HOME/.go/bin"

  export GOPATH="$HOME/go"
  export PATH=$PATH:"$GOPATH/bin"
fi

# Ruby
if [ -f "$HOME/.rbenv/bin/rbenv" ]; then
  export PATH=$PATH:"$HOME/.rbenv/bin"
  eval "$(rbenv init -)"
fi

# Pyenv
if [ -f "$HOME/.pyenv/bin/pyenv" ]; then
  export PATH=$PATH:"$HOME/.pyenv/bin"
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi

# Pipenv shell completion
if [ -x "$(command -v pipenv)" ]; then
    eval "$(pipenv --completion)"
fi

if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi

if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
    . "$HOME"/.nix-profile/etc/profile.d/nix.sh
fi

if [ -d "$HOME/Library/Python/3.10/bin" ]; then
    export PATH="$HOME/Library/Python/3.10/bin":$PATH
fi

unalias fd

export JAVA_TOOL_OPTIONS="-Djavax.net.ssl.trustStore=/Users/david.wagner/.gradle/castore-nexthink -Djavax.net.ssl.trustStorePassword=changeit -Djavax.net.ssl.keyStore=/Users/david.wagner/.gradle/castore-nexthink -Djavax.net.ssl.keyStorePassword=changeit"
