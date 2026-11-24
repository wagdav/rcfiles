if [ -f /etc/profile ]; then
    . /etc/profile
fi

export PATH=$HOME/.local/bin:$PATH

# Nix (must come before oh-my-zsh so plugins see nix-installed tools)
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi

if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
    . "$HOME"/.nix-profile/etc/profile.d/nix.sh
fi

# Nix profile completions (must be on fpath before compinit / oh-my-zsh)
if [ -d "$HOME/.nix-profile/share/zsh/site-functions" ]; then
    fpath=("$HOME/.nix-profile/share/zsh/site-functions" $fpath)
fi

if [ -d "$HOME/.oh-my-zsh" ]; then
    ZSH_THEME="robbyrussell"

    plugins=(
      autojump
      aws
      dirhistory
      fzf
      gh
      git
      gitfast
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

if [ -f "$HOME/ZscalerRootCertificate-2048-SHA256-Feb2025.crt" ]; then
    export AWS_CA_BUNDLE=~/ZscalerRootCertificate-2048-SHA256-Feb2025.crt
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
alias vi=nvim

LESS=FRX  # https://github.com/cli/cli/discussions/3685
