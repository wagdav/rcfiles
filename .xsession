if [ -f "$HOME/.profile" ]; then
    source "$HOME/.profile"
fi

GIT_ANNEX="/home/dwagner/.local/bin/git-annex"

# disable PC speaker beep
xset -b

dropbox start
if [ -f "$GIT_ANNEX" ]; then
    $GIT_ANNEX assistant --autostart
fi
setxkbmap us -option compose:ralt
mpd
xsetroot -solid black

eval `ssh-agent --daemon`

xmonad
mpd --kill  # this will run when xmonad returns
