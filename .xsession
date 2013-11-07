GIT_ANNEX="/home/dwagner/.local/bin/git-annex"

dropbox start
if [ -f "$GIT_ANNEX" ]; then
    $GIT_ANNEX assistant --autostart
fi
setxkbmap us -option compose:ralt
mpd
xsetroot -solid black
xmonad
mpd --kill  # this will run when xmonad returns
