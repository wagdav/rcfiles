xinput set-prop 'DELL08AF:00 06CB:76AF Touchpad' 'libinput Tapping Enabled' 1

if [ -f "$HOME/.profile" ]; then
    source "$HOME/.profile"
fi

# disable PC speaker beep
xset -b

dropbox start
setxkbmap us -option compose:ralt
mpd
xsetroot -solid black

ssh-agent -t 1d xmonad

mpd --kill  # this will run when xmonad returns
