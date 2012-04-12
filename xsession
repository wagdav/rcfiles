# .xsession
xscreensaver -no-splash &
dropbox start
setxkbmap us
$HOME/rcfiles/set_screen dual

#trayer --edge top --align left --SetDockType true --SetPartialStrut true \
#    --expand true --width 2 --height 12 --transparent true --tint 0x000000 &
x-terminal-emulator &
xmonad
