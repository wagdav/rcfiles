# .xsession
xscreensaver -no-splash &
dropbox start
setxkbmap us
$HOME/rcfiles/set_screen dual
trayer --edge top \
       --width 4 --widthtype percent \
       --height 15 --heighttype pixel \
       --transparent true --alpha 0 --tint 0x000000 \
       --align left \
&

xmonad
