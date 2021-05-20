#!/bin/sh
#         __         __ __             
# .--.--.|__|.-----.|__|  |_.----.----.
# |_   _||  ||     ||  |   _|   _|  __|
# |__.__||__||__|__||__|____|__| |____|
#                                     
# Emilia's xinitrc <3
# Edited: 2021-05-19
# Author: Emilia Dunfelt, edun@dunfelt.se
# 

###########################################
# X-stuff
###########################################
xrdb ~/.Xresources
xsetroot -cursor_name left_ptr &

############################################
# Applications
############################################
feh --bg-fill ~/.config/walls/city.jpg

# automounter
udiskie &

# screensaver
xscreensaver -no-splash &

# battery-stuff
tlp start &
batsignal -b -W "Did you bring my charger? I'm at 15%." -C "Hey, I need charging ASAP! I'm at 5%" -D "....2%...." -F "I'm all good, you can unplug me. I'm at 100%."

# notifications
dunst &

# flameshot (apparently this is needed now :/)
flameshot &

# bluetooth
bluetoothctl power on

picom &

# start wm
exec xmonad
