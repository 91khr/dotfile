#!/bin/sh
# A tiny util to deploy the dotfiles into a computer

# Constants
dotdir=$(realpath $(dirname $0))

# Install essential packages
pacman -S - <<EOF
wqy-zenhei
ttf-monaco
base-devel
zsh
zsh-syntax-highlighting
fcitx
fcitx-configtool
fcitx-im
fcitx-rime
firefox
gvim
lightdm
lightdm-gtk-greeter
xterm
netease-cloud-music
xorg-server
awesome
xorg-xrdb
EOF

# zsh
echo "source ${dotdir}/.zshrc" >> ~/.zshrc

# vim
echo "set rtp+=${dotdir}/.vim
source ${dotdir}/.vimrc" >> ~/.vimrc

# awesome
echo "os = require('os');
loadfile('${dotdir}/awesome/rc.lua')('${dotdir}')" >> ~/.config/awesome/rc.lua

# (fcitx
cat <<EOF >> ~/.xprofile
export XMODIFIERS="@im=fcitx"
export QT_IM_MODULE=<module>
export GTK_IM_MODULE=<module>
EOF

# xterm
echo "
#include \"${dotdir}/.Xresources\"
#include \"${dotdir}/Solarizedxterm/.Xdefaults\"" >> ~/.Xresources

# Enable lightdm
systemctl enable lightdm.service

# Unmute alsa
pacman -S alsa-utils
amixer sset Master unmute
amixer sset Speaker unmute
amixer sset Headphone unmute
pacman -Rs alsa-utils

