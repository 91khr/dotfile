#!/bin/sh

if which python > /dev/null; then; else
    sudo pacman -S --noconfirm python
fi

if [ $? -eq 0 ]; then
    nowpath=$(dirname $0)
    cd nowpath
    ./install.py $(realpath nowpath)
else
    echo '>< Failed to install python...'
fi

exit

# Constants
dotdir=$(realpath $(dirname $0))

# Install essential packages
sudo pacman -S -noconfirm - <<EOF
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
yay
xorg-xmodmap
EOF
yay -S -noconfirm - <<EOF
transset-df
EOF

# zsh
echo "source ${dotdir}/.zshrc" >> ~/.zshrc

# vim
echo "set rtp+=${dotdir}/.vim
source ${dotdir}/.vim/vimrc" >> ~/.vimrc

# emacs
test -d ~/.emacs.d || mkdir ~/.emacs.d
echo "(load-file \"${dotdir}/.emacs.d/init.el\")" >> ~/.emacs.d/init.el

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
sudo systemctl enable lightdm.service

# Unmute alsa
sudo pacman -S alsa-utils
amixer sset Master unmute
amixer sset Speaker unmute
amixer sset Headphone unmute
sudo pacman -Rs alsa-utils

