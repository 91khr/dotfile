#!/bin/bash

# Pacman conf
sed -i 's/#Color/Color/' /etc/pacman.conf
cat <<EOF > /etc/pacman.conf
[archlinuxcn]
Server = https://mirrors.tuna.tsinghua.edu.cn/archlinuxcn/$arch
Include = /etc/pacman.d/archlinuxcn-mirrorlist
EOF

# Install essential packages
sed 's/#.*$//' <<EOF | sudo pacman -Syu --noconfirm -
# Fonts
adobe-source-han-sans-cn-fonts
adobe-source-han-serif-cn-fonts
ttf-monaco
otf-latinmodern-math  # For firefox mathml ><
# Develop tools
#base-devel  # Shall be installed if install.cpp is executed()(
# Input method
fcitx5-im fcitx5-rime fcitx5-mozc
# Workflow (shell & tools)
zsh
zsh-syntax-highlighting
firefox
gvim
xterm
# Graphics environment
lightdm
lightdm-gtk-greeter
xorg-server
awesome
xorg-xrdb
# Extra packaging tool
yay
EOF
yay -S --noconfirm - <<EOF
transset-df
EOF

# Unmute alsa
sudo pacman -S alsa-utils
amixer sset Master unmute
amixer sset Speaker unmute
amixer sset Headphone unmute
sudo pacman -Rs alsa-utils

# vim: fdm=marker
