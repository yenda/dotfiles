#!/bin/sh

stow i3
stow spacemacs
stow shell
stow boot
stow lein
stow ssh
stow git
stow status

mkdir -p .config/systemd/user/
rm ~/.config/systemd/user/emacs.service
cp systemd/emacs.service ~/.config/systemd/user/emacs.service
systemctl --user enable --now emacs
