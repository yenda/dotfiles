#!/bin/sh

stow i3
stow shell
stow git
stow emacs
stow gnupg
gpgconf --reload gpg-agent
