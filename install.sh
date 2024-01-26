#!/bin/sh

stow i3
stow config
rm .bashrc
rm .bash_logout
stow shell
stow git
stow emacs

