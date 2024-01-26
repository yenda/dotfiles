#!/bin/sh

stow i3
rm .bashrc
rm .bash_logout
stow shell
stow git
stow emacs

