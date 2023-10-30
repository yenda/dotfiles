# Alias definitions.

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

alias src="source ~/.bashrc"
alias ll="ls -lha"
alias mk="mkdir "
alias dow="cd ~/Downloads; ll"
alias bsrc=". ~/.bashrc"

# git related aliases
alias gs="git status"
alias hist='git log --decorate --pretty=format:"%C(bold blue)%h%C(reset) %C(white)%s%C(reset) %C(dim white) - %an%C(reset) %C(green) - (%ar)%C(reset)" --graph --all'
alias gac="git add .; git commit -m $1"
alias gp="git push -u origin master"

# add alias to functions defined in .bash_functions
alias al="add-alias"
alias sshk="generate-ssh-key"

# past that comment aliases are appended with the add-alias function, only write manually above this line
alias dot="cd ~/dotfiles; git status; echo ''; ll"
alias e="emacsclient -c"
alias rmf="rm -rf"
alias ec="emacs -nw ~/.emacs"

alias rebase="git pull --rebase origin develop"