# Function definitions

# function for easy persistency of aliases
add-alias(){
    name=$1
    if [ $name ]; then
        shift
        echo alias $name="\""$@"\"" >> ~/.bash_aliases
        source ~/.bashrc
        echo $name 'has been installed in .bashrc'
    else
        echo "No name provided"
    fi
}

# function to create and load an ssh key and copy the public key to keyboard
generate-ssh-key(){
  ssh-keygen -t rsa -b 4096 -C "$EMAIL" -f ~/.ssh/$1 -q -N ""
  eval `ssh-agent -s`
  ssh-add ~/.ssh/$1
  xclip -selection clipboard < ~/.ssh/$1.pub
}

killnode(){
  lsof -i :8081 | awk 'NR!=1 {print $2}' | xargs kill
}

status(){
  /home/yenda/status
  cd status-react/
}

clash(){
  /home/yenda/clash-mobile/scripts/clash
}

status-emulator(){
  /home/yenda/status-emulator.sh
  cd status-react/
}