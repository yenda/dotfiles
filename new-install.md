sudo apt-get install --yes git chromium stow rofi i3 silversearcher-ag libc6:i386 libncurses5:i386 libstdc++6:i386 lib32z1 libbz2-1.0:i386 libgtk2.0-dev libc6-dev  libncurses5-dev libpng-dev libtiff5-dev xaw3dg-dev zlib1g-dev libice-dev libsm-dev libx11-dev libxext-dev libxi-dev libxmu-dev libxmuu-dev libxpm-dev libxrandr-dev libxt-dev libxtst-dev libxv-dev libjpeg-dev libjpeg8-dev libjpeg-turbo8-dev libgif-dev libgnutls28-dev 

sudo apt-get install qemu qemu-kvm libvirt-daemon bridge-utils virt-manager virtinst ia32-libs-multiarch 

sudo apt-get install xclip libgtk2.0-dev

# install dotfiles

```
rm bashrc .bash_logout
cd dotfiles
./install.sh
```

# generate ssh key and add ssh key to github in chromium
generate-ssh-key github

Add font source-code-pro

[ -d /usr/share/fonts/opentype ] || sudo mkdir /usr/share/fonts/opentype
sudo git clone --depth 1 --branch release https://github.com/adobe-fonts/source-code-pro.git /usr/share/fonts/opentype/scp
sudo fc-cache -f -v

Build and install emacs

http://ftp.gnu.org/gnu/emacs/

tar -xfv emacs-*
./autogen.sh    # not needed when installing from tarball
./configure -with-x-toolkit=gtk
make
sudo make install

# Install docker

sudo apt-get install -y apt-transport-https ca-certificates curl software-properties-common

curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -

export LSB_ETC_LSB_RELEASE=/etc/upstream-release/lsb-release

V=$(lsb_release -cs)

sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu focal stable"

sudo apt-get update -y

sudo apt-get install -y docker-ce

## Add user to docker group. Added user can run docker command without sudo command
sudo gpasswd -a "${USER}" docker
;;sudo usermod -a -G docker $USER

sudo curl -L "https://github.com/docker/compose/releases/download/1.24.0/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose

sudo chmod +x /usr/local/bin/docker-compose

docker --version
docker-compose --version

## test
docker run hello-world

# Install android studio

https://developer.android.com/studio/intro

sudo adduser $USER kvm

# Install expressvpn
https://www.expressvpn.com/subscriptions

# Install watchman

git clone https://github.com/facebook/watchman.git
cd watchman
git checkout v4.9.0  # the latest stable release
./autogen.sh
./configure
make
sudo make install

echo 999999 | sudo tee -a /proc/sys/fs/inotify/max_user_watches && echo 999999 | sudo tee -a /proc/sys/fs/inotify/max_queued_events && echo 999999 | sudo tee -a /proc/sys/fs/inotify/max_user_instances && watchman shutdown-server && sudo sysctl -p

