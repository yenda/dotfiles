ANDROID_HOME=${HOME}/Android/Sdk
PATH=$PATH:/usr/bin:${ANDROID_HOME}/tools:${ANDROID_HOME}/platform-tools

startx

xrandr --output DisplayPort-1 --off --output DisplayPort-0 --off --output DVI-1 --mode 1920x1080 --pos 0x0 --rotate normal --output DVI-0 --off --output HDMI-0 --mode 1920x1080 --pos 1920x0 --rotate normal
