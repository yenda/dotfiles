ANDROID_HOME=${HOME}/Android/Sdk
PATH=$PATH:/usr/bin:${ANDROID_HOME}/tools:${ANDROID_HOME}/platform-tools

startx

xrandr --output DVI-D-0 --off --output HDMI-1 --mode 1920x1080 --pos 3840x0 --rotate normal --output HDMI-0 --mode 3840x2160 --pos 0x0 --rotate normal --output DP-3 --off --output DP-2 --off --output DP-1 --off --output DP-0 --off
