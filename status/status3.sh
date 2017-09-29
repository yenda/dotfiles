#/bin/bash

cd status-react
adb reverse tcp:8081 tcp:8081
adb reverse tcp:3449 tcp:3449
adb reverse tcp:4567 tcp:4567
react-native run-android
