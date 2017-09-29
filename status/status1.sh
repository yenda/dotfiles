#/bin/bash

cd status-react
rm -rf node_modules
rm package-lock.json
lein clean
lein deps && npm install && ./re-natal deps  && ./re-natal use-figwheel && lein re-frisk use-re-natal && ./re-natal enable-source-maps
./re-natal use-android-device avd && ./re-natal use-figwheel && lein re-frisk use-re-natal

gnome-terminal -e "bash -c 'BUILD_IDS=\"android\" lein repl'"
