function bing() { start "https://bing.com/search?q=${*// /+}" }

if [ $WSL_DISTRO_NAME ]; then
    function start() {
        cmd.exe /c "(start $*)"
        return $?
    }
else
    function start() {
        xdg-open $*
        return $?
    }
fi
compdef _files start
