# XDG Base Directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_STATE_HOME="$HOME/.local/state"

# Rust
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"

# Go
export GOPATH="$XDG_DATA_HOME/go"
export GOMODCACHE="$XDG_CACHE_HOME/go/mod"

# Python
export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/pythonrc"
export PYTHONPYCACHEPREFIX="$XDG_CACHE_HOME/python"
export VIRTUAL_ENV_HOME="$XDG_DATA_HOME/venvs"   # kendi konvansiyon
export IPYTHONDIR="$XDG_CONFIG_HOME/ipython"
export JUPYTER_CONFIG_DIR="$XDG_CONFIG_HOME/jupyter"

# C / build araçları
export CCACHE_DIR="$XDG_CACHE_HOME/ccache"
export CONAN_USER_HOME="$XDG_DATA_HOME"           # conan kullanıyorsan
export CMAKE_USER_PRESET_FILE="$XDG_CONFIG_HOME/cmake/presets.json"

# GPG
export GNUPGHOME="$XDG_DATA_HOME/gnupg"

# Docker
export DOCKER_CONFIG="$XDG_CONFIG_HOME/docker"

# wget
export WGETRC="$XDG_CONFIG_HOME/wget/wgetrc"

# less
export LESSHISTFILE="$XDG_STATE_HOME/less/history"
export LESSKEY="$XDG_CONFIG_HOME/less/lesskey"

# npm / node
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export NODE_REPL_HISTORY="$XDG_STATE_HOME/node/repl_history"

# GDB
export GDBHISTFILE="$XDG_STATE_HOME/gdb/history"

# Gerekli dizinleri oluştur
mkdir -p \
  "$XDG_CONFIG_HOME" \
  "$XDG_DATA_HOME" \
  "$XDG_CACHE_HOME" \
  "$XDG_STATE_HOME" \
  "$XDG_STATE_HOME/less" \
  "$XDG_STATE_HOME/node" \
  "$XDG_STATE_HOME/gdb" \
  "$XDG_CACHE_HOME/python" \
  "$XDG_CACHE_HOME/go" \
  "$XDG_CONFIG_HOME/python" \
  "$XDG_CONFIG_HOME/npm" \
  "$XDG_CONFIG_HOME/wget" \
  "$XDG_CONFIG_HOME/less"
. "/home/levent/.local/share/cargo/env"
