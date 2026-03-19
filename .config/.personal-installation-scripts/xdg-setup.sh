#!/usr/bin/env bash
# =============================================================================
# xdg-setup.sh — Pro-level XDG Base Directory setup for Fedora i3 + Zsh
# Run once as your regular user (NOT root)
# =============================================================================

set -euo pipefail

RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; CYAN='\033[0;36m'; NC='\033[0m'
info()    { echo -e "${CYAN}[INFO]${NC}  $*"; }
ok()      { echo -e "${GREEN}[ OK ]${NC}  $*"; }
warn()    { echo -e "${YELLOW}[WARN]${NC}  $*"; }
die()     { echo -e "${RED}[FAIL]${NC}  $*" >&2; exit 1; }
section() { echo -e "\n${CYAN}══════════════════════════════════════${NC}"; \
            echo -e "${CYAN}  $*${NC}"; \
            echo -e "${CYAN}══════════════════════════════════════${NC}"; }

[[ "$EUID" -eq 0 ]] && die "Do not run as root. Run as your regular user."
command -v zsh &>/dev/null || die "zsh not found. Install it first: sudo dnf install -y zsh"

# =============================================================================
# 0. Resolve XDG variables
# =============================================================================
XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"
XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"

ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# =============================================================================
# 1. Create directory tree
# =============================================================================
section "1. Creating XDG directory tree"

dirs=(
    "$XDG_CONFIG_HOME"
    "$XDG_DATA_HOME"
    "$XDG_STATE_HOME"
    "$XDG_CACHE_HOME"

    # zsh dotdir and state
    "$ZDOTDIR"
    "$XDG_STATE_HOME/zsh"
    "$XDG_CACHE_HOME/zsh"

    # other shell state
    "$XDG_STATE_HOME/less"
    "$XDG_STATE_HOME/node"
    "$XDG_STATE_HOME/python"
    "$XDG_STATE_HOME/wget"

    # cache buckets
    "$XDG_CACHE_HOME/pip"
    "$XDG_CACHE_HOME/npm"
    "$XDG_CACHE_HOME/go/mod"
    "$XDG_CACHE_HOME/gem"
    "$XDG_CACHE_HOME/python"
    "$XDG_CACHE_HOME/fontconfig"
    "$XDG_CACHE_HOME/maven"
    "$XDG_CACHE_HOME/gradle"

    # data homes
    "$XDG_DATA_HOME/cargo"
    "$XDG_DATA_HOME/rustup"
    "$XDG_DATA_HOME/go"
    "$XDG_DATA_HOME/gem"
    "$XDG_DATA_HOME/gnupg"
    "$XDG_DATA_HOME/pipx"
    "$XDG_DATA_HOME/wine"
    "$XDG_DATA_HOME/pass"
    "$XDG_DATA_HOME/fonts"
    "$XDG_DATA_HOME/icons"
    "$XDG_DATA_HOME/applications"
    "$XDG_DATA_HOME/gradle"

    # config dirs for common tools
    "$XDG_CONFIG_HOME/i3"
    "$XDG_CONFIG_HOME/i3status"
    "$XDG_CONFIG_HOME/i3blocks"
    "$XDG_CONFIG_HOME/gtk-3.0"
    "$XDG_CONFIG_HOME/gtk-4.0"
    "$XDG_CONFIG_HOME/nvim"
    "$XDG_CONFIG_HOME/vim"
    "$XDG_CONFIG_HOME/npm"
    "$XDG_CONFIG_HOME/wget"
    "$XDG_CONFIG_HOME/readline"
    "$XDG_CONFIG_HOME/docker"
    "$XDG_CONFIG_HOME/aws"
    "$XDG_CONFIG_HOME/ansible"
    "$XDG_CONFIG_HOME/python"
    "$XDG_CONFIG_HOME/rofi"
    "$XDG_CONFIG_HOME/dunst"
    "$XDG_CONFIG_HOME/picom"
    "$XDG_CONFIG_HOME/alacritty"
    "$XDG_CONFIG_HOME/kitty"
    "$XDG_CONFIG_HOME/foot"
    "$XDG_CONFIG_HOME/polybar"
)

for d in "${dirs[@]}"; do
    mkdir -p "$d"
done
ok "Directory tree created"

chmod 700 "$XDG_DATA_HOME/gnupg"
ok "gnupg dir permissions set to 700"

# =============================================================================
# 2. XDG user dirs
# =============================================================================
section "2. XDG user dirs"

cat > "$XDG_CONFIG_HOME/user-dirs.dirs" << 'EOF'
XDG_DESKTOP_DIR="$HOME/desktop"
XDG_DOWNLOAD_DIR="$HOME/downloads"
XDG_TEMPLATES_DIR="$HOME/templates"
XDG_PUBLICSHARE_DIR="$HOME/public"
XDG_DOCUMENTS_DIR="$HOME/documents"
XDG_MUSIC_DIR="$HOME/music"
XDG_PICTURES_DIR="$HOME/pictures"
XDG_VIDEOS_DIR="$HOME/videos"
EOF

for ud in desktop downloads templates public documents music pictures videos; do
    mkdir -p "$HOME/$ud"
done

command -v xdg-user-dirs-update &>/dev/null && xdg-user-dirs-update
ok "XDG user dirs configured"

# =============================================================================
# 3. ZDOTDIR bootstrap — the critical piece for zsh
#
#    Zsh reads ~/.zshenv before ZDOTDIR is known, so we put ONE line there:
#    export ZDOTDIR=~/.config/zsh
#    Everything else lives under $ZDOTDIR.
# =============================================================================
section "3. Bootstrapping ZDOTDIR in ~/.zshenv"

ZSHENV_HOME="$HOME/.zshenv"

if ! grep -q "ZDOTDIR" "$ZSHENV_HOME" 2>/dev/null; then
    cat >> "$ZSHENV_HOME" << 'EOF'

# Bootstrap XDG for zsh — must be in ~/.zshenv (only zsh dotfile needed in $HOME)
export ZDOTDIR="${XDG_CONFIG_HOME:-$HOME/.config}/zsh"
EOF
    ok "~/.zshenv updated with ZDOTDIR"
else
    warn "~/.zshenv already sets ZDOTDIR — skipped"
fi

# =============================================================================
# 4. Write $ZDOTDIR/.zshenv  (exported vars, sourced by ALL zsh instances)
# =============================================================================
section "4. Writing $ZDOTDIR/.zshenv"

cat > "$ZDOTDIR/.zshenv" << 'ZSHENVEOF'
# ~/.config/zsh/.zshenv — XDG exports for ALL zsh instances
# Generated by xdg-setup.sh

# ── XDG Base Dirs ─────────────────────────────────────────────────────────────
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"

# ── XDG User Dirs ─────────────────────────────────────────────────────────────
export XDG_DESKTOP_DIR="$HOME/desktop"
export XDG_DOWNLOAD_DIR="$HOME/downloads"
export XDG_DOCUMENTS_DIR="$HOME/documents"
export XDG_PICTURES_DIR="$HOME/pictures"
export XDG_MUSIC_DIR="$HOME/music"
export XDG_VIDEOS_DIR="$HOME/videos"

# ── Zsh state ─────────────────────────────────────────────────────────────────
export HISTFILE="$XDG_STATE_HOME/zsh/history"
export ZSH_COMPDUMP="$XDG_CACHE_HOME/zsh/zcompdump"

# ── Less ──────────────────────────────────────────────────────────────────────
export LESSHISTFILE="$XDG_STATE_HOME/less/history"

# ── Python ────────────────────────────────────────────────────────────────────
export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/startup.py"
export PYTHONPYCACHEPREFIX="$XDG_CACHE_HOME/python"
export PYTHONHISTORY="$XDG_STATE_HOME/python/history"
export PIP_CACHE_DIR="$XDG_CACHE_HOME/pip"
export PIPX_HOME="$XDG_DATA_HOME/pipx"
export PIPX_BIN_DIR="$HOME/.local/bin"
export VIRTUAL_ENV_DISABLE_PROMPT=1

# ── Node / npm ────────────────────────────────────────────────────────────────
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export NPM_CONFIG_CACHE="$XDG_CACHE_HOME/npm"
export NODE_REPL_HISTORY="$XDG_STATE_HOME/node/repl_history"

# ── Rust / Cargo ──────────────────────────────────────────────────────────────
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"

# ── Go ────────────────────────────────────────────────────────────────────────
export GOPATH="$XDG_DATA_HOME/go"
export GOMODCACHE="$XDG_CACHE_HOME/go/mod"

# ── Ruby / Gems ───────────────────────────────────────────────────────────────
export GEM_HOME="$XDG_DATA_HOME/gem"
export GEM_SPEC_CACHE="$XDG_CACHE_HOME/gem"
export BUNDLE_USER_CONFIG="$XDG_CONFIG_HOME/bundle"
export BUNDLE_USER_CACHE="$XDG_CACHE_HOME/bundle"
export BUNDLE_USER_PLUGIN="$XDG_DATA_HOME/bundle"

# ── Java / JVM tools ─────────────────────────────────────────────────────────
export GRADLE_USER_HOME="$XDG_DATA_HOME/gradle"
export MAVEN_OPTS="-Dmaven.repo.local=$XDG_CACHE_HOME/maven"

# ── Docker ────────────────────────────────────────────────────────────────────
export DOCKER_CONFIG="$XDG_CONFIG_HOME/docker"

# ── GPG ───────────────────────────────────────────────────────────────────────
export GNUPGHOME="$XDG_DATA_HOME/gnupg"

# ── wget ──────────────────────────────────────────────────────────────────────
export WGETRC="$XDG_CONFIG_HOME/wget/wgetrc"

# ── readline ─────────────────────────────────────────────────────────────────
export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"

# ── Ansible ───────────────────────────────────────────────────────────────────
export ANSIBLE_HOME="$XDG_CONFIG_HOME/ansible"

# ── AWS CLI ───────────────────────────────────────────────────────────────────
export AWS_SHARED_CREDENTIALS_FILE="$XDG_CONFIG_HOME/aws/credentials"
export AWS_CONFIG_FILE="$XDG_CONFIG_HOME/aws/config"

# ── Wine ──────────────────────────────────────────────────────────────────────
export WINEPREFIX="$XDG_DATA_HOME/wine"

# ── Pass ──────────────────────────────────────────────────────────────────────
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/pass"

# ── Vim ───────────────────────────────────────────────────────────────────────
export VIMINIT="set nocp | source $XDG_CONFIG_HOME/vim/vimrc"

# ── Rofi ──────────────────────────────────────────────────────────────────────
export ROFI_CONFIG="$XDG_CONFIG_HOME/rofi/config.rasi"

# ── PATH ──────────────────────────────────────────────────────────────────────
export PATH="$HOME/.local/bin:$XDG_DATA_HOME/cargo/bin:$XDG_DATA_HOME/go/bin:$PATH"
ZSHENVEOF

ok "$ZDOTDIR/.zshenv written"

# =============================================================================
# 5. Write $ZDOTDIR/.zprofile  (login shell — runs once at login)
# =============================================================================
section "5. Writing $ZDOTDIR/.zprofile"

if [[ ! -f "$ZDOTDIR/.zprofile" ]]; then
    cat > "$ZDOTDIR/.zprofile" << 'ZPROFEOF'
# ~/.config/zsh/.zprofile — login shell setup
# Generated by xdg-setup.sh

# Start ssh-agent if not running
if [[ -z "$SSH_AUTH_SOCK" ]]; then
    eval "$(ssh-agent -s)" > /dev/null
fi
ZPROFEOF
    ok "$ZDOTDIR/.zprofile written"
else
    warn "$ZDOTDIR/.zprofile already exists — skipped"
fi

# =============================================================================
# 6. Write $ZDOTDIR/.zshrc  (interactive shell)
# =============================================================================
section "6. Writing $ZDOTDIR/.zshrc"

if [[ ! -f "$ZDOTDIR/.zshrc" ]]; then
    cat > "$ZDOTDIR/.zshrc" << 'ZSHRCEOF'
# ~/.config/zsh/.zshrc — interactive zsh config
# Generated by xdg-setup.sh — extend freely

# ── History ───────────────────────────────────────────────────────────────────
HISTSIZE=100000
SAVEHIST=100000
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_SAVE_NO_DUPS
setopt HIST_REDUCE_BLANKS
setopt INC_APPEND_HISTORY_TIME
setopt SHARE_HISTORY

# ── Completion ────────────────────────────────────────────────────────────────
autoload -Uz compinit
# Regenerate compdump at most once per day
if [[ -n ${ZDOTDIR}/.zcompdump(#qN.mh+24) ]]; then
    compinit -d "$ZSH_COMPDUMP"
else
    compinit -C -d "$ZSH_COMPDUMP"
fi

zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' use-cache yes
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh/zcompcache"

# ── Directory navigation ──────────────────────────────────────────────────────
setopt AUTO_CD
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_SILENT

# ── Key bindings ──────────────────────────────────────────────────────────────
bindkey -e
bindkey '^[[A' history-search-backward
bindkey '^[[B' history-search-forward
bindkey '^[[H' beginning-of-line
bindkey '^[[F' end-of-line
bindkey '^[[3~' delete-char
bindkey '^[[1;5C' forward-word
bindkey '^[[1;5D' backward-word

# ── Prompt (git-aware; replace with starship if preferred) ────────────────────
autoload -Uz vcs_info
precmd() { vcs_info }
zstyle ':vcs_info:git:*' formats ' (%b)'
setopt PROMPT_SUBST
PROMPT='%F{green}%n@%m%f:%F{blue}%~%f%F{yellow}${vcs_info_msg_0_}%f%# '

# ── Aliases ───────────────────────────────────────────────────────────────────
alias ls='ls --color=auto'
alias ll='ls -lhA'
alias la='ls -A'
alias grep='grep --color=auto'
alias ip='ip --color=auto'
alias diff='diff --color=auto'
alias mkdir='mkdir -pv'

# ── Plugins (dnf-installable, no plugin manager needed) ───────────────────────
# sudo dnf install zsh-syntax-highlighting zsh-autosuggestions
[[ -f /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]] && \
    source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

[[ -f /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh ]] && \
    source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh

# ── Local overrides ───────────────────────────────────────────────────────────
[[ -f "$ZDOTDIR/.zshrc.local" ]] && source "$ZDOTDIR/.zshrc.local"
ZSHRCEOF
    ok "$ZDOTDIR/.zshrc written"
else
    warn "$ZDOTDIR/.zshrc already exists — skipped"
fi

# =============================================================================
# 7. Write tool config stubs
# =============================================================================
section "7. Writing tool config stubs"

if [[ ! -f "$XDG_CONFIG_HOME/readline/inputrc" ]]; then
    cat > "$XDG_CONFIG_HOME/readline/inputrc" << 'EOF'
$include /etc/inputrc
set editing-mode emacs
set bell-style none
set completion-ignore-case on
set show-all-if-ambiguous on
set colored-stats on
set mark-symlinked-directories on
EOF
    ok "readline/inputrc written"
fi

if [[ ! -f "$XDG_CONFIG_HOME/wget/wgetrc" ]]; then
    cat > "$XDG_CONFIG_HOME/wget/wgetrc" << EOF
hsts-file = $XDG_STATE_HOME/wget/hsts
EOF
    ok "wget/wgetrc written"
fi

if [[ ! -f "$XDG_CONFIG_HOME/npm/npmrc" ]]; then
    cat > "$XDG_CONFIG_HOME/npm/npmrc" << EOF
cache=${XDG_CACHE_HOME}/npm
init-module=${XDG_CONFIG_HOME}/npm/config/npm-init.js
prefix=${HOME}/.local
EOF
    ok "npm/npmrc written"
fi

if [[ ! -f "$XDG_CONFIG_HOME/python/startup.py" ]]; then
    cat > "$XDG_CONFIG_HOME/python/startup.py" << 'EOF'
import readline, rlcompleter, os

readline.parse_and_bind("tab: complete")
history_file = os.path.join(
    os.environ.get("XDG_STATE_HOME", os.path.expanduser("~/.local/state")),
    "python", "history"
)
os.makedirs(os.path.dirname(history_file), exist_ok=True)
try:
    readline.read_history_file(history_file)
except FileNotFoundError:
    pass
import atexit
atexit.register(readline.write_history_file, history_file)
EOF
    ok "python/startup.py written"
fi

if [[ ! -f "$XDG_CONFIG_HOME/gtk-3.0/settings.ini" ]]; then
    cat > "$XDG_CONFIG_HOME/gtk-3.0/settings.ini" << 'EOF'
[Settings]
gtk-application-prefer-dark-theme=1
gtk-button-images=0
gtk-menu-images=0
gtk-enable-event-sounds=0
gtk-enable-input-feedback-sounds=0
gtk-xft-antialias=1
gtk-xft-hinting=1
gtk-xft-hintstyle=hintslight
gtk-xft-rgba=rgb
EOF
    ok "gtk-3.0/settings.ini written"
fi

# =============================================================================
# 8. Migrate legacy HOME dotfiles
# =============================================================================
section "8. Migrating legacy dotfiles"

migrate() {
    local src="$1" dst="$2"
    if [[ -e "$src" && ! -L "$src" ]]; then
        mkdir -p "$(dirname "$dst")"
        mv "$src" "$dst"
        ln -s "$dst" "$src"
        warn "Migrated $src → $dst  (symlink left for compatibility)"
    fi
}

migrate "$HOME/.zsh_history"        "$XDG_STATE_HOME/zsh/history"
migrate "$HOME/.lesshst"            "$XDG_STATE_HOME/less/history"
migrate "$HOME/.node_repl_history"  "$XDG_STATE_HOME/node/repl_history"
migrate "$HOME/.python_history"     "$XDG_STATE_HOME/python/history"
migrate "$HOME/.wget-hsts"          "$XDG_STATE_HOME/wget/hsts"
migrate "$HOME/.gnupg"              "$XDG_DATA_HOME/gnupg"
migrate "$HOME/.cargo"              "$XDG_DATA_HOME/cargo"
migrate "$HOME/.rustup"             "$XDG_DATA_HOME/rustup"
migrate "$HOME/.npm"                "$XDG_CACHE_HOME/npm"
migrate "$HOME/.docker"             "$XDG_CONFIG_HOME/docker"
migrate "$HOME/.ansible"            "$XDG_CONFIG_HOME/ansible"
migrate "$HOME/.gradle"             "$XDG_DATA_HOME/gradle"
migrate "$HOME/.wineprefix"         "$XDG_DATA_HOME/wine"
migrate "$HOME/.password-store"     "$XDG_DATA_HOME/pass"

# =============================================================================
# 9. Set zsh as default shell (if not already)
# =============================================================================
section "9. Default shell"

ZSH_PATH="$(command -v zsh)"
if [[ "$SHELL" != "$ZSH_PATH" ]]; then
    info "Changing default shell to $ZSH_PATH"
    chsh -s "$ZSH_PATH"
    ok "Default shell changed. Takes effect on next login."
else
    ok "Default shell is already zsh"
fi

# =============================================================================
# 10. Suggest optional packages
# =============================================================================
section "10. Optional enhancements"

pkgs=()
rpm -q zsh-syntax-highlighting &>/dev/null || pkgs+=("zsh-syntax-highlighting")
rpm -q zsh-autosuggestions     &>/dev/null || pkgs+=("zsh-autosuggestions")
command -v starship             &>/dev/null || pkgs+=("starship")

if [[ ${#pkgs[@]} -gt 0 ]]; then
    info "Recommended packages not yet installed:"
    for p in "${pkgs[@]}"; do echo "    $p"; done
    echo ""
    info "Install with: sudo dnf install -y ${pkgs[*]}"
    info "(starship may need: curl -sS https://starship.rs/install.sh | sh)"
fi

# =============================================================================
# 11. Audit remaining HOME dotfiles
# =============================================================================
section "11. Remaining HOME dotfile audit"

echo ""
info "Dotfiles still in \$HOME (review manually):"
while IFS= read -r f; do
    case "$(basename "$f")" in
        .zshenv|.bash_profile|.bashrc|.bash_logout|.profile) ;;
        .ssh|.local|.config|.cache) ;;
        *) echo "    $f" ;;
    esac
done < <(find "$HOME" -maxdepth 1 -name '.*' 2>/dev/null | sort)

echo ""
info ".zshenv is the ONLY zsh file that should remain in \$HOME."
info ".ssh cannot be moved — SSH ignores XDG by design."

# =============================================================================
# Done
# =============================================================================
section "Done"
ok "XDG + Zsh setup complete."
echo ""
echo "  Zsh startup chain:"
echo "    ~/.zshenv                  sets ZDOTDIR  (only file needed in \$HOME)"
echo "    ~/.config/zsh/.zshenv      all XDG exports (every zsh instance)"
echo "    ~/.config/zsh/.zprofile    login shell  (ssh-agent etc.)"
echo "    ~/.config/zsh/.zshrc       interactive config"
echo ""
echo "  Activate now:"
echo "    exec zsh"
echo ""
info "Verify with:  env | grep XDG"
