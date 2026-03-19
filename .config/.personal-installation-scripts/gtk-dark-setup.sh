#!/bin/bash
# GTK Dark Theme Setup — Fedora i3
# Materia-dark + Papirus-Dark + Inter

set -e

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

ok()   { echo -e "${GREEN}✓${NC} $1"; }
warn() { echo -e "${YELLOW}⚠${NC} $1"; }
err()  { echo -e "${RED}✗${NC} $1"; }

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "  GTK Dark Theme Setup"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# 1) Tema ve icon pack kurulu mu kontrol et, değilse kur
echo ""
echo "▶ Bağımlılıklar kontrol ediliyor..."

install_if_missing() {
    if ! rpm -q "$1" &>/dev/null; then
        warn "$1 bulunamadı, kuruluyor..."
        sudo dnf install -y "$1" && ok "$1 kuruldu" || err "$1 kurulamadı"
    else
        ok "$1 zaten kurulu"
    fi
}

install_if_missing materia-gtk-theme
install_if_missing papirus-icon-theme

# Inter font kontrolü (manuel kurulum gerekebilir)
if fc-list | grep -qi "Inter"; then
    ok "Inter font mevcut"
else
    warn "Inter font bulunamadı. Google Fonts'tan kurmayı düşünebilirsin."
    warn "  https://fonts.google.com/specimen/Inter"
fi

# 2) GTK-3 ayarları (zaten mevcut ama kontrol et)
echo ""
echo "▶ GTK-3 ayarlanıyor..."
mkdir -p ~/.config/gtk-3.0
cat > ~/.config/gtk-3.0/settings.ini << 'EOF'
[Settings]
gtk-theme-name=Materia-dark
gtk-icon-theme-name=Papirus-Dark
gtk-font-name=Inter 12
gtk-cursor-theme-name=Adwaita
gtk-cursor-theme-size=0
gtk-toolbar-style=GTK_TOOLBAR_BOTH
gtk-toolbar-icon-size=GTK_ICON_SIZE_LARGE_TOOLBAR
gtk-button-images=1
gtk-menu-images=1
gtk-enable-event-sounds=1
gtk-enable-input-feedback-sounds=1
gtk-xft-antialias=1
gtk-xft-hinting=1
gtk-xft-hintstyle=hintfull
gtk-xft-rgba=none
gtk-application-prefer-dark-theme=1
EOF
ok "GTK-3 ayarlandı"

# 3) GTK-4
echo ""
echo "▶ GTK-4 ayarlanıyor..."
mkdir -p ~/.config/gtk-4.0
cp ~/.config/gtk-3.0/settings.ini ~/.config/gtk-4.0/settings.ini
ok "GTK-4 ayarlandı"

# 4) GTK-2
echo ""
echo "▶ GTK-2 ayarlanıyor..."
cat > ~/.gtkrc-2.0 << 'EOF'
gtk-theme-name="Materia-dark"
gtk-icon-theme-name="Papirus-Dark"
gtk-font-name="Inter 12"
gtk-cursor-theme-name="Adwaita"
gtk-cursor-theme-size=0
gtk-toolbar-style=GTK_TOOLBAR_BOTH
gtk-toolbar-icon-size=GTK_ICON_SIZE_LARGE_TOOLBAR
gtk-button-images=1
gtk-menu-images=1
gtk-enable-event-sounds=1
gtk-enable-input-feedback-sounds=1
gtk-xft-antialias=1
gtk-xft-hinting=1
gtk-xft-hintstyle=hintfull
gtk-xft-rgba=none
EOF
ok "GTK-2 ayarlandı"

# 5) gsettings (GNOME dconf değerleri — bazı uygulamalar buraya bakıyor)
echo ""
echo "▶ gsettings ayarlanıyor..."
if command -v gsettings &>/dev/null; then
    gsettings set org.gnome.desktop.interface gtk-theme        'Materia-dark'      2>/dev/null && ok "gsettings: gtk-theme"       || warn "gsettings gtk-theme ayarlanamadı"
    gsettings set org.gnome.desktop.interface icon-theme       'Papirus-Dark'      2>/dev/null && ok "gsettings: icon-theme"      || warn "gsettings icon-theme ayarlanamadı"
    gsettings set org.gnome.desktop.interface cursor-theme     'Adwaita'           2>/dev/null && ok "gsettings: cursor-theme"    || warn "gsettings cursor-theme ayarlanamadı"
    gsettings set org.gnome.desktop.interface font-name        'Inter 12'          2>/dev/null && ok "gsettings: font"            || warn "gsettings font ayarlanamadı"
    gsettings set org.gnome.desktop.interface color-scheme     'prefer-dark'       2>/dev/null && ok "gsettings: color-scheme"   || warn "gsettings color-scheme ayarlanamadı"
else
    warn "gsettings bulunamadı, atlanıyor"
fi

# 6) xprofile — environment variables
echo ""
echo "▶ ~/.xprofile ayarlanıyor..."
XPROFILE=~/.xprofile
touch "$XPROFILE"

add_if_missing() {
    grep -qF "$1" "$XPROFILE" || echo "$1" >> "$XPROFILE"
}

add_if_missing 'export GTK_THEME=Materia-dark'
add_if_missing 'export GTK2_RC_FILES="$HOME/.gtkrc-2.0"'
add_if_missing 'export QT_STYLE_OVERRIDE=gtk2'
add_if_missing 'export QT_QPA_PLATFORMTHEME=gtk2'
ok "~/.xprofile güncellendi"

# 7) Qt için qt5ct / qt6ct (opsiyonel ama varsa kur)
echo ""
echo "▶ Qt tema desteği kontrol ediliyor..."
if command -v qt5ct &>/dev/null; then
    ok "qt5ct zaten kurulu"
elif rpm -q qt5-qtstyleplugins &>/dev/null || rpm -q qt5ct &>/dev/null; then
    ok "Qt5 tema araçları kurulu"
else
    warn "qt5ct bulunamadı. Qt uygulamaları (ör: VLC, qBittorrent) için:"
    warn "  sudo dnf install qt5ct qt5-qtstyleplugins"
fi

# 8) Xresources (bazı eski uygulamalar ve terminal emulatörleri için)
echo ""
echo "▶ ~/.Xresources kontrol ediliyor..."
XRES=~/.Xresources
touch "$XRES"
grep -qF "Xft.antialias" "$XRES" || cat >> "$XRES" << 'EOF'

! Font rendering
Xft.antialias: 1
Xft.hinting:   1
Xft.hintstyle: hintfull
Xft.rgba:      none
EOF
ok "~/.Xresources güncellendi"

# Özet
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo -e "${GREEN}  Tamamlandı!${NC}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "Değişikliklerin tam etki etmesi için:"
echo "  → Oturumu kapat ve tekrar gir"
echo "  → Ya da: source ~/.xprofile"
echo ""
echo "Qt uygulamaları hâlâ açık renkli görünüyorsa:"
echo "  sudo dnf install qt5ct qt5-qtstyleplugins"
echo "  ~/.xprofile'a QT_QPA_PLATFORMTHEME=qt5ct ekle"
echo "  qt5ct uygulamasını aç, tema olarak GTK2 seç"
echo ""
