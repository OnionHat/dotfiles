# dotfiles
This dotfiles repo uses `stow`

## Firefox
#### Fullscreen bookmark toolbar
Enable toolkit.legacyUserProfileCustomizations.stylesheets in about:config

Go to `Menu -> Help -> Troubleshooting Information -> Profile Directory`

`mkdir chrome` in Profile Directory and `nvim chrome/userChrome.css`
```css
#navigator-toolbox[inFullscreen="true"] #PersonalToolbar {
  visibility: unset !important;
}
```

### Dark Mode in `Reader view`

`about:config` -> `reader.color_scheme = dark`

### Fullscreen respecting window size
`about:config -> full-screen-api.ignore-widgets = true`

### Dont remeber last window size
Seems to break pdf viewer and dark mode.
`about:config -> privacy.resistFingerprinting = true`

## LibreOffice
#### Install Calibri
`yay -S ttf-vista-font`

#### Fix calibri rendering issue
`nvim ~/.config/fontconfig/fonts.conf`
```xml
</fontconfig>
  <match target="font">
    <edit name="embeddedbitmap" mode="assign">
      <bool>false</bool>
    </edit>
  </match>
</fontconfig>
```

### XFCE4 power manager
## Let logind handle lid close
`xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/logind-handle-lid-switch -s true`

### Remap caps 2 esc/ctrl
`pacman -S interception-caps2esc`
`/etc/interception/udevmon.yaml`
```yaml
- JOB: intercept -g $DEVNODE | caps2esc -m 1 | uinput -d $DEVNODE
  DEVICE:
    EVENTS:
      EV_KEY: [KEY_CAPSLOCK, KEY_ESC]
```

## Emacs
Install doom emacs:
```sh
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install
```

## Onedrive
Install onedrive client from AUR and choose ldc (option 2)
`paru -S onedrive-abraunegg`
Connect onedrive account
`onedrive`
Start systemd service
`systemctl enable --now onedrive`
