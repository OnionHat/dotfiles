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

## Betterlockscreen
`xfconf-query -c xfce4-session -p /general/LockCommand -s "betterlockscreen -l" -n -t string`
