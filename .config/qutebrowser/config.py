#!/usr/env python3

config.load_autoconfig()

# Solarized colorscheme
import solarized.draw

solarized.draw.konda(c)

c.content.user_stylesheets = [
    '~/.config/qutebrowser/solarized/solarized-all-sites.css'
]

config.bind(
    '<Ctrl-R>',
    'config-cycle content.user_stylesheets "~/.config/qutebrowser/solarized/solarized-all-sites.css" ""'
)

# Fonts
my_font = "10pt Iosevka"
c.fonts.hints = my_font
c.fonts.keyhint = my_font
c.fonts.prompts = my_font
c.fonts.downloads = my_font
c.fonts.statusbar = my_font
c.fonts.contextmenu = my_font
c.fonts.messages.info = my_font
c.fonts.debug_console = my_font
c.fonts.completion.entry = my_font
c.fonts.completion.category = my_font

# Bitwarden using dmenu
config.bind('zl', 'spawn --userscript bw-dmenu-fill')

# Zotero integration
config.bind('zt', 'spawn --userscript qute-zotero')

# Mpv picture in picture mode
config.bind('zm', 'hint links spawn --detach mpv {hint-url}')
