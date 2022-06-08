config.load_autoconfig()

# color scheme
import fairyfloss.draw
fairyfloss.draw.konda(c)

# font
my_font = "11pt Latin Modern Mono"
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

# userscripts
config.bind(',b', 'spawn --userscript qute-bitwarden')
config.bind(',m', 'spawn --userscript view_in_mpv')
config.bind(',z', 'spawn --userscript zotero')
config.bind(',Z', 'hint links userscript zotero')

# code selection
c.hints.selectors["code"] = [
    # Selects all code tags whose direct parent is not a pre tag
    ":not(pre) > code",
    "pre"
]
config.bind(',c', 'hint code userscript code_select')

