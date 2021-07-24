#!/usr/bin/env python3


# Solarized Light scheme by Ethan Schoonover (modified by aramisgithub)
def konda(c):
    colors = {
        'base00': '#fdf6e3',
        'base01': '#eee8d5',
        'base02': '#93a1a1',
        'base03': '#839496',
        'base04': '#657b83',
        'base05': '#586e75',
        'base06': '#073642',
        'base07': '#002b36',
        'base08': '#dc322f',
        'base09': '#cb4b16',
        'base0A': '#b58900',
        'base0B': '#859900',
        'base0C': '#2aa198',
        'base0D': '#268bd2',
        'base0E': '#6c71c4',
        'base0F': '#d33682',
    }

    # Text color of the completion widget. May be a single color to use for
    # all columns or a list of three colors, one for each column.
    c.colors.completion.fg = colors['base05']

    # Background color of the completion widget for odd rows.
    c.colors.completion.odd.bg = colors['base01']

    # Background color of the completion widget for even rows.
    c.colors.completion.even.bg = colors['base00']

    # Foreground color of completion widget category headers.
    c.colors.completion.category.fg = colors['base0A']

    # Background color of the completion widget category headers.
    c.colors.completion.category.bg = colors['base00']

    # Top border color of the completion widget category headers.
    c.colors.completion.category.border.top = colors['base00']

    # Bottom border color of the completion widget category headers.
    c.colors.completion.category.border.bottom = colors['base00']

    # Foreground color of the selected completion item.
    c.colors.completion.item.selected.fg = colors['base05']

    # Background color of the selected completion item.
    c.colors.completion.item.selected.bg = colors['base02']

    # Top border color of the selected completion item.
    c.colors.completion.item.selected.border.top = colors['base02']

    # Bottom border color of the selected completion item.
    c.colors.completion.item.selected.border.bottom = colors['base02']

    # Foreground color of the matched text in the selected completion item.
    c.colors.completion.item.selected.match.fg = colors['base0B']

    # Foreground color of the matched text in the completion.
    c.colors.completion.match.fg = colors['base0B']

    # Color of the scrollbar handle in the completion view.
    c.colors.completion.scrollbar.fg = colors['base05']

    # Color of the scrollbar in the completion view.
    c.colors.completion.scrollbar.bg = colors['base00']

    # Background color of disabled items in the context menu.
    c.colors.contextmenu.disabled.bg = colors['base01']

    # Foreground color of disabled items in the context menu.
    c.colors.contextmenu.disabled.fg = colors['base04']

    # Background color of the context menu. If set to null, the Qt default is used.
    c.colors.contextmenu.menu.bg = colors['base00']

    # Foreground color of the context menu. If set to null, the Qt default is used.
    c.colors.contextmenu.menu.fg = colors['base05']

    # Background color of the context menu’s selected item. If set to null, the Qt default is used.
    c.colors.contextmenu.selected.bg = colors['base02']

    #Foreground color of the context menu’s selected item. If set to null, the Qt default is used.
    c.colors.contextmenu.selected.fg = colors['base05']

    # Background color for the download bar.
    c.colors.downloads.bar.bg = colors['base00']

    # Color gradient start for download text.
    c.colors.downloads.start.fg = colors['base00']

    # Color gradient start for download backgrounds.
    c.colors.downloads.start.bg = colors['base0D']

    # Color gradient end for download text.
    c.colors.downloads.stop.fg = colors['base00']

    # Color gradient stop for download backgrounds.
    c.colors.downloads.stop.bg = colors['base0C']

    # Foreground color for downloads with errors.
    c.colors.downloads.error.fg = colors['base08']

    # Font color for hints.
    c.colors.hints.fg = colors['base00']

    # Background color for hints. Note that you can use a `rgba(...)` value
    # for transparency.
    c.colors.hints.bg = colors['base0A']

    # Font color for the matched part of hints.
    c.colors.hints.match.fg = colors['base05']

    # Text color for the keyhint widget.
    c.colors.keyhint.fg = colors['base05']

    # Highlight color for keys to complete the current keychain.
    c.colors.keyhint.suffix.fg = colors['base05']

    # Background color of the keyhint widget.
    c.colors.keyhint.bg = colors['base00']

    # Foreground color of an error message.
    c.colors.messages.error.fg = colors['base00']

    # Background color of an error message.
    c.colors.messages.error.bg = colors['base08']

    # Border color of an error message.
    c.colors.messages.error.border = colors['base08']

    # Foreground color of a warning message.
    c.colors.messages.warning.fg = colors['base00']

    # Background color of a warning message.
    c.colors.messages.warning.bg = colors['base0E']

    # Border color of a warning message.
    c.colors.messages.warning.border = colors['base0E']

    # Foreground color of an info message.
    c.colors.messages.info.fg = colors['base05']

    # Background color of an info message.
    c.colors.messages.info.bg = colors['base00']

    # Border color of an info message.
    c.colors.messages.info.border = colors['base00']

    # Foreground color for prompts.
    c.colors.prompts.fg = colors['base05']

    # Border used around UI elements in prompts.
    c.colors.prompts.border = colors['base00']

    # Background color for prompts.
    c.colors.prompts.bg = colors['base00']

    # Background color for the selected item in filename prompts.
    c.colors.prompts.selected.bg = colors['base02']

    # Foreground color of the statusbar.
    c.colors.statusbar.normal.fg = colors['base0B']

    # Background color of the statusbar.
    c.colors.statusbar.normal.bg = colors['base00']

    # Foreground color of the statusbar in insert mode.
    c.colors.statusbar.insert.fg = colors['base00']

    # Background color of the statusbar in insert mode.
    c.colors.statusbar.insert.bg = colors['base0D']

    # Foreground color of the statusbar in passthrough mode.
    c.colors.statusbar.passthrough.fg = colors['base00']

    # Background color of the statusbar in passthrough mode.
    c.colors.statusbar.passthrough.bg = colors['base0C']

    # Foreground color of the statusbar in private browsing mode.
    c.colors.statusbar.private.fg = colors['base00']

    # Background color of the statusbar in private browsing mode.
    c.colors.statusbar.private.bg = colors['base01']

    # Foreground color of the statusbar in command mode.
    c.colors.statusbar.command.fg = colors['base05']

    # Background color of the statusbar in command mode.
    c.colors.statusbar.command.bg = colors['base00']

    # Foreground color of the statusbar in private browsing + command mode.
    c.colors.statusbar.command.private.fg = colors['base05']

    # Background color of the statusbar in private browsing + command mode.
    c.colors.statusbar.command.private.bg = colors['base00']

    # Foreground color of the statusbar in caret mode.
    c.colors.statusbar.caret.fg = colors['base00']

    # Background color of the statusbar in caret mode.
    c.colors.statusbar.caret.bg = colors['base0E']

    # Foreground color of the statusbar in caret mode with a selection.
    c.colors.statusbar.caret.selection.fg = colors['base00']

    # Background color of the statusbar in caret mode with a selection.
    c.colors.statusbar.caret.selection.bg = colors['base0D']

    # Background color of the progress bar.
    c.colors.statusbar.progress.bg = colors['base0D']

    # Default foreground color of the URL in the statusbar.
    c.colors.statusbar.url.fg = colors['base05']

    # Foreground color of the URL in the statusbar on error.
    c.colors.statusbar.url.error.fg = colors['base08']

    # Foreground color of the URL in the statusbar for hovered links.
    c.colors.statusbar.url.hover.fg = colors['base05']

    # Foreground color of the URL in the statusbar on successful load
    # (http).
    c.colors.statusbar.url.success.http.fg = colors['base0C']

    # Foreground color of the URL in the statusbar on successful load
    # (https).
    c.colors.statusbar.url.success.https.fg = colors['base0B']

    # Foreground color of the URL in the statusbar when there's a warning.
    c.colors.statusbar.url.warn.fg = colors['base0E']

    # Background color of the tab bar.
    c.colors.tabs.bar.bg = colors['base00']

    # Color gradient start for the tab indicator.
    c.colors.tabs.indicator.start = colors['base0D']

    # Color gradient end for the tab indicator.
    c.colors.tabs.indicator.stop = colors['base0C']

    # Color for the tab indicator on errors.
    c.colors.tabs.indicator.error = colors['base08']

    # Foreground color of unselected odd tabs.
    c.colors.tabs.odd.fg = colors['base05']

    # Background color of unselected odd tabs.
    c.colors.tabs.odd.bg = colors['base01']

    # Foreground color of unselected even tabs.
    c.colors.tabs.even.fg = colors['base05']

    # Background color of unselected even tabs.
    c.colors.tabs.even.bg = colors['base00']

    # Background color of pinned unselected even tabs.
    c.colors.tabs.pinned.even.bg = colors['base0C']

    # Foreground color of pinned unselected even tabs.
    c.colors.tabs.pinned.even.fg = colors['base07']

    # Background color of pinned unselected odd tabs.
    c.colors.tabs.pinned.odd.bg = colors['base0B']

    # Foreground color of pinned unselected odd tabs.
    c.colors.tabs.pinned.odd.fg = colors['base07']

    # Background color of pinned selected even tabs.
    c.colors.tabs.pinned.selected.even.bg = colors['base02']

    # Foreground color of pinned selected even tabs.
    c.colors.tabs.pinned.selected.even.fg = colors['base05']

    # Background color of pinned selected odd tabs.
    c.colors.tabs.pinned.selected.odd.bg = colors['base02']

    # Foreground color of pinned selected odd tabs.
    c.colors.tabs.pinned.selected.odd.fg = colors['base05']

    # Foreground color of selected odd tabs.
    c.colors.tabs.selected.odd.fg = colors['base05']

    # Background color of selected odd tabs.
    c.colors.tabs.selected.odd.bg = colors['base02']

    # Foreground color of selected even tabs.
    c.colors.tabs.selected.even.fg = colors['base05']

    # Background color of selected even tabs.
    c.colors.tabs.selected.even.bg = colors['base02']

    # Background color for webpages if unset (or empty to use the theme's
    # color).
    # c.colors.webpage.bg = colors['base00']
