#!/usr/bin/env python3

# Acme color scheme
def konda(c):
    colors = {
        'base00': '#424242',
        'base01': '#b85c57',
        'base02': '#57864e',
        'base03': '#8f7634',
        'base04': '#2a8dc5',
        'base05': '#8888c7',
        'base06': '#6aa7a8',
        'base07': '#999957',
        'base08': '#b7b19c',
        'base09': '#f2acaa',
        'base0A': '#98ce8f',
        'base0B': '#ededa6',
        'base0C': '#a6dcf8',
        'base0D': '#d0d0f7',
        'base0E': '#b0eced',
        'base0F': '#ffffec',
        'light00':'#EAEBDB',
        'light01':'#EFFEEC',
        'light02':'#EEFEFF',
        'light03':'#E2F1F8'
    }

    # Text color of the completion widget. May be a single color to use for
    # all columns or a list of three colors, one for each column.
    c.colors.completion.fg = colors['base00']

    # Background color of the completion widget for odd rows.
    c.colors.completion.odd.bg = colors['light03']

    # Background color of the completion widget for even rows.
    c.colors.completion.even.bg = colors['light02']

    # Foreground color of completion widget category headers.
    c.colors.completion.category.fg = colors['base00']

    # Background color of the completion widget category headers.
    c.colors.completion.category.bg = colors['base0C']

    # Top border color of the completion widget category headers.
    c.colors.completion.category.border.top = colors['base0C']

    # Bottom border color of the completion widget category headers.
    c.colors.completion.category.border.bottom = colors['base0C']

    # Foreground color of the selected completion item.
    c.colors.completion.item.selected.fg = colors['base0F']

    # Background color of the selected completion item.
    c.colors.completion.item.selected.bg = colors['base04']

    # Top border color of the selected completion item.
    c.colors.completion.item.selected.border.top = colors['base04']

    # Bottom border color of the selected completion item.
    c.colors.completion.item.selected.border.bottom = colors['base04']

    # Foreground color of the matched text in the selected completion item.
    c.colors.completion.item.selected.match.fg = colors['base0C']

    # Foreground color of the matched text in the completion.
    c.colors.completion.match.fg = colors['base05']

    # Color of the scrollbar handle in the completion view.
    c.colors.completion.scrollbar.fg = colors['base04']

    # Color of the scrollbar in the completion view.
    c.colors.completion.scrollbar.bg = colors['base0C']

    # Background color of disabled items in the context menu.
    c.colors.contextmenu.disabled.bg = colors['light00']

    # Foreground color of disabled items in the context menu.
    c.colors.contextmenu.disabled.fg = colors['base08']

    # Background color of the context menu. If set to null, the Qt default is used.
    c.colors.contextmenu.menu.bg = colors['base0F']

    # Foreground color of the context menu. If set to null, the Qt default is used.
    c.colors.contextmenu.menu.fg = colors['base00']

    # Background color of the context menu’s selected item. If set to null, the Qt default is used.
    c.colors.contextmenu.selected.bg = colors['base0C']

    #Foreground color of the context menu’s selected item. If set to null, the Qt default is used.
    c.colors.contextmenu.selected.fg = colors['base00']

    # Background color for the download bar.
    c.colors.downloads.bar.bg = colors['base0D']

    # Color gradient start for download text.
    c.colors.downloads.start.fg = colors['base00']

    # Color gradient start for download backgrounds.
    c.colors.downloads.start.bg = colors['base0E']

    # Color gradient end for download text.
    c.colors.downloads.stop.fg = colors['base00']

    # Color gradient stop for download backgrounds.
    c.colors.downloads.stop.bg = colors['base0D']

    # Foreground color for downloads with errors.
    c.colors.downloads.error.fg = colors['base01']

    # Font color for hints.
    c.colors.hints.fg = colors['base00']

    # Background color for hints. Note that you can use a `rgba(...)` value
    # for transparency.
    c.colors.hints.bg = colors['base0B']

    # Font color for the matched part of hints.
    c.colors.hints.match.fg = colors['base03']

    # Text color for the keyhint widget.
    c.colors.keyhint.fg = colors['base00']

    # Highlight color for keys to complete the current keychain.
    c.colors.keyhint.suffix.fg = colors['base02']

    # Background color of the keyhint widget.
    c.colors.keyhint.bg = colors['base0A']

    # Foreground color of an error message.
    c.colors.messages.error.fg = colors['base00']

    # Background color of an error message.
    c.colors.messages.error.bg = colors['base09']

    # Border color of an error message.
    c.colors.messages.error.border = colors['base01']

    # Foreground color of a warning message.
    c.colors.messages.warning.fg = colors['base00']

    # Background color of a warning message.
    c.colors.messages.warning.bg = colors['base0B']

    # Border color of a warning message.
    c.colors.messages.warning.border = colors['base03']

    # Foreground color of an info message.
    c.colors.messages.info.fg = colors['base00']

    # Background color of an info message.
    c.colors.messages.info.bg = colors['base0C']

    # Border color of an info message.
    c.colors.messages.info.border = colors['base03']

    # Foreground color for prompts.
    c.colors.prompts.fg = colors['base03']

    # Border used around UI elements in prompts.
    c.colors.prompts.border = colors['base0F']

    # Background color for prompts.
    c.colors.prompts.bg = colors['base0F']

    # Background color for the selected item in filename prompts.
    c.colors.prompts.selected.bg = colors['light01']

    # Foreground color of the statusbar.
    c.colors.statusbar.normal.fg = colors['base00']

    # Background color of the statusbar.
    c.colors.statusbar.normal.bg = colors['light02']

    # Foreground color of the statusbar in insert mode.
    c.colors.statusbar.insert.fg = colors['base00']

    # Background color of the statusbar in insert mode.
    c.colors.statusbar.insert.bg = colors['base0A']

    # Foreground color of the statusbar in passthrough mode.
    c.colors.statusbar.passthrough.fg = colors['base00']

    # Background color of the statusbar in passthrough mode.
    c.colors.statusbar.passthrough.bg = colors['base0B']

    # Foreground color of the statusbar in private browsing mode.
    c.colors.statusbar.private.fg = colors['base0F']

    # Background color of the statusbar in private browsing mode.
    c.colors.statusbar.private.bg = colors['base00']

    # Foreground color of the statusbar in command mode.
    c.colors.statusbar.command.fg = colors['base00']

    # Background color of the statusbar in command mode.
    c.colors.statusbar.command.bg = colors['base0E']

    # Foreground color of the statusbar in private browsing + command mode.
    c.colors.statusbar.command.private.fg = colors['base00']

    # Background color of the statusbar in private browsing + command mode.
    c.colors.statusbar.command.private.bg = colors['base0E']

    # Foreground color of the statusbar in caret mode.
    c.colors.statusbar.caret.fg = colors['base00']

    # Background color of the statusbar in caret mode.
    c.colors.statusbar.caret.bg = colors['light01']

    # Foreground color of the statusbar in caret mode with a selection.
    c.colors.statusbar.caret.selection.fg = colors['base00']

    # Background color of the statusbar in caret mode with a selection.
    c.colors.statusbar.caret.selection.bg = colors['light00']

    # Background color of the progress bar.
    c.colors.statusbar.progress.bg = colors['base0C']

    # Default foreground color of the URL in the statusbar.
    c.colors.statusbar.url.fg = colors['base08']

    # Foreground color of the URL in the statusbar on error.
    c.colors.statusbar.url.error.fg = colors['base01']

    # Foreground color of the URL in the statusbar for hovered links.
    c.colors.statusbar.url.hover.fg = colors['base04']

    # Foreground color of the URL in the statusbar on successful load
    # (http).
    c.colors.statusbar.url.success.http.fg = colors['base03']

    # Foreground color of the URL in the statusbar on successful load
    # (https).
    c.colors.statusbar.url.success.https.fg = colors['base06']

    # Foreground color of the URL in the statusbar when there's a warning.
    c.colors.statusbar.url.warn.fg = colors['base07']

    # Background color of the tab bar.
    c.colors.tabs.bar.bg = colors['light03']

    # Color gradient start for the tab indicator.
    c.colors.tabs.indicator.start = colors['light01']

    # Color gradient end for the tab indicator.
    c.colors.tabs.indicator.stop = colors['light03']

    # Color for the tab indicator on errors.
    c.colors.tabs.indicator.error = colors['base09']

    # Foreground color of unselected odd tabs.
    c.colors.tabs.odd.fg = colors['base00']

    # Background color of unselected odd tabs.
    c.colors.tabs.odd.bg = colors['light02']

    # Foreground color of unselected even tabs.
    c.colors.tabs.even.fg = colors['base00']

    # Background color of unselected even tabs.
    c.colors.tabs.even.bg = colors['light03']

    # Background color of pinned unselected even tabs.
    c.colors.tabs.pinned.even.bg = colors['light01']

    # Foreground color of pinned unselected even tabs.
    c.colors.tabs.pinned.even.fg = colors['base00']

    # Background color of pinned unselected odd tabs.
    c.colors.tabs.pinned.odd.bg = colors['light00']

    # Foreground color of pinned unselected odd tabs.
    c.colors.tabs.pinned.odd.fg = colors['base00']

    # Background color of pinned selected even tabs.
    c.colors.tabs.pinned.selected.even.bg = colors['base0C']

    # Foreground color of pinned selected even tabs.
    c.colors.tabs.pinned.selected.even.fg = colors['base00']

    # Background color of pinned selected odd tabs.
    c.colors.tabs.pinned.selected.odd.bg = colors['base0C']

    # Foreground color of pinned selected odd tabs.
    c.colors.tabs.pinned.selected.odd.fg = colors['base00']

    # Foreground color of selected odd tabs.
    c.colors.tabs.selected.odd.fg = colors['base00']

    # Background color of selected odd tabs.
    c.colors.tabs.selected.odd.bg = colors['base0C']

    # Foreground color of selected even tabs.
    c.colors.tabs.selected.even.fg = colors['base00']

    # Background color of selected even tabs.
    c.colors.tabs.selected.even.bg = colors['base0C']

    # Background color for webpages if unset (or empty to use the theme's
    # color).
    c.colors.webpage.bg = colors['base0F']
