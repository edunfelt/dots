#!/usr/bin/env python3
# This is an adaption of NLKNguyen (https://github.com/NLKNguyen)'s
# PaperColor Light theme for qutebrowser (https://qutebrowser.org/)
#
# @author: stoerdebegga <stoerdebegga@mailbox.org>
# @source: https://codeberg.org/stoerdebegga/papercolor-light-contrib
# @source99: https://github.com/stoerdebegga/papercolor-light-contrib
#
# @author stoerdebegga <stoerdebegga@mailbox.org>
#
# --[Papercolor Light]----------------------------------------------
def konda(c):
    colors = {
        "bg": "#eeeeee",
        "red": "#af0000",
        "green": "#5f8700",
        "aqua": "#0087af",
        "gray": "#878787",
        "navy": "#005f87",
        "fg": "#444444",
        "midgray": "#878787",
        "lightgray": "#bcbcbc",
        "error": "#d70000",
        "pink": "#d70087",
        "orange": "#d75f00",
    }

    # Background color of the completion widget category headers.
    # Type: QssColor
    c.colors.completion.category.bg = colors["bg"]

    # Bottom border color of the completion widget category headers.
    # Type: QssColor
    c.colors.completion.category.border.bottom = colors["bg"]

    # Top border color of the completion widget category headers.
    # Type: QssColor
    c.colors.completion.category.border.top = colors["bg"]

    # Foreground color of completion widget category headers.
    # Type: QtColor
    c.colors.completion.category.fg = colors["fg"]

    # Background color of the completion widget for even rows.
    # Type: QssColor
    c.colors.completion.even.bg = colors["bg"]

    # Background color of the completion widget for odd rows.
    # Type: QssColor
    c.colors.completion.odd.bg = colors["bg"]

    # Text color of the completion widget.
    # Type: QtColor
    c.colors.completion.fg = colors["fg"]

    # Background color of the selected completion item.
    # Type: QssColor
    c.colors.completion.item.selected.bg = colors["lightgray"]

    # Bottom border color of the selected completion item.
    # Type: QssColor
    c.colors.completion.item.selected.border.bottom = colors["gray"]

    # Top border color of the completion widget category headers.
    # Type: QssColor
    c.colors.completion.item.selected.border.top = colors["gray"]

    # Foreground color of the selected completion item.
    # Type: QtColor
    c.colors.completion.item.selected.fg = colors["fg"]

    # Foreground color of the matched text in the completion.
    # Type: QssColor
    c.colors.completion.match.fg = colors["pink"]

    # Color of the scrollbar in completion view
    # Type: QssColor
    c.colors.completion.scrollbar.bg = colors["bg"]

    # Color of the scrollbar handle in completion view.
    # Type: QssColor
    c.colors.completion.scrollbar.fg = colors["pink"]

    # Background color for the download bar.
    # Type: QssColor
    c.colors.downloads.bar.bg = colors["bg"]

    # Background color for downloads with errors.
    # Type: QtColor
    c.colors.downloads.error.bg = colors["red"]

    # Foreground color for downloads with errors.
    # Type: QtColor
    c.colors.downloads.error.fg = colors["bg"]

    # Color gradient stop for download backgrounds.
    # Type: QtColor
    c.colors.downloads.stop.bg = colors["lightgray"]

    # Color gradient interpolation system for download backgrounds.
    # Type: ColorSystem
    # Valid values:
    #   - rgb: Interpolate in the RGB color system.
    #   - hsv: Interpolate in the HSV color system.
    #   - hsl: Interpolate in the HSL color system.
    #   - none: Don't show a gradient.
    c.colors.downloads.system.bg = "none"

    # Background color for hints. Note that you can use a `rgba(...)` value
    # for transparency.
    # Type: QssColor
    c.colors.hints.bg = colors["pink"]

    # Font color for hints.
    # Type: QssColor
    c.colors.hints.fg = colors["bg"]

    # Font color for the matched part of hints.
    # Type: QssColor
    c.colors.hints.match.fg = colors["fg"]

    # Background color of the keyhint widget.
    # Type: QssColor
    c.colors.keyhint.bg = colors["bg"]

    # Text color for the keyhint widget.
    # Type: QssColor
    c.colors.keyhint.fg = colors["fg"]

    # Highlight color for keys to complete the current keychain.
    # Type: QssColor
    c.colors.keyhint.suffix.fg = colors["pink"]

    # Background color of an error message.
    # Type: QssColor
    c.colors.messages.error.bg = colors["error"]

    # Border color of an error message.
    # Type: QssColor
    c.colors.messages.error.border = colors["error"]

    # Foreground color of an error message.
    # Type: QssColor
    c.colors.messages.error.fg = colors["bg"]

    # Background color of an info message.
    # Type: QssColor
    c.colors.messages.info.bg = colors["lightgray"]

    # Border color of an info message.
    # Type: QssColor
    c.colors.messages.info.border = colors["lightgray"]

    # Foreground color an info message.
    # Type: QssColor
    c.colors.messages.info.fg = colors["gray"]

    # Background color of a warning message.
    # Type: QssColor
    c.colors.messages.warning.bg = colors["lightgray"]

    # Border color of a warning message.
    # Type: QssColor
    c.colors.messages.warning.border = colors["orange"]

    # Foreground color a warning message.
    # Type: QssColor
    c.colors.messages.warning.fg = colors["gray"]

    # Background color for prompts.
    # Type: QssColor
    c.colors.prompts.bg = colors["bg"]

    # # Border used around UI elements in prompts.
    # # Type: String
    c.colors.prompts.border = "1px solid " + colors["gray"]

    # Foreground color for prompts.
    # Type: QssColor
    c.colors.prompts.fg = colors["fg"]

    # Background color for the selected item in filename prompts.
    # Type: QssColor
    c.colors.prompts.selected.bg = colors["pink"]

    # Background color of the statusbar in caret mode.
    # Type: QssColor
    c.colors.statusbar.caret.bg = colors["pink"]

    # Foreground color of the statusbar in caret mode.
    # Type: QssColor
    c.colors.statusbar.caret.fg = colors["gray"]

    # Background color of the statusbar in caret mode with a selection.
    # Type: QssColor
    c.colors.statusbar.caret.selection.bg = colors["bg"]

    # Foreground color of the statusbar in caret mode with a selection.
    # Type: QssColor
    c.colors.statusbar.caret.selection.fg = colors["gray"]

    # Background color of the statusbar in command mode.
    # Type: QssColor
    c.colors.statusbar.command.bg = colors["bg"]

    # Foreground color of the statusbar in command mode.
    # Type: QssColor
    c.colors.statusbar.command.fg = colors["fg"]

    # Background color of the statusbar in private browsing + command mode.
    # Type: QssColor
    c.colors.statusbar.command.private.bg = colors["bg"]

    # Foreground color of the statusbar in private browsing + command mode.
    # Type: QssColor
    c.colors.statusbar.command.private.fg = colors["gray"]

    # Background color of the statusbar in insert mode.
    # Type: QssColor
    c.colors.statusbar.insert.bg = colors["lightgray"]

    # Foreground color of the statusbar in insert mode.
    # Type: QssColor
    c.colors.statusbar.insert.fg = colors["fg"]

    # Background color of the statusbar.
    # Type: QssColor
    c.colors.statusbar.normal.bg = colors["bg"]

    # Foreground color of the statusbar.
    # Type: QssColor
    c.colors.statusbar.normal.fg = colors["gray"]

    # Background color of the statusbar in passthrough mode.
    # Type: QssColor
    c.colors.statusbar.passthrough.bg = colors["gray"]

    # Foreground color of the statusbar in passthrough mode.
    # Type: QssColor
    c.colors.statusbar.passthrough.fg = colors["bg"]

    # Background color of the statusbar in private browsing mode.
    # Type: QssColor
    c.colors.statusbar.private.bg = colors["bg"]

    # Foreground color of the statusbar in private browsing mode.
    # Type: QssColor
    c.colors.statusbar.private.fg = colors["fg"]

    # Background color of the progress bar.
    # Type: QssColor
    c.colors.statusbar.progress.bg = colors["gray"]

    # Foreground color of the URL in the statusbar on error.
    # Type: QssColor
    c.colors.statusbar.url.error.fg = colors["error"]

    # Default foreground color of the URL in the statusbar.
    # Type: QssColor
    c.colors.statusbar.url.fg = colors["gray"]

    # Foreground color of the URL in the statusbar for hovered links.
    # Type: QssColor
    c.colors.statusbar.url.hover.fg = colors["navy"]

    # Foreground color of the URL in the statusbar on successful load
    # (http).
    # Type: QssColor
    c.colors.statusbar.url.success.http.fg = colors["gray"]

    # Foreground color of the URL in the statusbar on successful load
    # (https).
    # Type: QssColor
    c.colors.statusbar.url.success.https.fg = colors["green"]

    # Foreground color of the URL in the statusbar when there's a warning.
    # Type: QssColor
    c.colors.statusbar.url.warn.fg = colors["orange"]

    # Background color of the tab bar.
    # Type: QtColor
    c.colors.tabs.bar.bg = colors["bg"]

    # Background color of unselected even tabs.
    # Type: QtColor
    c.colors.tabs.even.bg = colors["bg"]

    # Foreground color of unselected even tabs.
    # Type: QtColor
    c.colors.tabs.even.fg = colors["midgray"]

    # Color for the tab indicator on errors.
    # Type: QtColor
    c.colors.tabs.indicator.error = colors["error"]

    # Color gradient start for the tab indicator.
    # Type: QtColor
    # c.colors.tabs.indicator.start = colors['pink']

    # Color gradient end for the tab indicator.
    # Type: QtColor
    # c.colors.tabs.indicator.stop = colors['orange']

    # Color gradient interpolation system for the tab indicator.
    # Type: ColorSystem
    # Valid values:
    #   - rgb: Interpolate in the RGB color system.
    #   - hsv: Interpolate in the HSV color system.
    #   - hsl: Interpolate in the HSL color system.
    #   - none: Don't show a gradient.
    c.colors.tabs.indicator.system = "none"

    # Background color of unselected odd tabs.
    # Type: QtColor
    c.colors.tabs.odd.bg = colors["bg"]

    # Foreground color of unselected odd tabs.
    # Type: QtColor
    c.colors.tabs.odd.fg = colors["midgray"]

    # # Background color of selected even tabs.
    # # Type: QtColor
    c.colors.tabs.selected.even.bg = colors["lightgray"]

    # # Foreground color of selected even tabs.
    # # Type: QtColor
    c.colors.tabs.selected.even.fg = colors["fg"]

    # # Background color of selected odd tabs.
    # # Type: QtColor
    c.colors.tabs.selected.odd.bg = colors["lightgray"]

    # # Foreground color of selected odd tabs.
    # # Type: QtColor
    c.colors.tabs.selected.odd.fg = colors["fg"]

    # Background color for webpages if unset (or empty to use the theme's
    # color)
    # Type: QtColor
    # c.colors.webpage.bg = colors["bg"]
