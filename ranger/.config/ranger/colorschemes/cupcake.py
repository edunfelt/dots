from ranger.gui.colorscheme import ColorScheme
from ranger.gui.color import *

class cupcake(ColorScheme):
    progress_bar_color = 2

    def use(self, context):
        fg, bg, attr = default_colors

        if context.reset:
            return default_colors

        elif context.in_browser:
            if context.selected:
                attr = reverse
            else:
                attr = normal
            if context.empty or context.error:
                fg = 0
                bg = 1
            if context.border:
                fg = 11
            if context.image:
                fg = 6
            if context.video:
                fg = 6
            if context.audio:
                fg = 4
            if context.document:
                fg = 7
            if context.container:
                attr |= bold
                fg = 1
            if context.directory:
                attr |= bold
                fg = 4
            elif context.executable and not \
                    any((context.media, context.container,
                       context.fifo, context.socket)):
                attr |= bold
                fg = 3
            if context.socket:
                attr |= bold
                fg = 5
            if context.fifo or context.device:
                fg = 4
                if context.device:
                    attr |= bold
            if context.link:
                fg = context.good and 0 or 7
            if context.bad:
                fg = 9
            if context.tag_marker and not context.selected:
                attr |= bold
                if fg == 7:
                    fg = 1
                else:
                    fg = 9
            if not context.selected and (context.cut or context.copied):
                fg = 1
                bg = 11
            if context.main_column:
                if context.selected:
                    attr |= bold
                if context.marked:
                    attr |= bold
                    fg = 8
            if context.badinfo:
                if attr & reverse:
                    bg = 1
                else:
                    fg = 7

        elif context.in_titlebar:
            attr |= bold
            if context.hostname:
                fg = context.bad and 0 or 3
                bg = 12
            elif context.directory:
                fg = 10
            elif context.tab:
                if context.good:
                    fg = 6
            elif context.link:
                fg = 4

        elif context.in_statusbar:
            if context.permissions:
                if context.good:
                    fg = 6
                    bg = 0
                elif context.bad:
                    fg = 1
            if context.marked:
                attr |= bold | reverse
                fg = 0
            if context.message:
                if context.bad:
                    fg = 1
            if context.loaded:
                bg = self.progress_bar_color
            if context.vcsinfo:
                fg = red
                attr &= ~bold
            if context.vcscommit:
                fg = 10
                attr &= ~bold


        if context.text:
            if context.highlight:
                attr |= reverse

        if context.in_taskview:
            if context.title:
                fg = 2

            if context.selected:
                attr |= reverse

            if context.loaded:
                if context.selected:
                    fg = self.progress_bar_color
                else:
                    bg = self.progress_bar_color


        if context.vcsfile and not context.selected:
            attr &= ~bold
            if context.vcsconflict:
                fg = 3
            elif context.vcschanged:
                fg = 1
            elif context.vcsunknown:
                fg = 7
            elif context.vcsstaged:
                fg = 4
            elif context.vcssync:
                fg = 5
            elif context.vcsignored:
                fg = 8

        elif context.vcsremote and not context.selected:
            attr &= ~bold
            if context.vcssync:
                fg = 5
            elif context.vcsbehind:
                fg = 13
            elif context.vcsahead:
                fg = 1
            elif context.vcsdiverged:
                fg = 3
            elif context.vcsunknown:
                fg = 7

        return fg, bg, attr
