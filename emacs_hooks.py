# SPDX-License-Identifier: GPL-3.0-or-later

"""Manager for Emacs hooks."""

from qutebrowser.api import message
from qutebrowser.keyinput import modeman
from qutebrowser.misc import objects
from qutebrowser.utils import objreg
from functools import partial
from tempfile import mkstemp
import os


class EmacsHookManager:
    """Manager for Emacs hooks.

    Subscribes to a set of Qt signals exposed by Qutebrowser and
    forwards them to Emacs. Communication is done through the central
    EmacsRPCServer instance registered in objreg.
    """

    # TODO: Delete old favicon tempfiles on init
    def __init__(self):

        old_manager = objreg.get("emacs-hook-manager", None)

        objreg.register(name="emacs-hook-manager",
                        obj=self,
                        update=True)

        # Enable the window and tab hooks on startup
        if objreg.window_registry:
            for window in objreg.window_registry.values():
                self.enable_window_hooks(window)
                for tab in window.tabbed_browser.widgets():
                    self.enable_tab_hooks(tab, None)

        # Enable new window hook
        if objects.qapp:
            if old_manager:
                objects.qapp.new_window.disconnect(old_manager.on_new_window)
            objects.qapp.new_window.connect(self.on_new_window)

    def on_url_changed(self, window, url):
        """Called when the URL changed.

        Args:
            window: The window that the change happened in.
            url: The new URL.
        """
        url = url.toString()
        window_id = int(window.winId())

        data = {"win-id": window_id,
                "url": url}

        self.send_signal("url-changed", data)

    def on_link_hovered(self, window, url):
        """Called when a link is hover or unhovered.

        Args:
            window: The window that the link was hovered in.
            url: The URL of the link that was hovered. If the event
                 was an unhovering, the URL is ''.
        """
        window_id = int(window.winId())

        data = {"win-id": window_id,
                "hover": url}

        self.send_signal("link-hovered", data)

    def on_icon_changed(self, tab):
        """Called when the favicon changes.

        Saves the new favicon to a tempfile and sends the filename and
        X11 window ID to Emacs.

        Args:
            tab: The tab that the favicon changed in.
        """
        window = tab.window()
        window_id = int(window.winId())
        fd, path = mkstemp(prefix="qutebrowser-favicon-", suffix=".png")
        os.close(fd)
        window.windowIcon().pixmap(16, 16).save(path)

        data = {"win-id": window_id,
                "icon-file": path}

        self.send_signal("icon-changed", data)

    def on_scroll_perc_changed(self, window, x_perc, y_perc):
        """Called when the scroll position changes.

        Args:
            window: The window that the scroll position changed in.
            x_perc: X scroll position in percentage.
            y_perc: Y scroll position in percentage.
        """
        window_id = int(window.winId())

        data = {"win-id": window_id,
                "x-scroll-perc": x_perc,
                "y-scroll-perc": y_perc}

        self.send_signal("scroll-perc-changed", data)

    def on_recently_audible_changed(self, tab):
        """Called when the audible state changes.

        Args:
            tab: The tab that the audible state changed in.
        """
        window = tab.window()
        window_id = int(window.winId())
        recently_audible = tab.audio.is_recently_audible()

        data = {"win-id": window_id,
                "recently-audible": recently_audible}

        self.send_signal("recently-audible-changed", data)

    def on_search(self, window, search):
        """Called when a search is performed.

        Args:
            window: The window that search was performed in.
            search: The entered search term.
        """
        window_id = int(window.winId())

        data = {"win-id": window_id,
                "search": search}

        self.send_signal("got-search", data)

    def on_enter_mode(self, window, mode):
        """Called when a mode is entered.

        Args:
            window: The window that entered a mode.
            mode: The mode that was entered.
        """
        window_id = int(window.winId())

        data = {"win-id": window_id,
                "mode": str(mode)}

        self.send_signal("entered-mode", data)

    def on_leave_mode(self, window, mode):
        """Called when a mode is left.

        Args:
            window: The window that left a mode.
            mode: The mode that was left.
        """
        window_id = int(window.winId())

        data = {"win-id": window_id,
                "left-mode": str(mode),
                "mode": "KeyMode.normal"}

        self.send_signal("left-mode", data)

    def on_load_started(self, window):
        """Called when starting to load a new webpage.

        Args:
            window: The window that started loading.
        """
        window_id = int(window.winId())
        self.send_signal("load-started", {"win-id": window_id})

    def on_load_finished(self, window):
        """Called when finished loading a new webpage.

        Args:
            window: The window that finished loading.
        """
        window_id = int(window.winId())
        self.send_signal("load-finished", {"win-id": window_id})

    def enable_tab_hooks(self, tab, _):
        """Enable tab local hooks.

        Args:
            tab: The tab to enable hooks for.
        """
        tab.icon_changed.connect(partial(self.on_icon_changed, tab))
        tab.audio.recently_audible_changed.connect(
            partial(self.on_recently_audible_changed, tab))

    def enable_window_hooks(self, window):
        """Enable window local hooks.

        Args:
            window: The window to enable hooks for.
        """

        mode_manager = modeman.instance(window.win_id)
        tabbed_browser = window.tabbed_browser
        status = window.status

        mode_manager.entered.connect(partial(self.on_enter_mode, window))
        mode_manager.entered.connect(partial(self.on_enter_mode, window))
        mode_manager.left.connect(partial(self.on_leave_mode, window))
        tabbed_browser.cur_url_changed.connect(
            partial(self.on_url_changed, window))
        tabbed_browser.cur_link_hovered.connect(
            partial(self.on_link_hovered, window))
        tabbed_browser.new_tab.connect(self.enable_tab_hooks)
        tabbed_browser.cur_load_started.connect(
            partial(self.on_load_started, window))
        tabbed_browser.cur_load_finished.connect(
            partial(self.on_load_finished, window))
        tabbed_browser.cur_scroll_perc_changed.connect(
            partial(self.on_scroll_perc_changed, window))
        status.cmd.got_search.connect(partial(self.on_search, window))

    def on_new_window(self, window):
        """Called when a new window is created.

        Responsible for setting up all window local hooks in new windows.

        Args:
            window: The window that was created.
        """
        self.send_signal("new-window", {"win-id": int(window.winId())})
        self.enable_window_hooks(window)

    def send_signal(self, signal, args={}):
        """Send a signal to Emacs.

        Args:
            signal: The name of the signal to be sent.
            args: The arguments to pass to the signal handler in Emacs.
        """

        server = objreg.get("emacs-rpc", None)
        if server:
            server.send_notification(method=signal, params=args)
        else:
            message.info("No RPC server found! Could not send signal!")


if __name__ == "config":
    EmacsHookManager()

# Local Variables:
# eval: (qutebrowser-config-mode)
# End:
