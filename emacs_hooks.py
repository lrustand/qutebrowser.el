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
    forwards them to Emacs.
    """
    # TODO: Delete old favicon tempfiles on init
    def __init__(self, server=None):
        self.server = objreg.get("emacs-ipc", server)
        if not server:
            from emacs_ipc import EmacsIPCServer
            self.server = EmacsIPCServer(self)
        objreg.register(name = "emacs-hook-manager",
                        obj = self,
                        update = True)
        # Get the window list
        try:
            window_registry = objreg.window_registry
        except:
            window_registry = {}

        # Enable the window hooks on startup
        if len(window_registry) > 0:
            for window in objreg.window_registry.values():
                self.enable_window_hooks(window)
        else:
            message.info("No window found, not enabling window hooks.")
        # Enable new window hook
        if objects.qapp:
            objects.qapp.new_window.connect(self.on_new_window)

    def on_url_changed(self, window, url):
        """Called when the URL changed.

        Args:
            window: The window that the change happened in.
            url: The new URL.
        """
        url = url.toString()
        window_id = int(window.winId())
        self.send_signal("url-changed", {"win-id": window_id, "url": url})

    def on_link_hovered(self, window, url):
        """Called when a link is hover or unhovered.

        Args:
            window: The window that the link was hovered in.
            url: The URL of the link that was hovered. If the event
                 was an unhovering, the URL is ''.
        """
        window_id = int(window.winId())
        self.send_signal("link-hovered", {"win-id": window_id, "hover": url})

    def on_icon_changed(self, tab):
        """Called when the favicon changes.

        Saves the new favicon to a tempfile and sends the filename and
        window ID to Emacs.

        Args:
            tab: The tab that the favicon changed in.
        """
        window = tab.window()
        window_id = int(window.winId())
        fd, path = mkstemp(suffix=".png", prefix="qutebrowser-favicon-")
        os.close(fd)
        window.windowIcon().pixmap(16,16).save(path)
        self.send_signal("icon-changed", {"win-id": window_id, "icon-file": path})

    def on_search(self, window, search):
        """Called when a search is performed.

        Args:
            window: The window that search was performed in.
            search: The entered search term.
        """
        window_id = int(window.winId())
        self.send_signal("got-search", {"win-id": window_id, "search": search})

    def on_enter_mode(self, window, mode):
        """Called when a mode is entered.

        Args:
            window: The window that entered a mode.
            mode: The mode that was entered.
        """
        window_id = int(window.winId())
        self.send_signal("entered-mode", {"win-id": window_id, "mode": str(mode)})

    def on_leave_mode(self, window, mode):
        """Called when a mode is left.

        Args:
            window: The window that left a mode.
            mode: The mode that was left.
        """
        window_id = int(window.winId())
        self.send_signal("left-mode", {"win-id": window_id,
                                       "left-mode": str(mode),
                                       "mode": "KeyMode.normal"})

    def enable_tab_hooks(self, tab, _):
        """Enable tab local hooks.

        Args:
            tab: The tab to enable hooks for.
        """
        tab.icon_changed.connect(partial(self.on_icon_changed, tab))

    def enable_window_hooks(self, window=None):
        """Enable window local hooks.

        Args:
            window: The window to enable hooks for.
                    If unspecified, use the last visible window.
        """
        if not window:
            window = objreg.last_visible_window()
        if not hasattr(window, "hooks_initialized"):
            mode_manager = modeman.instance(window.win_id)
            tabbed_browser = window.tabbed_browser
            status = window.status

            mode_manager.entered.connect(partial(self.on_enter_mode, window))
            mode_manager.left.connect(partial(self.on_leave_mode, window))
            tabbed_browser.cur_url_changed.connect(partial(self.on_url_changed, window))
            tabbed_browser.cur_link_hovered.connect(partial(self.on_link_hovered, window))
            tabbed_browser.new_tab.connect(self.enable_tab_hooks)
            status.cmd.got_search.connect(partial(self.on_search, window))
            window.hooks_initialized = True

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
        self.server.send_data({"signal": signal, "args": args})


# Local Variables:
# eval: (qutebrowser-config-mode)
# End:
