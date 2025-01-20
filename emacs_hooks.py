# SPDX-License-Identifier: GPL-3.0-or-later

"""Manager for Emacs hooks."""

from tempfile import mkstemp
import os

from qutebrowser.api import message
from qutebrowser.browser.browsertab import AbstractTab
from qutebrowser.keyinput import modeman
from qutebrowser.mainwindow.mainwindow import MainWindow
from qutebrowser.misc import objects
from qutebrowser.utils import objreg, usertypes
from qutebrowser.qt.core import QUrl, pyqtSlot


class EmacsHookManager:
    """Manager for Emacs hooks.

    Subscribes to a set of Qt signals exposed by Qutebrowser and
    forwards them to Emacs. Communication is done through the central
    EmacsRPCServer instance registered in objreg.
    """

    # TODO: Delete old favicon tempfiles on init
    def __init__(self) -> None:

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
            if old_manager is not None:
                try:
                    objects.qapp.new_window.disconnect(old_manager.on_new_window)
                # Previous manager was not connected
                except TypeError:
                    pass
            objects.qapp.new_window.connect(self.on_new_window)

    #@pyqtSlot(AbstractTab, int)
    def enable_tab_hooks(self, tab: AbstractTab, _: int) -> None:
        """Enable tab local hooks.

        Args:
            tab: The tab to enable hooks for.
        """

        def send_tab_signal(signal: str, data: dict) -> None:
            window: MainWindow = tab.window()
            window_x11_id: int = int(window.winId())

            tab_data = {"x11-win-id": window_x11_id,
                        "win-id": window.win_id}

            self.send_signal(signal, tab_data | data)

        @pyqtSlot()
        def on_icon_changed() -> None:
            """Called when the favicon changes.

            Saves the new favicon to a tempfile and sends the filename and
            X11 window ID to Emacs.
            """
            fd, path = mkstemp(prefix="qutebrowser-favicon-", suffix=".png")
            os.close(fd)
            window: MainWindow = tab.window()
            window.windowIcon().pixmap(16, 16).save(path)

            send_tab_signal("icon-changed", {"icon-file": path})

        @pyqtSlot()
        def on_recently_audible_changed() -> None:
            """Called when the audible state changes."""
            recently_audible = tab.audio.is_recently_audible()

            send_tab_signal("recently-audible-changed",
                            {"recently-audible": recently_audible})

        tab.icon_changed.connect(on_icon_changed)
        tab.audio.recently_audible_changed.connect(on_recently_audible_changed)

    def enable_window_hooks(self, window: MainWindow) -> None:
        """Enable window local hooks.

        Args:
            window: The window to enable hooks for.
        """
        mode_manager = modeman.instance(window.win_id)
        tabbed_browser = window.tabbed_browser
        status = window.status

        # We use this function to wrap the window data, since the X11
        # window ID changes sometime after the window is createad,
        # probably because EXWM "reparents" it.
        def send_window_signal(signal: str, data: dict = {}) -> None:
            window_data = {"x11-win-id": int(window.winId()),
                           "win-id": window.win_id}
            self.send_signal(signal, window_data | data)


        def on_url_changed(url: QUrl) -> None:
            """Called when the URL changed.

            Args:
                url: The new URL.
            """
            send_window_signal("url-changed", {"url": url.toString()})

        @pyqtSlot(str)
        def on_link_hovered(url: str) -> None:
            """Called when a link is hover or unhovered.

            Args:
                url: The URL of the link that was hovered. If the event
                     was an unhovering, the URL is ''.
            """
            send_window_signal("link-hovered", {"hover": url})

        @pyqtSlot()
        def on_load_started() -> None:
            """Called when starting to load a new webpage."""
            send_window_signal("load-started")

        @pyqtSlot()
        def on_load_finished() -> None:
            """Called when finished loading a new webpage."""
            send_window_signal("load-finished")

        @pyqtSlot(usertypes.KeyMode)
        def on_enter_mode(mode: usertypes.KeyMode) -> None:
            """Called when a mode is entered.

            Args:
                mode: The mode that was entered.
            """
            send_window_signal("entered-mode", {"mode": str(mode)})

        @pyqtSlot(usertypes.KeyMode)
        def on_leave_mode(mode: usertypes.KeyMode) -> None:
            """Called when a mode is left.

            Args:
                left-mode: The mode that was left.
                mode: The new mode.
            """
            send_window_signal("left-mode", {"left-mode": str(mode),
                                             "mode": "KeyMode.normal"})

        @pyqtSlot(int, int)
        def on_scroll_perc_changed(x_perc: int,
                                   y_perc: int) -> None:
            """Called when the scroll position changes.

            Args:
                x_perc: X scroll position in percentage.
                y_perc: Y scroll position in percentage.
            """
            send_window_signal("scroll-perc-changed", {"x-scroll-perc": x_perc,
                                                       "y-scroll-perc": y_perc})

        @pyqtSlot(str)
        def on_search(search: str) -> None:
            """Called when a search is performed.

            Args:
                search: The entered search term.
            """
            send_window_signal("got-search", {"search": search})

        tabbed_browser.new_tab.connect(self.enable_tab_hooks)
        tabbed_browser.cur_url_changed.connect(on_url_changed)
        tabbed_browser.cur_link_hovered.connect(on_link_hovered)
        tabbed_browser.cur_load_started.connect(on_load_started)
        tabbed_browser.cur_load_finished.connect(on_load_finished)
        tabbed_browser.cur_scroll_perc_changed.connect(on_scroll_perc_changed)
        mode_manager.entered.connect(on_enter_mode)
        mode_manager.left.connect(on_leave_mode)
        status.cmd.got_search.connect(on_search)

    #@pyqtSlot(MainWindow)
    def on_new_window(self, window: MainWindow) -> None:
        """Called when a new window is created.

        Responsible for setting up all window local hooks in new windows.

        Args:
            window: The window that was created.
        """
        self.send_signal("new-window", {"x11-win-id": int(window.winId()),
                                        "win-id": window.win_id})
        self.enable_window_hooks(window)

    def send_signal(self, signal: str, args: dict[str, object] = {}) -> None:
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
