from qutebrowser.api import message
from qutebrowser.config import config as conf
from qutebrowser.keyinput import modeman
from qutebrowser.misc import objects
from qutebrowser.utils import objreg
from qutebrowser.mainwindow.statusbar import bar
from functools import partial
from tempfile import mkstemp
import os

class EmacsHookManager:
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
        url = url.toString()
        window_id = int(window.winId())
        self.server.send_signal("url-changed", {"win-id": window_id, "url": url})

    def on_link_hovered(self, window, url):
        window_id = int(window.winId())
        self.server.send_signal("link-hovered", {"win-id": window_id, "url": url})

    def on_icon_changed(self, tab):
        window = tab.window()
        window_id = int(window.winId())
        fd, path = mkstemp(suffix=".png", prefix="qutebrowser-favicon-")
        os.close(fd)
        window.windowIcon().pixmap(16,16).save(path)
        self.server.send_signal("icon-changed", {"win-id": window_id, "icon-file": path})

    def on_search(self, window, search):
        window_id = int(window.winId())
        self.server.send_signal("got-search", {"win-id": window_id, "search": search})

    def on_enter_mode(self, window, mode):
        window_id = int(window.winId())
        self.server.send_signal("entered-mode", {"win-id": window_id, "mode": str(mode)})

    def on_leave_mode(self, window, mode):
        window_id = int(window.winId())
        self.server.send_signal("left-mode", {"win-id": window_id, "mode": str(mode)})

    def enable_tab_hooks(self, tab, idx):
        tab.icon_changed.connect(partial(self.on_icon_changed, tab))

    def enable_window_hooks(self, window=None):
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
        self.server.send_signal("new-window", int(window.winId()))
        self.enable_window_hooks(window)


# Local Variables:
# eval: (qutebrowser-config-mode)
# End:
