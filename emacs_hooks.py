from qutebrowser.api import message
from qutebrowser.config import config as conf
from qutebrowser.keyinput import modeman
from qutebrowser.misc import objects
from qutebrowser.utils import objreg
from qutebrowser.mainwindow.statusbar import bar
from functools import partial

class EmacsHookManager:
    def __init__(self, server=None):
        self.server = objreg.get("emacs-ipc", server)
        if not server:
            from emacs_ipc import EmacsIPCServer
            self.server = EmacsIPCServer(self)
        objreg.register(name = "emacs-hook-manager",
                        obj = self,
                        update = True)
        # Get current window
        try:
            window = objreg.last_visible_window()
        except:
            window = None

        # Enable the local hooks on startup
        if window:
            hook_manager = objreg.get("emacs-hook-manager", None)
            if not hook_manager:
                hook_manager = EmacsHookManager()
            for window in objreg.window_registry.values():
                hook_manager.enable_local_hooks(window)
        else:
            message.info("No window found, not enabling local hooks.")
        # Enable new window hook
        if objects.qapp:
            objects.qapp.new_window.connect(self.on_new)

    def on_url_changed(self, window, url):
        url = url.toString()
        window_id = int(window.winId())
        self.server.send_signal("url-changed", {"win-id": window_id, "url": url})

    def on_link_hovered(self, window, url):
        window_id = int(window.winId())
        self.server.send_signal("link-hovered", {"win-id": window_id, "url": url})

    def on_search(self, window, search):
        window_id = int(window.winId())
        self.server.send_signal("got-search", {"win-id": window_id, "search": search})

    def on_enter_mode(self, window, mode):
        window_id = int(window.winId())
        self.server.send_signal("entered-mode", {"win-id": window_id, "mode": str(mode)})

    def on_leave_mode(self, window, mode):
        window_id = int(window.winId())
        self.server.send_signal("left-mode", {"win-id": window_id, "mode": str(mode)})

    def enable_local_hooks(self, window=None):
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
            status.cmd.got_search.connect(partial(self.on_search, window))
            window.hooks_initialized = True

    def on_new(self, window):
        self.server.send_signal("new-window", int(window.winId()))
        self.enable_local_hooks(window)


# Local Variables:
# eval: (qutebrowser-config-mode)
# End:
