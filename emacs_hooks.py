from qutebrowser.api import message
from qutebrowser.config import config as conf
from qutebrowser.keyinput import modeman
from qutebrowser.misc import objects
from qutebrowser.utils import objreg
from qutebrowser.mainwindow.statusbar import bar

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

    def on_url_changed(self, url):
        url = url.toString()
        win_id = objreg.last_visible_window().win_id
        self.server.send_signal("url-changed", {"win-id": win_id, "url": url})

    def on_search(self, search):
        self.server.send_signal("got-search", search)

    def on_enter_mode(self, mode):
        self.server.send_signal("entered-mode", str(mode))

    def on_leave_mode(self, mode):
        self.server.send_signal("left-mode", str(mode))

    def enable_local_hooks(self, window=None):
        if not window:
            window = objreg.last_visible_window()
        if not hasattr(window, "hooks_initialized"):
            mode_manager = modeman.instance(window.win_id)
            tabbed_browser = window.tabbed_browser
            status = window.status
            mode_manager.entered.connect(self.on_enter_mode)
            mode_manager.left.connect(self.on_leave_mode)
            tabbed_browser.cur_url_changed.connect(self.on_url_changed)
            status.cmd.got_search.connect(self.on_search)
            window.hooks_initialized = True

    def on_new(self, window):
        self.server.send_signal("new-window", str(window.win_id))
        self.enable_local_hooks(window)


# Local Variables:
# eval: (qutebrowser-config-mode)
# End:
