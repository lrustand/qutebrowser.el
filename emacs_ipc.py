# SPDX-License-Identifier: GPL-3.0-or-later

"""IPC server for Emacs."""

from PyQt6.QtCore import QByteArray, pyqtSlot
from qutebrowser.api import message
from qutebrowser.keyinput import modeman
from qutebrowser.misc import objects
from qutebrowser.misc.ipc import IPCServer
from qutebrowser.utils import objreg
import json
from tempfile import mkstemp
import os


class EmacsIPCServer(IPCServer):
    """IPC server for Emacs."""
    def __init__(self, hook_manager=None):
        super().__init__("/tmp/emacs-ipc")
        objreg.register(name = "emacs-ipc",
                        obj = self,
                        update = True)
        self.hook_manager = objreg.get("emacs-hook-manager", hook_manager)
        if not self.hook_manager:
            from emacs_hooks import EmacsHookManager
            self.hook_manager = EmacsHookManager(self)
        self.listen()

    @pyqtSlot()
    def on_timeout(self):
        # this needs to be adjusted
        #message.debug("Ignoring timeout")
        return

    def _handle_data(self, data):
        """Handle data received from Emacs.

        Args:
            data: The data received from Emacs.
        """
        json_data = json.loads(data.decode("utf-8"))
        if "eval" in json_data:
            try:
                response = str(eval(json_data["eval"]))
            except Exception as e:
                response = str(e)
            except:
                response = "Error!"
            self.send_data({"rpc-response": response})
        if "request" in json_data:
            self.handle_request(json_data["request"], json_data.get("args", {}))
        if "repl" in json_data:
            try:
                response = str(eval(json_data["repl"]))
            except Exception as e:
                response = str(e)
            except:
                response = "Error!"
            self.send_data({"repl-response": response})

    def handle_request(self, request, args={}):
        """Handle a request for data.

        Currently implemented endpoint is 'window-info', that returns
        a dictionary containing all information about a window.
        """
        if request == "window-info":
            for window in objreg.window_registry.values():
                tabbed_browser = window.tabbed_browser
                mode_manager = modeman.instance(window.win_id)
                win_id = int(window.winId())
                try:
                    # This can fail if the url is empty
                    url = tabbed_browser.current_url().toString()
                except:
                    url = None
                title = window.windowTitle()
                fd, icon_file = mkstemp(suffix=".png", prefix="qutebrowser-favicon-")
                os.close(fd)
                tabbed_browser.windowIcon().pixmap(16,16).save(icon_file)
                search = tabbed_browser.search_text
                hover = None # FIXME
                mode = str(mode_manager.mode)
                self.send_data({"window-info": {"win-id": win_id,
                                                "url": url,
                                                "title": title,
                                                "icon-file": icon_file,
                                                "search": search,
                                                "hover": hover,
                                                "mode": mode}})

    def send_data(self, data):
        """Send data to Emacs.

        The data is JSON-encoded before sending over the socket to
        Emacs. A comma is appended to the end of the JSON string in
        case multiple JSON messages are received at once.

        Args:
            data: The data to send. This should be a dictionary, which
                  Emacs will receive as an alist.

        """
        json_data = json.dumps(data) + ","
        socket = self._get_socket()
        if socket and socket.isOpen():
            socket.write(QByteArray(json_data.encode("utf-8")))
            socket.flush()

    def send_cmd(self, cmd):
        """Send a command to be evaluated in Emacs.

        Args:
            cmd: The command(s) to be executed in Emacs.
        """
        self.send_data({"eval": cmd})


# Fails if run during startup, qapp not initialized yet
if objects.qapp:
    server = objreg.get("emacs-ipc", None)
    if not server:
        message.info("No IPC server found. Starting one.")
        server = EmacsIPCServer()


# Local Variables:
# eval: (qutebrowser-config-mode)
# End:
