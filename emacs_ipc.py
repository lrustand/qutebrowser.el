# SPDX-License-Identifier: GPL-3.0-or-later

"""IPC server for Emacs."""

from PyQt6.QtCore import QByteArray, pyqtSlot
from qutebrowser.api import message, cmdutils
from qutebrowser.keyinput import modeman
from qutebrowser.misc import objects
from qutebrowser.misc.ipc import IPCServer
from qutebrowser.utils import objreg
import json
from tempfile import mkstemp
import os


class EmacsIPCServer(IPCServer):
    """IPC server for Emacs."""

    def __init__(self):
        old_server = objreg.get("emacs-ipc", None)
        if old_server:
            message.info("Shutting down old server")
            old_server.shutdown()

        super().__init__("/tmp/emacs-ipc")
        objreg.register(name = "emacs-ipc",
                        obj = self,
                        update = True)
        self.listen()

    @pyqtSlot()
    def on_timeout(self):
        # this needs to be adjusted
        #message.debug("Ignoring timeout")
        return

    def eval_or_exec(self, code):
        """Evaluate or execute code."""
        try:
            try:
                return str(eval(code))
            except SyntaxError:
                exec(code, globals())
                return ""
        except Exception as e:
            return str(e)
        except:
            return "Error!"

    def _handle_data(self, data):
        """Handle data received from Emacs.

        Args:
            data: The data received from Emacs.
        """

        try:
            json_data = json.loads(data.decode("utf-8"))
        except:
            json_data = {}

        if "jsonrpc" in json_data:
            method = json_data.get("method")
            params = json_data.get("params",[])
            req_id = json_data.get("id", None)

            match method:
                case "window-info":
                    result = self.get_window_info()
                case "eval":
                    result = self.eval_or_exec(params["code"])
                case "repl":
                    result = self.eval_or_exec(params["input"])
                case _:
                    result = f"ERROR: Unknown method {method}"

            self.send_data({"result": result,
                            "id": req_id})

    def send_notification(self, method, params={}):
        self.send_data({"method": method,
                        "params": params})

    def send_data(self, data):
        data["jsonrpc"] = "2.0"
        json_data = json.dumps(data)
        message = f"Content-Length: {len(json_data)}\r\n\r\n{json_data}\r\n"
        socket = self._get_socket()
        if socket and socket.isOpen():
            socket.write(QByteArray(message.encode("utf-8")))
            socket.flush()

    def get_window_info(self):

        window_info_list = []

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
            window_info = {"win-id": win_id,
                           "url": url,
                           "title": title,
                           "icon-file": icon_file,
                           "search": search,
                           "hover": hover,
                           "private": window.is_private,
                           "mode": mode}

            window_info_list.append(window_info)
        return window_info_list

    # FIXME: :emacs command temporarily disabled because it causes problems when redefined
    #@cmdutils.register(instance="emacs-ipc", maxsplit=0, name="emacs")
    # FIXME: Implement in jsonrpc
    #def send_cmd(self, cmd: str) -> None:
    #    """Send a command to be evaluated in Emacs.

    #    Args:
    #        cmd: The command(s) to be executed in Emacs.
    #    """
    #    self.send_data({"eval": cmd})


EmacsIPCServer()


# Local Variables:
# eval: (qutebrowser-config-mode)
# End:
