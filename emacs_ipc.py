from PyQt6.QtNetwork import QLocalSocket
from PyQt6.QtCore import Qt, QTextStream, QByteArray, pyqtSlot
from qutebrowser.api import message, cmdutils
from qutebrowser.keyinput import modeman
from qutebrowser.misc import objects
from qutebrowser.misc.ipc import IPCServer
from qutebrowser.utils import objreg
import json


class EmacsIPCServer(IPCServer):
    def __init__(self, hook_manager=None):
        super().__init__("/tmp/emacs-ipc")
        objreg.register(name = "emacs-ipc",
                        obj = self,
                        update = True)
        self.listen()
        self.hook_manager = objreg.get("emacs-hook-manager", hook_manager)
        if not self.hook_manager:
            from emacs_hooks import EmacsHookManager
            self.hook_manager = EmacsHookManager(self)

    @pyqtSlot()
    def on_timeout(self):
        # this needs to be adjusted
        #message.debug("Ignoring timeout")
        return

    def _handle_data(self, data):
        json_data = json.loads(data.decode("utf-8"))
        if "eval" in json_data:
            try:
                response = str(eval(json_data["eval"]))
            except Exception as e:
                response = str(e)
            except:
                response = "Error!"
        socket = self._get_socket()
        response = json.dumps({"response": response}) + "\n"
        socket.write(QByteArray(response.encode("utf-8")))
        socket.flush()

    def send_signal(self, signal, args=[]):
        data = json.dumps({"signal": signal,
                           "args": args}) + ","
        socket = self._get_socket()
        if socket and socket.isOpen():
            socket.write(QByteArray(data.encode("utf-8")))
            socket.flush()

    def send_cmd(self, cmd, args=[]):
        data = json.dumps({"eval": cmd}) + ","
        socket = self._get_socket()
        if socket and socket.isOpen():
            socket.write(QByteArray(data.encode("utf-8")))
            socket.flush()


# Fails if run during startup, qapp not initialized yet
if objects.qapp:
    #objects.qapp.new_window.connect(on_new)
    server = objreg.get("emacs-ipc", None)
    if not server:
        message.info("No IPC server found. Starting one.")
        server = EmacsIPCServer()
else:
    message.info("No qapp, no new window hooks.")


# Local Variables:
# eval: (qutebrowser-config-mode)
# End:
