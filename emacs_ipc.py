from PyQt6.QtNetwork import QLocalSocket
from PyQt6.QtCore import QTextStream
from qutebrowser.api import message
from qutebrowser.keyinput import modeman
from qutebrowser.misc import objects
from qutebrowser.misc.ipc import IPCServer
from qutebrowser.utils import objreg

import json
from subprocess import run

class EmacsIPCServer(IPCServer):
    def __init__(self, socketname):
        super().__init__(socketname)
        objreg.register(name = "emacs-ipc-server",
                        obj = self,
                        update = True)
        self.listen()
    def _handle_data(self, data):
        json_data = json.loads(data.decode("utf-8"))
        match json_data["cmd"]:
            case "exec":
                exec(json_data["args"])
                response = "Ran commands\n"
            case "eval":
                try:
                    response = str(eval(json_data["args"])) + "\n"
                except Exception as e:
                    response = str(e) + "\n"
                except:
                    response = "Error!\n"
            case "keepalive":
                response = ""
            case _:
                response = "UNKNOWN!\n"
        socket = self._get_socket()
        stream = QTextStream(socket)
        stream << response
        stream.flush()
        socket.waitForBytesWritten(1000)

class EmacsSignalHandler:
    def __init__(self):
        objreg.register(name = "emacs-signal-handler",
                        obj = self,
                        update = True)
        try:
            # Enable the local hooks on startup in the current window
            self.enable_local_hooks(objreg.last_visible_window())
            # Fails if run during startup, qapp not initialized yet
            objects.qapp.new_window.connect(self.on_new)
        except:
            pass

    def send_to_emacs(self, msg):
        data = json.dumps(msg)
        socket = QLocalSocket()
        socket.connectToServer('/tmp/emacs-ipc-server')
        if socket.waitForConnected():
            socket.write(data.encode())
            socket.flush()
            #if socket.waitForReadyRead():
            #    response = socket.readAll()
            #    message.info(response.data().decode())
            socket.disconnectFromServer()

    def on_enter_mode(self, mode):
        self.send_to_emacs({"signal": "entered-mode",
                            "args": str(mode)})

    def on_leave_mode(self, mode):
        self.send_to_emacs({"signal": "left-mode",
                            "args": str(mode)})

    def enable_local_hooks (self, window):
        mode_manager = modeman.instance(window.win_id)
        mode_manager.entered.connect(self.on_enter_mode)
        mode_manager.left.connect(self.on_leave_mode)

    def on_new(self, window):
        self.send_to_emacs({"signal": "new-window",
                            "args": str(window.win_id)})
        self.enable_local_hooks(window)

def init():
    server = EmacsIPCServer("/tmp/emacs-ipc")
    signal_handler = EmacsSignalHandler()

init()

# Local Variables:
# eval: (qutebrowser-config-mode)
# End:
