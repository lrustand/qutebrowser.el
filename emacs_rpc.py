# SPDX-License-Identifier: GPL-3.0-or-later

"""RPC server for Emacs.

This provides a two-way communication channel between Qutebrowser and
Emacs, and implements a RPC scheme for executing Python code inside
the context of the running Qutebrowser instance.
"""

from PyQt6.QtCore import QByteArray, pyqtSlot
from qutebrowser.api import message
from qutebrowser.keyinput import modeman
from qutebrowser.misc.ipc import IPCServer
from qutebrowser.utils import objreg
from qutebrowser import app
import json
from tempfile import mkstemp
import os
import inspect
import traceback


class rpcmethod():
    """Decorator for registering an RPC method.

    Register a function as an RPC method in the "rpcmethods"
    dictionary in objreg. The functions are then exposed over JSON-RPC
    by the RPC server and can be called remotely using the JSON-RPC
    protocol.

    Any parameters given in "params" in the JSON-RPC request are
    mapped onto the correspondingly named keyword arguments of the
    function.
    """

    def __init__(self, method):
        self.method = method
        self.name = method.__name__.lower().replace('_', '-')

        signature = inspect.signature(method)
        self.params_info = {}

        # Get the parameter names and default values
        for name, param in signature.parameters.items():
            if name != 'self':
                default = param.default is not inspect.Parameter.empty
                self.params_info[name] = default

        # Register the method in objreg
        rpcmethods = objreg.get("rpcmethods", {})
        rpcmethods[self.name] = self
        objreg.register("rpcmethods",
                        obj=rpcmethods,
                        update=True)

    def __call__(self, **params):
        return self.method(**params)


@rpcmethod
def EVAL(code):
    """Evaluate or execute code.

    Will first try to evaluate as an expression to return a value.
    If this is not possible, instead try to execute it as a statement
    or multi-line code.
    """
    try:
        return str(eval(code))
    except SyntaxError:
        exec(code, globals())
        return ""


@rpcmethod
def command(commands):
    """Run interactive commands.

    Args:
        commands: A list of commands to run.
    """

    if isinstance(commands, str):
        commands = [commands]

    app.process_pos_args(commands, via_ipc=True)
    return True


@rpcmethod
def get_window_info():
    """Get a list of window information.

    Return a list of window information for each window in
    Qutebrowser. Each item in the list is a dictionary containing
    the following information:

    Keys:
        win-id: The X11 window ID.
        url: The currently visited URL in the window.
        title: The title of the window.
        icon-file: A temp file containing the favicon.
        search: The current search text.
        hover: The currently hovered link.
        private: If the window is private.
        mode: The KeyMode of the window.
    """

    window_info_list = []

    for window in objreg.window_registry.values():
        tabbed_browser = window.tabbed_browser
        mode_manager = modeman.instance(window.win_id)
        win_id = int(window.winId())

        try:
            # This can fail if the url is empty
            url = tabbed_browser.current_url().toString()
        except Exception:
            url = None

        title = window.windowTitle()
        fd, icon_file = mkstemp(prefix="qutebrowser-favicon-",
                                suffix=".png")
        os.close(fd)
        tabbed_browser.windowIcon().pixmap(16, 16).save(icon_file)
        search = tabbed_browser.search_text
        hover = None  # FIXME
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


class EmacsRPCServer(IPCServer):
    """RPC server for Emacs."""

    def __init__(self):
        """Initialize RPC server instance."""
        old_server = objreg.get("emacs-rpc", None)
        if old_server:
            message.info("Shutting down old server")
            old_server.shutdown()

        super().__init__("/tmp/emacs-rpc")
        objreg.register(name="emacs-rpc",
                        obj=self,
                        update=True)
        self.listen()

    @pyqtSlot()
    def on_timeout(self):
        """Disable the timeout function of IPCServer."""
        # this needs to be adjusted
        # message.debug("Ignoring timeout")
        return

    def call_method(self, method, params):
        """Call an RPC method registered with @rpcmethod.

        Used to dispatch RPC calls to the correct function. Functions can be
        registered as an RPC method by decorating them with the @rpcmethod
        decorator.

        The function will be called with **kwargs corresponding to the params
        received from the JSON-RPC request.

        Args:
            method: The method to call. Names a function decorated with @rpcmethod.
            params: The parameters to the method, given in the params
                    field of the JSON-RPC message. Should be a dictionary
                    containing parameter names and values and will be mapped
                    onto the function parameters of the method.
        """

        rpcmethods = objreg.get("rpcmethods", {})
        method = rpcmethods.get(method, None)

        if not params:
            params = {}

        if not method:
            raise Exception(f"Unknown RPC method {method}")

        return method(**params)

    def _handle_data(self, data):
        """Handle data received from Emacs.

        Override the _handle_data function from the superclass IPCServer.

        If the data is not valid JSON data we simply ignore it.  The
        jsonrpc-process-connection implementation in Emacs sends some
        HTTP headers before the actual JSON data, but we don't care
        about those.

        Any valid JSON data that is not a valid JSON-RPC message is
        also ignored.

        Exceptions are catched and sent back as a JSON-RPC error
        response if a response is expected, otherwise they are ignored.

        Args:
            data: The data received from Emacs.
        """
        try:
            json_data = json.loads(data.decode("utf-8"))

            method = json_data.get("method", None)
            params = json_data.get("params", {})
            result = json_data.get("result", None)
            error = json_data.get("error", None)
            req_id = json_data.get("id", None)

            if "jsonrpc" not in json_data:
                raise Exception("Invalid JSON-RPC message: {json_data}")

            # Requests MUST contain both "method" and "id"
            if (method is not None) and (req_id is not None):
                message_type = "request"
                self.handle_request(method, params, req_id)

            # Notifications contain "method" but not "id"
            elif method is not None:
                message_type = "request"
                self.handle_notification(method, params)

            # Result responses MUST contain "result" but NOT "error"
            elif (result is not None) and (error is None):
                message_type = "result"
                self.handle_result(result, req_id)

            # Error responses MUST contain "error" but NOT "result"
            elif (error is not None) and (result is None):
                message_type = "error"
                self.handle_error(error, req_id)

            # Anything else is not a valid JSON-RPC message
            else:
                message_type = "invalid"
                raise Exception(f"Invalid JSON-RPC message: {json_data}")

        # Ignore non-JSON data
        except json.JSONDecodeError:
            return

        except Exception as err:
            # Only respond to requests
            if message_type in ("request", "invalid"):
                self.send_error(req_id=req_id,
                                message=f"{err.__class__.__name__}: {err}",
                                data={"traceback": traceback.format_exc()})

    def handle_notification(self, method, params):
        """Handle a received notification."""
        self.call_method(method, params)

    def handle_request(self, method, params, req_id):
        """Handle a received request."""
        result = self.call_method(method, params)
        self.send_result(result, req_id)

    def handle_result(self, result, req_id):
        """Handle a received response."""
        # TODO: Implement
        pass

    def handle_error(self, error, req_id):
        """Handle a received error response."""
        # TODO: Implement
        pass

    def send_error(self, code=-1, message=None, data=None, req_id=None):
        """Send an error response to a received request."""

        error = {"code": code,
                 "message": message}

        if data is not None:
            error["data"] = data

        self.send_data({"id": req_id, "error": error})

    def send_result(self, result, req_id=None):
        """Send a result response to a received request."""
        self.send_data({"id": req_id,
                        "result": result})

    def send_notification(self, method, params={}):
        """Send a JSON-RPC notification.

        Does not expect a response.
        """
        self.send_data({"method": method,
                        "params": params})

    def send_data(self, data):
        """Send data over JSON-RPC.

        The data is prepended with some HTTP style headers as expected
        by the jsonrpc-process-connection implementation in Emacs.

        The "jsonrpc" key is added with a value of "2.0" if it is not
        already present in the data.

        Args:
            data: JSON-serializable data to send.
        """
        data["jsonrpc"] = data.get("jsonrpc", "2.0")
        json_data = json.dumps(data)
        message = f"Content-Length: {len(json_data)}\r\n\r\n{json_data}\r\n"
        socket = self._get_socket()

        if socket and socket.isOpen():
            socket.write(QByteArray(message.encode("utf-8")))
            socket.flush()


if __name__ == "config":
    EmacsRPCServer()


# Local Variables:
# eval: (qutebrowser-config-mode)
# End:
