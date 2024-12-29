# SPDX-License-Identifier: GPL-3.0-or-later

"""RPC server for Emacs.

This provides a two-way communication channel between Qutebrowser and
Emacs, and implements a RPC scheme for executing Python code inside
the context of the running Qutebrowser instance.
"""

from PyQt6.QtCore import QByteArray, pyqtSlot
from qutebrowser.api import message, cmdutils
from qutebrowser.commands import runners
from qutebrowser.keyinput import modeman
from qutebrowser.misc import objects
from qutebrowser.misc.ipc import IPCServer
from qutebrowser.utils import objreg
import json
from tempfile import mkstemp
import os
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

    def __init__(self):
        pass

    def __call__(self, func):

        name = func.__name__.lower().replace('_', '-')
        rpcmethods = objreg.get("rpcmethods", None)

        if rpcmethods is not None:
            rpcmethods.update({name: func})
        else:
            rpcmethods = {name: func}
            objreg.register("rpcmethods",
                            obj=rpcmethods)
        return func





def get_tabs(window):
    """Get a list of tab objects belonging to window."""
    tab_registry = objreg._get_window_registry(window.win_id)["tab-registry"]
    tabs = list(tab_registry.values())
    return tabs


def get_window(win_id):
    """Return window."""
    return objreg.window_registry.get(win_id)


def find_window(x11_win_id):
    """Find window from X11 window ID."""

    for win in objreg.window_registry.values():
        if x11_win_id == int(win.winId()):
            return win

    raise Exception(f"Could not find window with X11 ID {x11_win_id}")


@rpcmethod()
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


@rpcmethod()
def command(commands, win_id=None, count=None):
    """Run interactive commands.

    Args:
        commands: A list of commands to run.
        win_id: The window to run the commands in.
        count: The count to pass to the command.
    """

    if win_id is None:
        win_id = objreg.last_visible_window().win_id

    if not isinstance(commands, (list, tuple)):
        commands = [commands]

    commandrunner = runners.CommandRunner(win_id)

    for cmd in commands:
        # We don't use run_safely, since we want to send any
        # exceptions back to Emacs instead of displaying them in
        # Qutebrowser.
        commandrunner.run(cmd[1:], count)

    return True


@rpcmethod()
def x11_win_id_to_win_id(x11_win_id):
    """Return the win_id corresponding to x11_win_id."""
    return find_window(x11_win_id).win_id


@rpcmethod()
def win_id_to_x11_win_id(win_id):
    """Return the x11_win_id corresponding to win_id."""
    return get_window(win_id).winId()


@rpcmethod()
def get_window_url(win_id):
    """Return the URL of window."""
    window = get_window(win_id)
    tabbed_browser = window.tabbed_browser
    try:
        url = tabbed_browser.current_url().toString()
    except Exception:
        url = None
    return url


@rpcmethod()
def get_window_title(win_id):
    """Return the title of window."""
    window = get_window(win_id)
    return window.windowTitle()


@rpcmethod()
def get_window_icon(win_id):
    """Return the favicon of window."""
    window = get_window(win_id)
    tabbed_browser = window.tabbed_browser
    fd, icon_file = mkstemp(prefix="qutebrowser-favicon-",
                            suffix=".png")
    os.close(fd)
    tabbed_browser.windowIcon().pixmap(16, 16).save(icon_file)

    return icon_file


@rpcmethod()
def get_window_search(win_id):
    """Return the search term of window."""
    window = get_window(win_id)
    tabbed_browser = window.tabbed_browser

    return tabbed_browser.search_text


@rpcmethod()
def is_window_private(win_id):
    """Return a boolean indicating the private status of window."""
    window = get_window(win_id)

    return window.is_private


@rpcmethod()
def get_window_mode(win_id):
    """Return the mode of window."""
    mode_manager = modeman.instance(win_id)

    return str(mode_manager.mode)


@rpcmethod()
def is_window_audible(win_id):
    """Return a boolean indicating the audible status of window."""
    window = get_window(win_id)

    for tab in get_tabs(window):
        if tab.audio.is_recently_audible():
            return True

    return False


@rpcmethod()
def get_window_scroll(win_id):
    """Return the X and Y scroll percentages of window."""
    window = get_window(win_id)
    tab = window.tabbed_browser.widget.currentWidget()
    x_scroll, y_scroll = tab.scroller.pos_perc()
    return (x_scroll, y_scroll)


def get_command_arguments(command):
    cmd = objects.commands.get(command)
    args = []
    keywords = []
    for arg, name in cmd.pos_args:
        desc = cmd.docparser.arg_descs.get(arg)
        args.append({"name": name,
                     "description": desc})

    for arg, flags in cmd.opt_args.items():
        long = flags[0]
        name = long[2:]
        desc = cmd.docparser.arg_descs.get(arg)
        keywords.append({"name": name,
                         "description": desc})

    return {"arguments": args,
            "keywords": keywords}


@rpcmethod()
def list_commands():
    """Return a list of Qutebrowser commands."""

    response = []

    for name, cmd in objects.commands.items():
        description = cmd.docparser.short_desc

        if cmd.docparser.long_desc:
            description += "\n" + cmd.docparser.long_desc

        takes_count = cmd.docparser.arg_descs.get("count")

        cmd_info = {"command": name,
                    "description": description,
                    "takes-count": takes_count}

        cmd_info.update(get_command_arguments(name))
        response.append(cmd_info)

    return response


@rpcmethod()
def list_rpc_methods():
    """Return a list of RPC methods."""
    return list(objreg.get("rpcmethods", {}).keys())


@rpcmethod()
def get_window_info():
    """Get a list of window information.

    Return a list of window information for each window in
    Qutebrowser. Each item in the list is a dictionary containing
    the following information:

    Keys:
        x11-win-id: The X11 window ID.
        win-id: The internal window ID.
        url: The currently visited URL in the window.
        title: The title of the window.
        icon-file: A temp file containing the favicon.
        search: The current search text.
        hover: The currently hovered link.
        private: If the window is private.
        mode: The KeyMode of the window.
        recently-audible: The audible status of the window.
        x-scroll-perc: The scroll percentage in the x direction.
        y-scroll-perc: The scroll percentage in the y direction.
    """

    window_info_list = []

    for window in objreg.window_registry.values():
        tabbed_browser = window.tabbed_browser
        mode_manager = modeman.instance(window.win_id)
        x11_win_id = int(window.winId())
        win_id = window.win_id

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
        recently_audible = is_window_audible(win_id)
        x_scroll, y_scroll = get_window_scroll(win_id)

        window_info = {"x11-win-id": x11_win_id,
                       "win-id": win_id,
                       "url": url,
                       "title": title,
                       "icon-file": icon_file,
                       "search": search,
                       "hover": hover,
                       "private": window.is_private,
                       "mode": mode,
                       "recently-audible": recently_audible,
                       "x-scroll-perc": x_scroll,
                       "y-scroll-perc": y_scroll}

        window_info_list.append(window_info)
    return window_info_list


class EmacsRPCServer(IPCServer):
    """RPC server for Emacs."""

    def __init__(self):
        """Initialize RPC server instance."""

        self._req_id = 0
        self.continuations = {}
        old_server = objreg.get("emacs-rpc", None)

        if old_server:
            message.info("Shutting down old server")
            old_server.shutdown()
            self._req_id = getattr(old_server, "_req_id", 0)
            self.continuations = getattr(old_server, "continuations", {})

        super().__init__("/tmp/emacs-rpc")
        objreg.register(name="emacs-rpc",
                        obj=self,
                        update=True)
        self.listen()

    def _request_id(self):
        self._req_id += 1
        return self._req_id

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
        function = rpcmethods.get(method)

        if not function:
            raise Exception(f"Unknown RPC method {method}")

        if isinstance(params, dict):
            # Convert parameter names
            converted_params = {}
            for key, value in params.items():
                key = key.lower().replace("-", "_")
                converted_params[key] = value
            return function(**converted_params)

        elif isinstance(params, (list, tuple)):
            return function(*params)

        elif params is None:
            return function()

        else:
            return function(params)

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
            params = json_data.get("params", None)
            result = json_data.get("result", None)
            error = json_data.get("error", None)
            req_id = json_data.get("id", None)

            if "jsonrpc" not in json_data:
                raise Exception(f"Invalid JSON-RPC message: {json_data}")

            # Result responses MUST contain "result" but NOT "error"
            if (result is not None) and (error is None):
                message_type = "result"
                self.handle_result(result, req_id)

            # Error responses MUST contain "error" but NOT "result"
            elif (error is not None) and (result is None):
                message_type = "error"
                self.handle_error(error, req_id)

            # Requests MUST contain both "method" and "id"
            elif (method is not None) and (req_id is not None):
                message_type = "request"
                self.handle_request(method, params, req_id)

            # Notifications contain "method" but not "id"
            elif method is not None:
                message_type = "request"
                self.handle_notification(method, params)

            # Anything else is not a valid JSON-RPC message
            else:
                message_type = "invalid"
                raise Exception(f"Invalid JSON-RPC message: {json_data}")

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
        cont = self.continuations.pop(req_id, None)

        kwargs = {"result": result,
                  "id": req_id}

        if cont is not None:
            cont[0](**kwargs)

    def handle_error(self, error, req_id):
        """Handle a received error response."""
        cont = self.continuations.pop(req_id, None)

        kwargs = {"error": error,
                  "id": req_id}

        if cont is not None:
            cont[1](**kwargs)

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

    def send_notification(self, method, params=None):
        """Send a JSON-RPC notification.

        Does not expect a response.
        """
        data = {"method": method}

        if params is not None:
            data["params"] = params

        self.send_data(data)

    def send_request(self,
                     method,
                     params=None,
                     result_callback=None,
                     error_callback=None):
        """Send a JSON-RPC request.
        """
        req_id = self._request_id()
        data = {"id": req_id,
                "method": method}

        if params is not None:
            data["params"] = params

        if (result_callback is not None) or (error_callback is not None):
            self.continuations[req_id] = (result_callback, error_callback)

        self.send_data(data)

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


def print_result(result, id):
    """Print the result of a request as an info message."""
    message.info(f"Result args: {result}")


class redefinable_command(cmdutils.register):

    def __call__(self, func: cmdutils._CmdHandlerFunc) -> cmdutils._CmdHandlerType:
        if self._name is None:
            name = func.__name__.lower().replace('_', '-')
        else:
            assert isinstance(self._name, str), self._name
            name = self._name
        objects.commands.pop(name, None)
        super().__call__(func)


@redefinable_command()
def emacs(code, print=False):
    """Evaluate lisp code in Emacs.

    Args:
        code: The code to evaluate.
        print: Whether to show the result in an info message.
    """

    callback = print_result if print else None
    objreg.get("emacs-rpc").send_request("eval", {"code": code},
                                         result_callback=callback)


if __name__ == "config":
    EmacsRPCServer()


# Local Variables:
# eval: (qutebrowser-config-mode)
# End:
