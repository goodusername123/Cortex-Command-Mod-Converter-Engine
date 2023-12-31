import os
import subprocess
from pathlib import Path

import old_python_implementation
from old_python_implementation import utils


class WronglyFormattedLuaFile(Exception):
    pass


def stylize(output_folder_path):
    if os.name == "nt":  # If the OS is Windows
        executable_location = "windows/stylua.exe"
    elif os.name == "posix":  # If the OS is Linux
        executable_location = "linux/stylua"

    stylua_path = (
        Path(old_python_implementation.__path__[0])
        / "stylua_executables"
        / executable_location
    )
    # Setting stdin to subprocess.DEVNULL is necessary for the EXE not to throw "OSError: [WinError 6] The handle is invalid"
    result = subprocess.run(
        [stylua_path, output_folder_path],
        capture_output=True,
        text=True,
        stdin=subprocess.DEVNULL,
    )

    if result.stderr:
        raise WronglyFormattedLuaFile(result.stderr)
