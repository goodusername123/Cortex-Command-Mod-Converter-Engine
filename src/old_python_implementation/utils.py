import os
from pathlib import Path

import old_python_implementation


def get_relative_subfolder(input_folder_path, input_subfolder_path):
    return os.path.relpath(
        input_subfolder_path,
        os.path.join(input_folder_path, os.pardir)
        if input_folder_path.endswith(".rte")
        else input_folder_path,
    )


def is_mod_folder(entry):
    return entry.is_dir() and entry.suffix == ".rte"


def is_mod_folder_or_subfolder(path):
    path_parts = Path(path).parts
    # If it is a folder inside of the input folder and if it is the mod folder or is inside of it.
    return len(path_parts) >= 1 and path_parts[0].endswith(".rte")


def get_ini_files_in_dir_deep(path):
    count = 0
    for name in os.listdir(path):
        p = path / Path(name)
        if p.is_file() and p.suffix == ".ini" and p.stem != "desktop":
            count += 1
        elif p.is_dir():
            count += get_ini_files_in_dir_deep(p)

    return count


def get_path(relative_path):
    return Path(old_python_implementation.__file__).parent / relative_path
