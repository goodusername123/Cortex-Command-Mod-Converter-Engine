import os
from pathlib import Path

from old_python_implementation import utils
from old_python_implementation.ini_converting import ini_cst, ini_tokenizer


def get_full_cst(input_folder_path, output_folder_path, subfolder_path):
    """
    The "cst" in this function's name stands for Concrete Syntax Tree. Returns the tree root and the number of ini files.
    """
    parsed_portion = {}

    for name in os.listdir(subfolder_path):
        p = subfolder_path / Path(name)
        relative_subfolder = utils.get_relative_subfolder(input_folder_path, str(p))

        if (
            not utils.is_mod_folder_or_subfolder(relative_subfolder)
            or name == "invalid_tabbing.ini"
        ):  # TODO: Remove this once CCCP has a Mods folder that can be iterated over.
            continue
        elif (
            p.is_file() and p.suffix == ".ini" and p.stem != "desktop"
        ):  # Skips the desktop.ini Windows metadata file.
            output_file_path = output_folder_path / Path(relative_subfolder)
            tokens = ini_tokenizer.get_tokens(output_file_path)
            parsed_portion[name] = ini_cst.get_cst(tokens)
        elif p.is_dir():
            cst = get_full_cst(input_folder_path, output_folder_path, str(p))
            parsed_portion[name] = cst

    return parsed_portion
