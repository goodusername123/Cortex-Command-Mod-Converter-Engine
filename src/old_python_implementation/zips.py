import os
import shutil
import zipfile

import old_python_implementation.cfg as cfg


def unzip(input_mod_path, input_folder_path):
    with zipfile.ZipFile(input_mod_path) as item:
        item.extractall(input_folder_path)
        extracted_name = item.namelist()[0]
    os.remove(input_mod_path)
    return extracted_name[:-1]


def zip(input_mod_name, output_folder_path):
    print("Zipping '{}'".format(input_mod_name))

    output_mod_zip_name = input_mod_name.replace(
        ".rte", f"-{cfg.SUPPORTED_GAME_VERSION}-v1.0.rte"
    )
    shutil.make_archive(
        output_folder_path / output_mod_zip_name,
        "zip",
        root_dir=output_folder_path,
        base_dir=input_mod_name,
    )

    output_mod_path = output_folder_path / input_mod_name
    shutil.rmtree(output_mod_path)
