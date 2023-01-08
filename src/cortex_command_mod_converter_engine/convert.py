import json
import os
import shutil
import zipfile
from pathlib import Path

from cortex_command_mod_converter_engine import (
    bmp_to_png,
    regex_rules,
    stylua,
    utils,
    zips,
)
from cortex_command_mod_converter_engine.case_check import case_check
from cortex_command_mod_converter_engine.ini_converting import (
    ini_cst_builder,
    ini_rules,
    ini_writer,
)


def convert(
    input_mod_path,
    output_folder_path,
    beautify_lua=True,
    output_zip=False,
    skip_conversion=False,
    remove_input_mod_path=False,
):
    input_mod_path = Path(input_mod_path)
    input_folder_path = str(input_mod_path.parent)

    if zipfile.is_zipfile(input_mod_path):
        input_mod_name = zips.unzip(input_mod_path, input_folder_path)
        input_mod_path = input_mod_path.with_name(input_mod_name)
    else:
        input_mod_name = input_mod_path.name

    output_mod_path = Path(output_folder_path) / input_mod_name

    case_check.init_glob(output_folder_path, input_folder_path)

    conversion_rules = get_conversion_rules()

    converter_walk(
        input_mod_path,
        input_folder_path,
        output_folder_path,
        conversion_rules,
        skip_conversion,
    )

    if beautify_lua:
        stylua.stylize(output_mod_path)

    ini_cst = ini_cst_builder.get_full_cst(
        input_folder_path, output_folder_path, input_mod_path
    )
    ini_rules.apply_rules_on_ini_cst(ini_cst, output_folder_path)
    ini_writer.write_converted_ini_cst(ini_cst, output_mod_path)

    if output_zip:
        zips.zip(input_mod_name, Path(output_folder_path))

    if remove_input_mod_path:
        shutil.rmtree(input_mod_path)


def get_conversion_rules():
    conversion_rules = {}

    for folder_path, _, subfiles in os.walk("conversion_rules"):
        for filename in subfiles:
            p = folder_path / Path(filename)

            if p.is_file() and p.suffix.lower() == ".json":
                with open(p) as f:
                    conversion_rules.update(json.load(f))

    return conversion_rules


def converter_walk(
    input_mod_path,
    input_folder_path,
    output_folder_path,
    conversion_rules,
    skip_conversion,
):
    for input_subfolder_path, _input_subfolders, input_subfiles in os.walk(
        input_mod_path
    ):
        relative_subfolder = utils.get_relative_subfolder(
            str(input_mod_path), input_subfolder_path
        )

        if utils.is_mod_folder_or_subfolder(relative_subfolder):
            output_subfolder = os.path.join(output_folder_path, relative_subfolder)
            Path(output_subfolder).mkdir(exist_ok=True)
            process_files(
                input_subfiles,
                input_subfolder_path,
                output_subfolder,
                input_folder_path,
                conversion_rules,
                skip_conversion,
            )


def process_files(
    input_subfiles,
    input_subfolder_path,
    output_subfolder,
    input_folder_path,
    conversion_rules,
    skip_conversion,
):
    for full_filename in input_subfiles:
        filename, file_extension = os.path.splitext(
            full_filename
        )  # TODO: Use pathlib instead here
        file_extension = file_extension.lower()

        input_file_path = os.path.join(input_subfolder_path, full_filename)

        output_file_path = os.path.join(output_subfolder, filename + file_extension)

        if bmp_to_png.is_bmp(full_filename):
            if not skip_conversion:
                bmp_to_png.bmp_to_png(
                    input_file_path, Path(output_file_path).with_suffix(".png")
                )
            else:
                shutil.copyfile(input_file_path, output_file_path)

        if (
            full_filename == "desktop.ini" or full_filename == "Thumbs.db"
        ):  # Skip this Windows metadata file.
            continue

        if file_extension in (".ini", ".lua"):
            create_converted_file(
                input_file_path,
                output_file_path,
                input_folder_path,
                conversion_rules,
                skip_conversion,
            )
        elif not bmp_to_png.is_bmp(full_filename):
            shutil.copyfile(input_file_path, output_file_path)


def create_converted_file(
    input_file_path,
    output_file_path,
    input_folder_path,
    conversion_rules,
    skip_conversion,
):
    # try: # TODO: Figure out why this try/except is necessary and why it doesn't check for an error type.
    with open(
        input_file_path, "r", errors="ignore"
    ) as file_in:  # TODO: Why ignore errors?
        with open(output_file_path, "w") as file_out:
            all_lines = ""
            file_path = os.path.relpath(input_file_path, input_folder_path)

            line_number = 0
            for line in file_in:
                line_number += 1

                line = bmp_to_png.change_bmp_to_png_name(line, skip_conversion)

                # line = lua_parser.convert(line)

                regex_rules.playsound_warning(line, file_path, line_number)

                all_lines += line

            # Conversion rules can contain newlines, so they can't be applied on a per-line basis.
            all_lines = apply_conversion_rules(
                conversion_rules, all_lines, skip_conversion
            )

            # Case matching must be done after conversion, otherwise tons of errors wil be generated
            # all_lines = case_check.case_check(all_lines, input_file_path, output_file_path)

            if not skip_conversion:
                all_lines = regex_rules.regex_replace(all_lines)

            # Case matching must be done after conversion, otherwise tons of errors wil be generated
            all_lines = case_check.case_check(
                all_lines, input_file_path, output_file_path
            )

            file_out.write(all_lines)
    # except:
    # 	shutil.copyfile(input_file_path, output_file_path)


def apply_conversion_rules(conversion_rules, all_lines, skip_conversion):
    if not skip_conversion:
        for old_str, new_str in conversion_rules.items():
            old_str_parts = os.path.splitext(old_str)
            # Because bmp -> png already happened on all_lines we'll make all old_str conversion rules png.
            if old_str_parts[1] == ".bmp":
                all_lines = all_lines.replace(old_str_parts[0] + ".png", new_str)
            else:
                all_lines = all_lines.replace(old_str, new_str)
    return all_lines


def pluralize(word, count):
    return word + "s" if count != 1 else word
