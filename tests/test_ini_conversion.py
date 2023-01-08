import shutil
import tempfile
import unittest
from pathlib import Path

from cortex_command_mod_converter_engine import convert
from cortex_command_mod_converter_engine.ini_converting.ini_cst import TooManyTabs
from cortex_command_mod_converter_engine.ini_converting.ini_tokenizer import (
    UnclosedMultilineComment,
)


class ShouldHaveRaisedException(Exception):
    pass


class TestINIConversion(unittest.TestCase):
    CONVERSION_TESTS_PATH = Path("tests/ini_test_files")

    def test_conversions(self):
        for in_test_path, out_test_path in self.get_filepaths():
            with self.subTest(msg=in_test_path):
                with tempfile.TemporaryDirectory() as temporary_directory_string:
                    self.conversion_test(
                        Path(temporary_directory_string), in_test_path, out_test_path
                    )

    def conversion_test(self, temporary_directory_path, in_test_path, out_test_path):
        original_test_directory_path = in_test_path.parent
        mod_name = original_test_directory_path.with_suffix(".rte").name

        converted_test_name_path = temporary_directory_path / "input_copy" / mod_name
        shutil.copytree(original_test_directory_path, converted_test_name_path)

        test_name = original_test_directory_path.name

        try:
            convert.convert(converted_test_name_path, temporary_directory_path)
        except TooManyTabs as exception:
            if test_name == "too_many_tabs":
                return
            raise exception
        except UnclosedMultilineComment as exception:
            if test_name == "unclosed_multiline_comment":
                return
            raise exception

        if test_name == "too_many_tabs" or test_name == "unclosed_multiline_comment":
            raise ShouldHaveRaisedException(test_name)

        converted_in_path = temporary_directory_path / mod_name / in_test_path.name

        self.assertEqual(converted_in_path.read_text(), out_test_path.read_text())

    def get_filepaths(self):
        return (
            (in_test_path, self.get_out_filepath(in_test_path))
            for in_test_path in self.get_in_test_paths()
        )

    def get_in_test_paths(self):
        return (
            entry_path
            for entry_path in self.CONVERSION_TESTS_PATH.rglob("in.ini")
            if entry_path.is_file()
        )

    def get_out_filepath(self, in_test_path):
        return in_test_path.with_stem("out")
