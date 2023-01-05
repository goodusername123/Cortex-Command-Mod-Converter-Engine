import unittest
from pathlib import Path

from cortex_command_mod_converter_engine.ini_converting import (
    ini_cst,
    ini_rules,
    ini_tokenizer,
    ini_writer,
)


class TestINIConversion(unittest.TestCase):
    CONVERSION_TESTS_PATH = Path("tests/ini_test_files/conversion")

    def test_conversions(self):
        for in_filepath, out_filepath in self.get_filepaths():
            in_str = in_filepath.read_text()
            out_str = out_filepath.read_text()

            tokens = ini_tokenizer.get_tokens_from_str(in_str, in_filepath)
            cst = ini_cst.get_cst(tokens)
            ini_rules.apply_rules_on_sections([cst[0]], None)

            result = ini_writer.get_ini_cst_string(cst)
            self.assertEqual(result, out_str)

    def get_filepaths(self):
        return (
            (in_filepath, self.get_out_filepath(in_filepath))
            for in_filepath in self.get_in_filepaths()
        )

    def get_in_filepaths(self):
        return (
            entry_path
            for entry_path in self.CONVERSION_TESTS_PATH.rglob("in.ini")
            if entry_path.is_file()
        )

    def get_out_filepath(self, in_filepath):
        return in_filepath.with_stem("out")
