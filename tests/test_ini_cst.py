import unittest

from cortex_command_mod_converter_engine.ini_converting import ini_cst, ini_tokenizer
from cortex_command_mod_converter_engine.ini_converting.ini_cst import TooManyTabs
from cortex_command_mod_converter_engine.ini_converting.ini_tokenizer import (
    UnclosedMultilineComment,
)
from tests.get_test_path_from_filename import get_test_path_from_filename


class TestINICST(unittest.TestCase):
    def test_comments(self):
        self.cst_test(
            "comments",
            [
                [
                    {"type": "extra", "content": "// foo"},
                    {"type": "extra", "content": "\n"},
                    {"type": "extra", "content": "/*a\nb\nc*/"},
                    {"type": "extra", "content": "\n"},
                ],
            ],
        )

    def test_complex(self):
        self.cst_test(
            "complex",
            [
                [
                    {"type": "extra", "content": "// foo"},
                    {"type": "extra", "content": "\n"},
                    {"type": "extra", "content": "/*a\nb\nc*/"},
                    {"type": "extra", "content": "\n"},
                    {"type": "property", "content": "AddEffect"},
                    {"type": "extra", "content": "  "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "MOPixel"},
                    {"type": "extra", "content": "//bar"},
                    {"type": "extra", "content": "\n"},
                    {
                        "type": "children",
                        "content": [
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "PresetName"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": "  "},
                                {"type": "value", "content": "red_dot_tiny"},
                                {"type": "extra", "content": "\n"},
                            ],
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "Mass"},
                                {"type": "extra", "content": "  "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": "  "},
                                {"type": "value", "content": "0.0"},
                                {"type": "extra", "content": "\n"},
                            ],
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "Xd"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "42"},
                                {"type": "extra", "content": "\n"},
                            ],
                        ],
                    },
                ]
            ],
        )

    def test_datamodule(self):
        self.cst_test(
            "datamodule",
            [
                [
                    {"type": "property", "content": "DataModule"},
                    {"type": "extra", "content": "\n"},
                    {
                        "type": "children",
                        "content": [
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "IconFile"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "ContentFile"},
                                {"type": "extra", "content": "\n"},
                                {
                                    "type": "children",
                                    "content": [
                                        [
                                            {"type": "extra", "content": "\t\t"},
                                            {"type": "property", "content": "FilePath"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "Foo"},
                                            {"type": "extra", "content": "\n"},
                                        ]
                                    ],
                                },
                            ],
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "ModuleName"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "Bar"},
                            ],
                        ],
                    },
                ]
            ],
        )

    def test_deindentation_1(self):
        self.cst_test(
            "deindentation_1",
            [
                [
                    {"type": "property", "content": "PresetName"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "Foo"},
                    {"type": "extra", "content": "\n"},
                    {
                        "type": "children",
                        "content": [
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "A1"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "X"},
                                {"type": "extra", "content": "\n\n"},
                            ],
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "A2"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "X"},
                                {"type": "extra", "content": "\n"},
                            ],
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "B1"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "X"},
                                {"type": "extra", "content": "\n"},
                                {"type": "extra", "content": "\n"},
                            ],
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "B2"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "X"},
                                {"type": "extra", "content": "\n"},
                            ],
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "C1"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "X"},
                                {"type": "extra", "content": "\n"},
                                {"type": "extra", "content": "//foo"},
                                {"type": "extra", "content": "\n"},
                            ],
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "C2"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "X"},
                            ],
                        ],
                    },
                ]
            ],
        )

    def test_deindentation_2(self):
        self.cst_test(
            "deindentation_2",
            [
                [
                    {"type": "property", "content": "AddEffect"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "MOPixel"},
                    {"type": "extra", "content": "\n"},
                    {
                        "type": "children",
                        "content": [
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "PresetName"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "Foo"},
                                {"type": "extra", "content": "\n"},
                                {
                                    "type": "children",
                                    "content": [
                                        [
                                            {"type": "extra", "content": "\t\t"},
                                            {"type": "property", "content": "A1"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "X"},
                                            {"type": "extra", "content": "\n\n"},
                                        ],
                                        [
                                            {"type": "extra", "content": "\t\t"},
                                            {"type": "property", "content": "A2"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "X"},
                                            {"type": "extra", "content": "\n"},
                                        ],
                                        [
                                            {"type": "extra", "content": "\t\t"},
                                            {"type": "property", "content": "B1"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "X"},
                                            {"type": "extra", "content": "\n"},
                                            {"type": "extra", "content": "\n"},
                                        ],
                                        [
                                            {"type": "extra", "content": "\t\t"},
                                            {"type": "property", "content": "B2"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "X"},
                                            {"type": "extra", "content": "\n"},
                                        ],
                                        [
                                            {"type": "extra", "content": "\t\t"},
                                            {"type": "property", "content": "C1"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "X"},
                                            {"type": "extra", "content": "\n"},
                                            {"type": "extra", "content": "//foo"},
                                            {"type": "extra", "content": "\n"},
                                        ],
                                        [
                                            {"type": "extra", "content": "\t\t"},
                                            {"type": "property", "content": "C2"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "X"},
                                        ],
                                    ],
                                },
                            ]
                        ],
                    },
                ]
            ],
        )

    def test_deindentation_3(self):
        self.cst_test(
            "deindentation_3",
            [
                [
                    {"type": "property", "content": "AddEffect"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "MOPixel"},
                    {"type": "extra", "content": "\n"},
                    {
                        "type": "children",
                        "content": [
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "PresetName"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "Foo"},
                                {"type": "extra", "content": "\n"},
                                {
                                    "type": "children",
                                    "content": [
                                        [
                                            {"type": "extra", "content": "\t\t"},
                                            {"type": "property", "content": "A1"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "X"},
                                            {"type": "extra", "content": "\n"},
                                            {"type": "extra", "content": "\t"},
                                            {"type": "extra", "content": "\n"},
                                        ],
                                        [
                                            {"type": "extra", "content": "\t\t"},
                                            {"type": "property", "content": "A2"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "X"},
                                            {"type": "extra", "content": "\n"},
                                        ],
                                        [
                                            {"type": "extra", "content": "\t\t"},
                                            {"type": "property", "content": "B1"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "X"},
                                            {"type": "extra", "content": "\n"},
                                            {"type": "extra", "content": "\t"},
                                            {"type": "extra", "content": "\n"},
                                        ],
                                        [
                                            {"type": "extra", "content": "\t\t"},
                                            {"type": "property", "content": "B2"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "X"},
                                            {"type": "extra", "content": "\n"},
                                        ],
                                        [
                                            {"type": "extra", "content": "\t\t"},
                                            {"type": "property", "content": "C1"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "X"},
                                            {"type": "extra", "content": "\n"},
                                            {"type": "extra", "content": "\t"},
                                            {"type": "extra", "content": "//foo"},
                                            {"type": "extra", "content": "\n"},
                                        ],
                                        [
                                            {"type": "extra", "content": "\t\t"},
                                            {"type": "property", "content": "C2"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "X"},
                                        ],
                                    ],
                                },
                            ]
                        ],
                    },
                ]
            ],
        )

    def test_include_files(self):
        self.cst_test(
            "include_files",
            [
                [
                    {"type": "property", "content": "IncludeFile"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "A.ini"},
                    {"type": "extra", "content": "\n\n"},
                ],
                [
                    {"type": "property", "content": "IncludeFile"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "B.ini"},
                    {"type": "extra", "content": "\n"},
                ],
            ],
        )

    def test_left_stripped_tab(self):
        self.cst_test(
            "left_stripped_tab",
            [
                [
                    {"type": "property", "content": "Foo"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "Bar"},
                ]
            ],
        )

    def test_multiline_comment_after_equals(self):
        self.cst_test(
            "multiline_comment_after_equals",
            [
                [
                    {"type": "property", "content": "A1"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "A2"},
                    {"type": "extra", "content": "\n"},
                    {
                        "type": "children",
                        "content": [
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "B1"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "B2"},
                                {"type": "extra", "content": "\n"},
                                {
                                    "type": "children",
                                    "content": [
                                        [
                                            {"type": "extra", "content": "\t\t"},
                                            {"type": "property", "content": "C1"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "C2"},
                                            {"type": "extra", "content": "\n"},
                                            {
                                                "type": "children",
                                                "content": [
                                                    [
                                                        {
                                                            "type": "extra",
                                                            "content": "\t\t\t",
                                                        },
                                                        {
                                                            "type": "property",
                                                            "content": "D1",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "=",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "/*foo*/",
                                                        },
                                                        {
                                                            "type": "value",
                                                            "content": "D2",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "\n",
                                                        },
                                                    ],
                                                    [
                                                        {
                                                            "type": "extra",
                                                            "content": "\t\t\t",
                                                        },
                                                        {
                                                            "type": "property",
                                                            "content": "E1",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "=",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "value",
                                                            "content": "E2",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "\n",
                                                        },
                                                    ],
                                                ],
                                            },
                                        ]
                                    ],
                                },
                            ]
                        ],
                    },
                ]
            ],
        )

    def test_multiline_comment_after_property(self):
        self.cst_test(
            "multiline_comment_after_property",
            [
                [
                    {"type": "property", "content": "A1"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "A2"},
                    {"type": "extra", "content": "\n"},
                    {
                        "type": "children",
                        "content": [
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "B1"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "B2"},
                                {"type": "extra", "content": "\n"},
                                {
                                    "type": "children",
                                    "content": [
                                        [
                                            {"type": "extra", "content": "\t\t"},
                                            {"type": "property", "content": "C1"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "C2"},
                                            {"type": "extra", "content": "\n"},
                                            {
                                                "type": "children",
                                                "content": [
                                                    [
                                                        {
                                                            "type": "extra",
                                                            "content": "\t\t\t",
                                                        },
                                                        {
                                                            "type": "property",
                                                            "content": "D1",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "/*foo*/",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "=",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "value",
                                                            "content": "D2",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "\n",
                                                        },
                                                    ],
                                                    [
                                                        {
                                                            "type": "extra",
                                                            "content": "\t\t\t",
                                                        },
                                                        {
                                                            "type": "property",
                                                            "content": "E1",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "=",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "value",
                                                            "content": "E2",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "\n",
                                                        },
                                                    ],
                                                ],
                                            },
                                        ]
                                    ],
                                },
                            ]
                        ],
                    },
                ]
            ],
        )

    def test_multiline_comment_after_tabs(self):
        self.cst_test(
            "multiline_comment_after_tabs",
            [
                [
                    {"type": "property", "content": "A1"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "A2"},
                    {"type": "extra", "content": "\n"},
                    {
                        "type": "children",
                        "content": [
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "B1"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "B2"},
                                {"type": "extra", "content": "\n"},
                                {
                                    "type": "children",
                                    "content": [
                                        [
                                            {"type": "extra", "content": "\t\t"},
                                            {"type": "property", "content": "C1"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "C2"},
                                            {"type": "extra", "content": "\n"},
                                            {
                                                "type": "children",
                                                "content": [
                                                    [
                                                        {
                                                            "type": "extra",
                                                            "content": "\t\t\t",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "/*foo*/",
                                                        },
                                                        {
                                                            "type": "property",
                                                            "content": "D1",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "=",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "value",
                                                            "content": "D2",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "\n",
                                                        },
                                                    ],
                                                    [
                                                        {
                                                            "type": "extra",
                                                            "content": "\t\t\t",
                                                        },
                                                        {
                                                            "type": "property",
                                                            "content": "E1",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "=",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "value",
                                                            "content": "E2",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "\n",
                                                        },
                                                    ],
                                                ],
                                            },
                                        ]
                                    ],
                                },
                            ]
                        ],
                    },
                ]
            ],
        )

    def test_multiline_comment_after_value(self):
        self.cst_test(
            "multiline_comment_after_value",
            [
                [
                    {"type": "property", "content": "A1"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "A2"},
                    {"type": "extra", "content": "\n"},
                    {
                        "type": "children",
                        "content": [
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "B1"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "B2"},
                                {"type": "extra", "content": "\n"},
                                {
                                    "type": "children",
                                    "content": [
                                        [
                                            {"type": "extra", "content": "\t\t"},
                                            {"type": "property", "content": "C1"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "C2"},
                                            {"type": "extra", "content": "\n"},
                                            {
                                                "type": "children",
                                                "content": [
                                                    [
                                                        {
                                                            "type": "extra",
                                                            "content": "\t\t\t",
                                                        },
                                                        {
                                                            "type": "property",
                                                            "content": "D1",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "=",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "value",
                                                            "content": "D2",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "/*foo*/",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "\n",
                                                        },
                                                    ],
                                                    [
                                                        {
                                                            "type": "extra",
                                                            "content": "\t\t\t",
                                                        },
                                                        {
                                                            "type": "property",
                                                            "content": "E1",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "=",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "value",
                                                            "content": "E2",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "\n",
                                                        },
                                                    ],
                                                ],
                                            },
                                        ]
                                    ],
                                },
                            ]
                        ],
                    },
                ]
            ],
        )

    def test_multiline_comment_before_tabs(self):
        self.cst_test(
            "multiline_comment_before_tabs",
            [
                [
                    {"type": "property", "content": "A1"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "A2"},
                    {"type": "extra", "content": "\n"},
                    {
                        "type": "children",
                        "content": [
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "B1"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "B2"},
                                {"type": "extra", "content": "\n"},
                                {
                                    "type": "children",
                                    "content": [
                                        [
                                            {"type": "extra", "content": "\t\t"},
                                            {"type": "property", "content": "C1"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "C2"},
                                            {"type": "extra", "content": "\n"},
                                            {
                                                "type": "children",
                                                "content": [
                                                    [
                                                        {
                                                            "type": "extra",
                                                            "content": "/*foo*/",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "\t\t\t",
                                                        },
                                                        {
                                                            "type": "property",
                                                            "content": "D1",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "=",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "value",
                                                            "content": "D2",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "\n",
                                                        },
                                                    ],
                                                    [
                                                        {
                                                            "type": "extra",
                                                            "content": "\t\t\t",
                                                        },
                                                        {
                                                            "type": "property",
                                                            "content": "E1",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "=",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "value",
                                                            "content": "E2",
                                                        },
                                                    ],
                                                ],
                                            },
                                        ]
                                    ],
                                },
                            ]
                        ],
                    },
                ]
            ],
        )

    def test_multiline_comment_between_multiline_tabs(self):
        self.cst_test(
            "multiline_comment_between_multiline_tabs",
            [
                [
                    {"type": "property", "content": "A1"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "A2"},
                    {"type": "extra", "content": "\n"},
                    {
                        "type": "children",
                        "content": [
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "B1"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "B2"},
                                {"type": "extra", "content": "\n"},
                                {
                                    "type": "children",
                                    "content": [
                                        [
                                            {"type": "extra", "content": "\t"},
                                            {"type": "extra", "content": "/*\n*/"},
                                            {"type": "extra", "content": "\t"},
                                            {"type": "property", "content": "C1"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "C2"},
                                            {"type": "extra", "content": "\n"},
                                        ]
                                    ],
                                },
                            ]
                        ],
                    },
                ]
            ],
        )

    def test_multiline_comment_between_tabs(self):
        self.cst_test(
            "multiline_comment_between_tabs",
            [
                [
                    {"type": "property", "content": "A1"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "A2"},
                    {"type": "extra", "content": "\n"},
                    {
                        "type": "children",
                        "content": [
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "B1"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "B2"},
                                {"type": "extra", "content": "\n"},
                                {
                                    "type": "children",
                                    "content": [
                                        [
                                            {"type": "extra", "content": "\t\t"},
                                            {"type": "property", "content": "C1"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "C2"},
                                            {"type": "extra", "content": "\n"},
                                            {
                                                "type": "children",
                                                "content": [
                                                    [
                                                        {
                                                            "type": "extra",
                                                            "content": "\t",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "/*foo*/",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "\t\t",
                                                        },
                                                        {
                                                            "type": "property",
                                                            "content": "D1",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "=",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "value",
                                                            "content": "D2",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "\n",
                                                        },
                                                    ],
                                                    [
                                                        {
                                                            "type": "extra",
                                                            "content": "\t\t\t",
                                                        },
                                                        {
                                                            "type": "property",
                                                            "content": "E1",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": "=",
                                                        },
                                                        {
                                                            "type": "extra",
                                                            "content": " ",
                                                        },
                                                        {
                                                            "type": "value",
                                                            "content": "E2",
                                                        },
                                                    ],
                                                ],
                                            },
                                        ]
                                    ],
                                },
                            ]
                        ],
                    },
                ]
            ],
        )

    def test_multiline_comments_with_space_in_between(self):
        self.cst_test(
            "multiline_comments_with_space_in_between",
            [
                [
                    {"type": "property", "content": "A"},
                    {"type": "extra", "content": "\n"},
                    {"type": "extra", "content": "/*foo*/"},
                    {"type": "extra", "content": "/*bar*/"},
                    {"type": "extra", "content": "\n"},
                ]
            ],
        )

    def test_multiple(self):
        self.cst_test(
            "multiple",
            [
                [
                    {"type": "property", "content": "Foo"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "Bar"},
                    {"type": "extra", "content": "\n"},
                    {
                        "type": "children",
                        "content": [
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "Baz"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "Bee"},
                                {"type": "extra", "content": "\n"},
                            ]
                        ],
                    },
                ],
                [
                    {"type": "property", "content": "A"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "B"},
                    {"type": "extra", "content": "\n"},
                    {
                        "type": "children",
                        "content": [
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "C"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "D"},
                                {"type": "extra", "content": "\n"},
                            ]
                        ],
                    },
                ],
            ],
        )

    def test_nested(self):
        self.cst_test(
            "nested",
            [
                [
                    {"type": "property", "content": "Foo"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "Bar"},
                    {"type": "extra", "content": "\n"},
                    {
                        "type": "children",
                        "content": [
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "Baz"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "Bee"},
                                {"type": "extra", "content": "\n"},
                            ]
                        ],
                    },
                ]
            ],
        )

    def test_object_and_property(self):
        self.cst_test(
            "object_and_property",
            [
                [
                    {"type": "property", "content": "Foo"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "Bar"},
                    {"type": "extra", "content": "\n"},
                    {
                        "type": "children",
                        "content": [
                            [
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "Baz"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "Bee"},
                                {"type": "extra", "content": "\n\n"},
                            ]
                        ],
                    },
                ],
                [
                    {"type": "property", "content": "IncludeFile"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "A.ini"},
                    {"type": "extra", "content": "\n"},
                ],
            ],
        )

    def test_path(self):
        self.cst_test(
            "path",
            [
                [
                    {"type": "property", "content": "FilePath"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "A/B"},
                    {"type": "extra", "content": "\n"},
                ],
                [
                    {"type": "property", "content": "AirResistance"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "0.05"},
                    {"type": "extra", "content": "\n"},
                ],
            ],
        )

    def test_simple(self):
        self.cst_test(
            "simple",
            [
                [
                    {"type": "property", "content": "AddEffect"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "MOPixel"},
                ]
            ],
        )

    def test_space_in_value(self):
        self.cst_test(
            "space_in_value",
            [
                [
                    {"type": "property", "content": "Foo"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "Bar Baz"},
                ]
            ],
        )

    def test_spaces_at_start_of_line(self):
        self.cst_test(
            "spaces_at_start_of_line",
            [
                [
                    {"type": "property", "content": "Foo"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "Bar"},
                    {"type": "extra", "content": "\n"},
                ],
                [
                    {"type": "property", "content": "Baz"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "Bee"},
                ],
            ],
        )

    def test_spaces_before_tab(self):
        self.cst_test(
            "spaces_before_tab",
            [
                [
                    {"type": "property", "content": "A1"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "A2"},
                    {"type": "extra", "content": "\n"},
                    {
                        "type": "children",
                        "content": [
                            [
                                {"type": "extra", "content": "   "},
                                {"type": "extra", "content": "\t"},
                                {"type": "property", "content": "B1"},
                                {"type": "extra", "content": " "},
                                {"type": "extra", "content": "="},
                                {"type": "extra", "content": " "},
                                {"type": "value", "content": "B2"},
                                {"type": "extra", "content": "\n"},
                                {
                                    "type": "children",
                                    "content": [
                                        [
                                            {"type": "extra", "content": "\t\t"},
                                            {"type": "property", "content": "C1"},
                                            {"type": "extra", "content": " "},
                                            {"type": "extra", "content": "="},
                                            {"type": "extra", "content": " "},
                                            {"type": "value", "content": "C2"},
                                            {"type": "extra", "content": "\n"},
                                        ]
                                    ],
                                },
                            ],
                        ],
                    },
                ]
            ],
        )

    def test_tabbed_comment(self):
        self.cst_test(
            "tabbed_comment",
            [
                [
                    {"type": "property", "content": "A"},
                    {"type": "extra", "content": "\n"},
                    {"type": "extra", "content": "\t"},
                    {"type": "extra", "content": "// foo"},
                    {"type": "extra", "content": "\n"},
                ],
                [
                    {"type": "property", "content": "AddEffect"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "MOPixel"},
                    {"type": "extra", "content": "\n"},
                ],
            ],
        )

    def test_too_many_tabs(self):
        with self.assertRaises(TooManyTabs):
            self.cst_test("too_many_tabs", [])

    def test_traditional_ini(self):
        self.cst_test(
            "traditional_ini",
            [
                [
                    {"type": "property", "content": "[Foo]"},
                    {"type": "extra", "content": "\n"},
                ],
                [
                    {"type": "property", "content": "Bar"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": " "},
                    {"type": "value", "content": "42"},
                    {"type": "extra", "content": "\n"},
                ],
            ],
        )

    def test_unclosed_multiline_comment(self):
        with self.assertRaises(UnclosedMultilineComment):
            self.cst_test("unclosed_multiline_comment", [])

    def test_value_on_next_line(self):
        self.cst_test(
            "value_on_next_line",
            [
                [
                    {"type": "property", "content": "Foo"},
                    {"type": "extra", "content": " "},
                    {"type": "extra", "content": "="},
                    {"type": "extra", "content": "\n"},
                    {"type": "value", "content": "Bar"},
                ]
            ],
        )

    def cst_test(self, filename, expected):
        filepath = get_test_path_from_filename(filename)

        tokens = ini_tokenizer.get_tokens(filepath)
        cst = ini_cst.get_cst(tokens)

        self.assertEqual(cst, expected)
