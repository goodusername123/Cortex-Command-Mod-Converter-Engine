import re


def get_tokens(filepath):
    with open(filepath, "r") as f:
        text = f.read().lstrip()

    return get_tokens_from_str(text, filepath)


def get_tokens_from_str(text, filepath=None):
    tokens = []
    text_len = len(text)
    line_number = 1

    tokenize_lookup_table = {
        "/": tokenize_comment,
        "\t": tokenize_tabs,
        " ": tokenize_spaces,
        "=": tokenize_equals,
        "\n": tokenize_newlines,
    }

    i = 0
    while i < text_len:
        char = text[i]

        if char in tokenize_lookup_table:
            tokenize_fn = tokenize_lookup_table[char]
        else:
            tokenize_fn = tokenize_word

        i, token_type, content, line_number = tokenize_fn(
            i, text_len, text, line_number
        )

        tokens.append(get_token(token_type, content, line_number, filepath))

    return tokens


def tokenize_comment(i, text_len, text, line_number):
    if i + 1 < text_len and text[i + 1] == "/":
        return tokenize_single_line_comment(i, text_len, text, line_number)
    else:
        return tokenize_multi_line_comment(i, text_len, text, line_number)


def tokenize_single_line_comment(i, text_len, text, line_number):
    content = ""

    while i < text_len and text[i] != "\n":
        content += text[i]
        i += 1

    return i, "EXTRA", content, line_number


class UnclosedMultilineComment(Exception):
    pass


def tokenize_multi_line_comment(i, text_len, text, line_number):
    content = ""

    while i < text_len and not (
        text[i] == "*" and i + 1 < text_len and text[i + 1] == "/"
    ):
        content += text[i]
        i += 1

    if i == text_len:
        raise UnclosedMultilineComment(f"line {line_number}")

    content += "*/"
    i += 2

    return i, "EXTRA", content, line_number


def tokenize_tabs(i, text_len, text, line_number):
    content = ""

    while i < text_len and text[i] == "\t":
        content += text[i]
        i += 1

    return i, "TABS", content, line_number


def tokenize_spaces(i, text_len, text, line_number):
    content = ""

    while i < text_len and text[i] == " ":
        content += text[i]
        i += 1

    return i, "EXTRA", content, line_number


def tokenize_equals(i, text_len, text, line_number):
    content = ""

    while i < text_len and text[i] == "=":
        content += text[i]
        i += 1

    return i, "EQUALS", content, line_number


def tokenize_newlines(i, text_len, text, line_number):
    content = ""

    while i < text_len and text[i] == "\n":
        content += text[i]
        i += 1
        line_number += 1

    return i, "NEWLINES", content, line_number


def tokenize_word(i, text_len, text, line_number):
    content = ""

    subtext = text[i:]

    content = re.match("(.+?)\s*(=|\/\/|\/\*|\n)", subtext + "\n").group(1)

    i += len(content)

    return i, "WORD", content, line_number


def get_token(token_type, content, line_number, filepath):
    return {
        "type": token_type,
        "content": content,
        "line_number": line_number,
        "filepath": filepath,
    }
