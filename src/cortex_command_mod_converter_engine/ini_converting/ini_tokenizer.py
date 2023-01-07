import re


def get_tokens(filepath):
    with open(filepath, "r") as f:
        text = f.read().lstrip()

    return get_tokens_from_str(text, filepath)


def get_tokens_from_str(text, filepath=None):
    tokens = []
    text_len = len(text)
    line_number = 1

    i = 0
    while i < text_len:
        char = text[i]

        if char == "/":
            i, token_type, content, line_number = tokenize_comment(
                i, text_len, text, tokens, line_number, filepath
            )
        elif char == "\t":
            i, token_type, content, line_number = tokenize_tabs(
                i, text_len, text, tokens, line_number, filepath
            )
        elif char == " ":
            i, token_type, content, line_number = tokenize_spaces(
                i, text_len, text, tokens, line_number, filepath
            )
        elif char == "=":
            i, token_type, content, line_number = tokenize_equals(
                i, text_len, text, tokens, line_number, filepath
            )
        elif char == "\n":
            i, token_type, content, line_number = tokenize_newlines(
                i, text_len, text, tokens, line_number, filepath
            )
        else:
            i, token_type, content, line_number = tokenize_word(
                i, text_len, text, tokens, line_number, filepath
            )

        tokens.append(get_token(token_type, content, line_number, filepath))

    return tokens


def tokenize_comment(i, text_len, text, tokens, line_number, filepath):
    if i + 1 < text_len and text[i + 1] == "/":
        return tokenize_single_line_comment(
            i, text_len, text, tokens, line_number, filepath
        )
    else:
        return tokenize_multi_line_comment(
            i, text_len, text, tokens, line_number, filepath
        )


def tokenize_single_line_comment(i, text_len, text, tokens, line_number, filepath):
    content = ""

    while i < text_len and text[i] != "\n":
        content += text[i]
        i += 1

    return i, "EXTRA", content, line_number


def get_token(token_type, content, line_number, filepath):
    return {
        "type": token_type,
        "content": content,
        "line_number": line_number,
        "filepath": filepath,
    }


def tokenize_multi_line_comment(i, text_len, text, tokens, line_number, filepath):
    content = ""

    while i < text_len and not (
        text[i] == "*" and i + 1 < text_len and text[i + 1] == "/"
    ):
        content += text[i]
        i += 1

    content += "*/"
    i += 2

    return i, "EXTRA", content, line_number


def tokenize_tabs(i, text_len, text, tokens, line_number, filepath):
    content = ""

    while i < text_len and text[i] == "\t":
        content += text[i]
        i += 1

    return i, "TABS", content, line_number


def tokenize_spaces(i, text_len, text, tokens, line_number, filepath):
    content = ""

    while i < text_len and text[i] == " ":
        content += text[i]
        i += 1

    return i, "EXTRA", content, line_number


def tokenize_equals(i, text_len, text, tokens, line_number, filepath):
    content = ""

    while i < text_len and text[i] == "=":
        content += text[i]
        i += 1

    return i, "EQUALS", content, line_number


def tokenize_newlines(i, text_len, text, tokens, line_number, filepath):
    content = ""

    while i < text_len and text[i] == "\n":
        content += text[i]
        i += 1
        line_number += 1

    return i, "NEWLINES", content, line_number


def tokenize_word(i, text_len, text, tokens, line_number, filepath):
    content = ""

    subtext = text[i:]

    content = re.match("(.+?)\s*(=|\/\/|\/\*|\n)", subtext + "\n").group(1)

    i += len(content)

    return i, "WORD", content, line_number
