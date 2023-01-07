# Cortex Command Mod Converter Engine

Automatically converts mods to the latest version of the Cortex Command Community Project.

## Locally testing your changes

This builds the project and overwrites any older pip build of it:

`py -m build && pip install dist/*.tar.gz`

## Running the tests

The unit tests are only executed on the pip build, so if you edit Python files inside of `src/` you'll need to VS Code launch `rebuild & test`.

If you just made changes to Python files inside of `tests/` however, you can just launch `test`.

Once you've launched either of these configurations once, you can just press `F5` to relaunch that same configuration.

Note that these launch configurations have the `justMyCode` option disabled, which allows you to step into tests while debugging.

## Updating this project on PyPI

1. Generate distribution archives with `py -m build`
2. Update this package on PyPI by running the `upload to pypi` VS Code task.
