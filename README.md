# Cortex Command Mod Converter Engine

Automatically converts mods to the latest version of the Cortex Command Community Project.

# Contributing

## Locally testing your changes

1. Generate distribution archives and overwrite the old distribution archive with `py -m build && pip install dist/*.tar.gz`

## Running the tests
`py -m unittest`

## Updating this project on PyPI

1. Generate distribution archives with `py -m build`
2. Update this package on PyPI with `twine upload dist/*`. You'll have to enter your PyPI username and password.
