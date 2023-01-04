# Cortex Command Mod Converter Engine

Automatically converts mods to the latest version of the Cortex Command Community Project.

## Locally testing your changes

This builds the project and overwrites any older pip build of it:

`py -m build && pip install dist/*.tar.gz`

## Running the tests

The unit tests are only executed on the pip build, so the pip build has to be updated first:

`py -m build && pip install dist/*.tar.gz && py -m unittest`

## Updating this project on PyPI

1. Generate distribution archives with `py -m build`
2. Update this package on PyPI with `twine upload dist/*`
