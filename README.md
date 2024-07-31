<p align="center"><img src="https://github.com/cortex-command-community/Cortex-Command-Legacy-Mod-Converter/blob/master/Media/legacy-mod-converter-icon.png" alt="Cortex Command Mod Converter Engine icon"></p>
<h1 align="center">Cortex Command Mod Converter Engine</h1>

A library for automatically converting mods to the latest version of the [Cortex Command Community Project](https://github.com/cortex-command-community/Cortex-Command-Community-Project).

The [Cortex Command Legacy Mod Converter](https://github.com/cortex-command-community/Cortex-Command-Legacy-Mod-Converter) presents a GUI for this library.

## Usage

1. Download Zig version 0.12.0 for Windows from [here](https://ziglang.org/download/0.12.0/zig-windows-x86_64-0.12.0.zip). You'll need to add the path to its unzipped directory to your system's `Path` environment variable. Confirm it works by running `zig version` in a terminal.
2. Clone this repository.
3. Open the cloned directory in [Visual Studio Code](https://code.visualstudio.com/). (Which is absolutely not the same thing as Visual Studio!)
4. Hold `Ctrl+Shift+B` to get a selection of the available tasks (coming from `.vscode/tasks.json`), and select the `Tests` task. A terminal should pop up in VS Code that shows that all tests pass.
5. Like stated in the introduction, you should use the [Cortex Command Legacy Mod Converter](https://github.com/cortex-command-community/Cortex-Command-Legacy-Mod-Converter) if you want a GUI for this library. If you want to run the engine from the terminal however, you can do so. By hitting `F5` while having this repository open in VS Code, the default launch configuration (coming from `.vscode/launch.json`) will run, which converts the mods in the `input/` directory and outputs them to the `output/` directory.
