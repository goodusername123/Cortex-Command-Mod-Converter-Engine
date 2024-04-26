# Cortex Command Mod Converter Engine

A library for automatically converting mods to the latest version of the [Cortex Command Community Project](https://github.com/cortex-command-community/Cortex-Command-Community-Project).

The [Cortex Command Legacy Mod Converter](https://github.com/cortex-command-community/Cortex-Command-Legacy-Mod-Converter) presents a GUI for this library.

## Usage

1. Download Zig version 0.12.0 from [here](https://ziglang.org/download/#release-0.12.0)
2. Clone this repository somewhere
3. Open the cloned directory in [Visual Studio Code](https://code.visualstudio.com/) (which is absolutely not the same thing as Visual Studio!)
4. Hold `Ctrl+Shift+B` to get a selection of the available tasks (coming from `.vscode/tasks.json`), and select the `Tests` task. A terminal should pop up in VS Code that shows that all tests pass.
5. Like stated in the introduction, you should use the [Cortex Command Legacy Mod Converter](https://github.com/cortex-command-community/Cortex-Command-Legacy-Mod-Converter) if you want a GUI for this library. If you want to run the engine from the terminal however, you can do so. By hitting `F5` while having this repository open in VS Code, the default launch configuration (coming from `.vscode/launch.json`) will run, which converts the mods in the `input/` directory and outputs them to the `output/` directory.
