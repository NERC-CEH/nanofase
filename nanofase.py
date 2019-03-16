#!/usr/bin/env python
"""Simple command line interface for performing frequent tasks
related to the NanoFASE model."""
import sys
import os

options = {
    "clean-log": {
        "description": "Remove log files from log/",
        "run-description": "Removing log files from log/",
        "cmd": "rm -f log/*"
    },
    "clean-cache": {
        "description": "Clear cache files from cache/",
        "run-description": "Clearing cache files from cache/",
        "cmd": "rm -f cache/*"
    },
    "clean-comp": {
        "description": "Remove compilation files",
        "run-description": "Removing compilation files",
        "cmd": "make clean"
    },
    "clean": {
        "description": "Remove log, cache and compilation files",
        "run-description": "Removing log, cache and compilation files",
        "cmd": "rm -f log/* && rm -f cache/* && make clean"
    },
    "compile": {
        "description": "Compile the model using the makefile",
        "run-description": "Compiling the model using the makefile",
        "cmd": "make"
    },
    "run": {
        "description": "Run the model (without compiling)",
        "run-description": "Running the model (without compiling)",
        "cmd": "make run"
    },
    "run-thames": {
        "description": "Run the model (without compiling) using Thames config (config/thames.nml)",
        "run-description": "Running the model (without compiling) using Thames config (config/thames.nml)",
        "cmd": "make run CONFIG_FILE=thames.nml"
    },
    "run-gwava": {
        "description": "Run the model (without compiling) using GWAVA config (config/gwava.nml)",
        "run-description": "Running the model (without compiling) using GWAVA config (config/gwava.nml)",
        "cmd": "make run CONFIG_FILE=gwava.nml"
    },
    "compile-run": {
        "description": "Compile the model using the makefile and then run",
        "run-description": "Compiling the model using the makefile and then running",
        "cmd": "make && make run"
    },
    "compile-run-thames": {
        "description": "Compile the model using the makefile and then run using Thames config (config/thames.nml)",
        "run-description": "Compiling the model using the makefile and then running using Thames config (config/thames.nml)",
        "cmd": "make && make run CONFIG_FILE=thames.nml"
    },
    "view-log": {
        "description": "View the latest log file",
        "run-description": "Viewing the latest log file",
        "cmd": "cat ./log/$(ls ./log -t | head -n1)"
    },
    "vi-log": {
        "description": "Edit the latest log file in Vim",
        "run-description": "Editting the latest log file in Vim",
        "cmd": "vi ./log/$(ls ./log -t | head -n1)"
    },
    "subl-log": {
        "description": "Edit the latest log file in Sublime Text",
        "run-description": "Editting the latest log file in Sublime Text",
        "cmd": "subl ./log/$(ls ./log -t | head -n1)"
    }
}

if len(sys.argv) < 2:
    print("Welcome to the NanoFASE CLI tool! Pass me one of the following options:")
    for option, detail in options.items():
        print("\t{0:20}\t{1}".format(option,detail['description']))

    n_files = len(os.listdir('./log'))
    if n_files > 50:
        print("\033[34mWarning:\033[0m There are {0} files in the log/ directory. Consider cleaning using the `clean` command.".format(n_files))

else:

    n_files = len(os.listdir('./log'))
    if n_files > 50:
        print("\033[34mWarning:\033[0m There are {0} files in the log/ directory. Consider cleaning using the `clean` command.".format(n_files))

    option = sys.argv[1]                        # Get the command
    print(options[option]['run-description'])   # Print out what we're doing
    os.system(options[option]['cmd'])           # Run the command
