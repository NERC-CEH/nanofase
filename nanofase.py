#!/usr/bin/env python
"""A simple CLI tool for the NanoFASE model. Acts as a Python wrapper around data and
model compilation and running. Run `./nanofase.py -h` for usage."""
import sys
sys.path.append('vendor/nanofase-data')
import os
import argparse
from compiler import Compiler


# Compile data using the given config file
def compile_data(config_file):
    compiler = Compiler('create', config_file, os.path.join(sys.path[0], 'vendor/nanofase-data/model_vars.yaml'))
    compiler.create()


# Edit data using the given config file
def edit_data(config_file):
    compiler = Compiler('edit', config_file, os.path.join(sys.path[0], 'vendor/nanofase-data/model_vars.yaml'))
    compiler.edit()


# Compile the model using make. This is simply a wrapper to execute the make shell command
def compile_model():
    os.system(f'make')


# Run the model, again using make. We use make so that we call the exe in the correct place.
def run_model(config_file):
    # Splitting the config file location to the path and the filename, to pass separately
    # to the make run command
    if os.path.isfile(config_file):
        config_split = os.path.split(config_file)
        os.system(f'make run CONFIG_PATH=\"{config_split[0]}/\" CONFIG_FILE=\"{config_split[1]}\"')
    else:
        sys.exit(f'Config file at {config_file} could not be found.')


# Command line choices available
choices = ['compile-data', 'edit-data', 'compile-model', 'run-model']

# Parse the command line args
parser = argparse.ArgumentParser(description='Common utilities for the NanoFASE model.')
parser.add_argument('method', help='method to run', choices=choices)
parser.add_argument('-c', '--config', help='path to the config file for this method')
args = parser.parse_args()

# Check the option chosen and run the relevant method for that choice
if args.method == 'compile-data':
    # Compile the data from scratch
    compile_data(args.config)
elif args.method == 'edit-data':
    # Edit the data
    edit_data(args.config)
elif args.method == 'compile-model':
    # Compile the model using make
    compile_model()
elif args.method == 'run-model':
    # Run the model using the given config file
    run_model(args.config)
