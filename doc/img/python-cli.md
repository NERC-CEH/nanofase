# NanoFASE CLI tool

The [nanofase.py](../nanofase.py) file is a very simple command line tool for compiling/editing data and compiling/running the model. It is simply a Python wrapper for these operations. Its requirements are the same as the respective operations, e.g. [these Python packages](https://github.com/NERC-CEH/nanofase-data/blob/develop/environment.yaml) for data compilation/editing.

Compiling data:

```shell
$ ./nanofase.py compile-data /path/to/data/compilation/config.yaml
```

Editing data:

```shell
$ ./nanofase.py edit-data /path/to/data/editing/config.yaml
```

Compiling the model:

```shell
$ ./nanofase.py compile-model
```

Running the model

```shell
$ ./nanofase.py run-model /path/to/config.yaml
```