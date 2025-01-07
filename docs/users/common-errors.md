# Common issues and errors

This page contains a selection of common issues and errors that might be encountered during model usage.

## Runtime errors

**Process killed errors**
: If the model crashes with an error message something along the lines of "Killed", it is likely that the model has run out of memory. This is most likely to happen for model runs with big and/or high-resolution spatial domains, or over long time periods. Over and above using a machine with more memory, you could also try splitting your model run into chunks, as described in the [](./batch.md) documentation.

% TODO add errors for:
%    missing folders
%    grain size distribution???