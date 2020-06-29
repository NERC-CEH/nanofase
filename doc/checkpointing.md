# Checkpointing

Checkpointing is the ability to "save" a computation so that it can be resumed later, rather than started again. The NanoFASE model offers limited checkpointing capability, in [CheckpointModule.f90](../src/CheckpointModule.f90).

## Config file options

Checkpointing is configured by the model config file, specifically the `&checkpoint` group:

```nml
&checkpoint
checkpoint_file = "./checkpoint.dat"                    ! Location of checkpoint file to read from and/or save to
save_checkpoint = .true.                                ! Save a checkpoint file when the run is finished? Defaults to false
reinstate_checkpoint = .false.                          ! Reinstate a checkpoint from checkpoint_file? Defaults to false
preserve_timestep = .false.                             ! Should the timestep from the checkpoint be used as a starting timestep in a reinstated run?
/
```

## Saving a checkpoint

A checkpoint can be saved at the end of a model run by setting `save_checkpoint` to `.true.`. When a checkpoint is saved, a binary file is created in a specified location with the current values of all dynamic variables (the variables whose value on a particular timestep is a function of their value on the previous timestep). The location of this checkpoint is given by `checkpoint_file`. Make sure the directory you wish to save the checkpoint to exists.

## Reinstating a checkpoint

A previously saved checkpoint can be reinstated at the beginning of a model run, from the checkpoint at `checkpoint_file`, by setting `reinstate_checkpoint` to `.true.`.

## Interoperability and checkpoint file size

The checkpoint file is saved as a binary (unformatted) file. This ensures complete accuracy (no data loss) and speed in saving/reinstate, but at the expense of interoperability. Notably, binary checkpoint files created on one operating system may not be usable on a different operating system.

Whilst the checkpoint module only saves a subset of the model variables to file, checkpoint file sizes can still be fairly large for geographical scenarios with a large number of grid cells, sediment size classes and nanomaterial size classes. The example Thames scenario, when saved to a checkpoint, creates a file that is ~500 MB.