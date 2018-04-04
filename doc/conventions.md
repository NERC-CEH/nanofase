# Conventions

*Work in progress*

List of code style, formatting and naming conventions to follow. Semantics based loosely on these style guides, as well as following some [Pythonic conventions](https://www.python.org/dev/peps/pep-0008/):
- [fortran90.org: Fortran Best Practices](http://www.fortran90.org/src/best-practices.html)
- [fortran.com: Fortran Style](http://www.fortran.com/Fortran_Style.pdf)

## Indentation, line length and encoding
- Use four spaces to indent. Most editors, such as Sublime Text and Atom, allow you to specify that the <kbd>TAB</kbd> key inserts spaces.
- Limit line length to 120 characters.
- Save Fortran files in UTF-8 encoding.

## Naming conventions
Though Fortran is insensitive to case, it's still important to follow some sort of naming convention to improve code readability.
- Use lowercase for all Fortran constructs (`do`, `subroutine`, `module`, etc).
- Follow the Java/C convention of using lowerCamelCase for variable and procedure names (`settlingVelocity`, `calculateSettlingVelocity`) and UpperCamelCase for module, class, interface and type name (`RiverReachModule`, `RiverReach`).
- The exception to this is that mathematical variables should follow LaTeX-like notation and utilising underscores and capatilisation accordingly. For example, ![alpha](https://latex.codecogs.com/gif.latex?\alpha) should be named `alpha`, ![Gamma](https://latex.codecogs.com/gif.latex?\Gamma) should be named `Gamma` and ![k_settle](https://latex.codecogs.com/gif.latex?k_{\text{settle}}) should be named `k_settle`.
- Constants and globally-available variables should be written in all capitals, e.g., `ERROR_HANDLER`. Again, the exception to this is for mathematical constants, which should follow the above rule. For example, gravitational acceleration from the Constants user-derived type should be obtained by `C%g`.
- Acronyms of three or more letters should use CamelCase instead of uppercase. For example, `nSizeClassesSpm`.
- Prepend integer variables that give a length or size with `n`. For example, number of RiverReaches in a SubRiver would be `nRiverReaches`, and the number of nanoparticle size class would be `nSizeClassesNP`.
- Be explicit as possible without being unduly verbose when choosing names. A variable or procedure name should describe the value or functionality without need for further documentation. For example, use `spmSizeClasses` instead of `spmSC` and `calculateSettlingVelocity` instead of `calW`. The exception is that mathematical formalae can use mathematical symbols (as detailed above) to improve the readability of algorithms (i.e., make them resemble their mathematically-notated counterparts). For example, ![E = mc^2](https://latex.codecogs.com/gif.latex?E=mc^2) could be coded `E = m*c**2`.
- Array variable names should be plural when they represent a collection of scalars. For example, a 1D array `densities` could represent an array of densities for different objects.
- The name of logical variables or functions that return logical variables should usually be prepended with "is" or "has" as they are usually used to perform a check, often inside an `if` construct. This promotes code readability. For example, `if (gridCell%isEmpty())`.
- Time series data should be stored in a variable appended with `_timeSeries`. E.g. `Q_runoff_timeSeries`.

### Loop iterators
- Loop iterators should be a single character, e.g., `i`, `j` or `k`.
- The character used depends on what is being looped through. *Note, this convention isn't being followed rigorously at the moment.*
	+ `s` for SPM size classes.
	+ `n` for NM size classes.
	+ `l` for layers, e.g., collections of `SoilLayer` or `BedSedimentLayer` objects.
	+ `r` for collections of reaches, e.g., `EstuaryReach` or `RiverReach` objects.
	+ `x` and `y` for looping over `GridCell`s, in *x* and *y* directions, respectively.
	+ `i`, `j` and `k` for everything else. `i` should be used preferentially and `j` and `k` only if nested within an `i` loop.

### Getters, setters, initialisers, creators and calculators
Procedure names should distinguish between the types of operations they perform by the use of the keywords "get", "set", "create", "calculate" and "parse":
- `create()`: A class can have a create procedure that, if represent, must be called before anything else. It performs calculations and runs operation (e.g., gets data from a file) and populates the instance's type variables.
- `updateSomething()`: Update methods are primarily for updating the instance's type variables at each time step. This might include performing some calculations or simulations.
- `getSomething()`: Return a type variable ("something") or a simple modification of a class variable. For example, a RiverReach might have `getWidth()` to return a pre-calculated width. If the variable in question is named by a mathematical symbol (e.g. `alpha_het`), an underscore can be used to avoid capitalising the first letter of the variable name (`get_alpha_het` instead of `getAlpha_het`).
- `setSomething(something)`: Set a type variable ("something").
- `parseSomething()`: Used for reading data and manipulating to another form for us. For example, `parseInputData()` should be used to read from data input file and store data in model variables.
- `calculateSomething(args)`: Calculate functions should be *pure* functions that take input arguments, perform some calculation with them, and return an output. They shouldn't modify any variable except the input arguments and result variable. A calculate function is the analogy to a mathematical function, and might be called from within `create` and `update` procedures to set up and update object variables (though the calculate functions themselves shouldn't alter the object variables). Error handling should be used sparingly in calculate functions (see Error Handling section).

## Error Handling
- Care must be taken not to duplicate error checking and thus slow down code unnecessarily.
	+ In general, check for errors as far up the call stack as possible. This will catch errors earlier, reduce the risk of errors crashing the code and, if the error-containing datum is passed to multiple procedures, avoid having to check in multiple places for the same error:
		* For input data, `parseInputData()` procedures should, where possible, perform error checking. The exception is if the datum in question has the potential to be erroneous for one purpose but non-erroneous for another.
		* Calculate functions (`calculateSomething(args)`) should only check for an error when these errors can not be picked up further up the stack. These functions are likely to be called the most throughout the model run and errors arising from bad input data should have been picked up previously. Thus, to limit computational demands, only check for errors when the error couldn't have been checked for before the function was called (e.g., from bad input data). An example of when error checking can be used in calculate functions is when performing numerical calculations; it is difficult to tell whether certain input data will cause instabilities in numerical calculations, thus it is only practicable to do this error checking during the numerical calculation itself.
- Error messages and traces are likely to be read by the end user of the model, rather than the programmer (as programming errors are likely to be weeded out during development). Bear this in mind when writing these messages; technical information about where in the code the error ocurred is likely to be less useful than general descriptions of what the model was doing and what input data might have caused the error. For example, "Value for heteroaggregation attachment efficiency, alpha_hetero, not found in input file. Trace: Creating GridCell at (1,1) > Creating RiverReach 1 > Parsing input data" is more useful then "Value for alpha_hetero not found. Trace: GridCell_1_1%RiverReach_1_1_1%parseInputData".
- Let errors propagate as far up the call stack as possible before triggering them, to give users as complete a trace as possible as to where the error if from. The `if (r%hasError())` check and `return` construct can be used to this effect, to avoid code being run that the error will cause to crash.

## Units
 - Where possible, input and output data should use SI units.
 - Internal data should also, for the most part, use SI units. The exception is for flux/flow data (e.g. water flows, SPM/NM mass flows): In these cases, the unit of time should be the duration of the timestep. For example, m<sup>3</sup>/timestep and kg/timestep should be used for water flows and SPM/NM mass flows, respectively.
 - If a variable is normalised to a unit length from an area or volume (e.g. rainfall as m/s = m<sup>3</sup>/m<sup>2</sup>/s, as opposed to m<sup>3</sup>/s), the corresponding variable name should be lowercase. Otherwise, it should be capitalised. For example, `q_runoff` for runoff in m/s, `Q_in` for river inflow in m<sup>3</sup>/s.

## Data conventions
- Where possible, input and output data should use SI units.
- In contrast to the Fortran code naming conventions above, input data variable names should use snake_case, in line with common practice for data files. End users are likely to be more accustomed to snake_case rather than lowerCamelCase. Group names, which represent objects in the model, should still use UpperCamelCase.
- External variable names (those use in input/output data files) should map to internal variable names (those used in the model), aside for the change of case conventions (e.g. `n_timesteps` to `nTimesteps`). The exception to this is if a mathematical symbol is being used to store the data internally and that symbol would be unintuitive out of context (though, if this is the case, it might be worth checking the internal variable name itself it appropriate).