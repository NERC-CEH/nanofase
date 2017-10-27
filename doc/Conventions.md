# Conventions

*Work in progress*

List of code style, formatting and naming conventions to follow. Based loosely on these style guides, as well as following some [Pythonic conventions](https://www.python.org/dev/peps/pep-0008/):
- [fortran90.org: Fortran Best Practices](http://www.fortran90.org/src/best-practices.html)
- [fortran.com: Fortran Style](http://www.fortran.com/Fortran_Style.pdf)

## Indentation, line length and encoding
- Use four spaces to indent. Most editors, such as Sublime Text and Atom, allow you to specify that the <kbd>TAB</kbd> key inserts spaces.
- Limit line length to 80 characters.
- Save Fortran files in UTF-8 encoding.

## Naming conventions
Though Fortran is insensitive to case, it's still important to follow some sort of naming convention to improve code readability.
- Use lowercase for all Fortran constructs (`do`, `subroutine`, `module`, etc).
- Follow the Java convention of using lowerCamelCase for variable and procedure names (`settlingVelocity`, `calculateSettlingVelocity`) and UpperCamelCase for module, class, interface and type name (`RiverReachModule`, `RiverReach`).
- The exception to this is that mathematical variables should follow LaTeX-like notation and utilising underscores and capatilisation accordingly. For example, ![alpha](https://latex.codecogs.com/gif.latex?\alpha) should be named `alpha`, ![Gamma](https://latex.codecogs.com/gif.latex?\Gamma) should be named `Gamma` and ![k_settle](https://latex.codecogs.com/gif.latex?k_{\text{settle}}) should be named `k_settle`.
    - Maintain the list of mathematical symbols used, which is located at [doc\Mathematical Symbols.ipynb](https://github.com/NERC-CEH/nanofase/blob/master/doc/Mathematical%20symbols.ipynb).
- Constants and globally-available variables should be written in all capitals, e.g., `ERROR_HANDLER`. Again, the exception to this is for mathematical constants, which should follow the above rule. For example, gravitational acceleration from the Constants user-derived type should be obtained by `C%g`.
- Acronyms of three or more letters should use CamelCase instead of uppercase. For example, `nSizeClassesSpm`.
- Prepend integer variables that give a length or size with `n`. For example, number of RiverReaches in a SubRiver would be `nRiverReaches`, and the number of nanoparticle size class would be `nSizeClassesNP`.
- Prepend integer variables that represent an array index with `i`. For example, index of a particular size class could be called `iSizeClass`.
- The variable names `i`, `j` and `k` should be reserved for loop iterators.
- Be explicit as possible without being unduly verbose when choosing names. A variable or procedure name should describe the value or functionality without need for further documentation. For example, use `spmSizeClasses` instead of `spmSC` and `calculateSettlingVelocity` instead of `calW`. The exception is that mathematical formalae can use mathematical symbols (as detailed above) to improve the readability of algorithms (i.e., make them resemble their mathematically-notated counterparts). For example, ![E = mc^2](https://latex.codecogs.com/gif.latex?E=mc^2) could be coded `E = m*c**2`.
- Array variable names should be plural when they represent a collection of scalars. For example, a 1D array `densities` could represent an array of densities for different objects.
- The name of functions that return or variable that are logical values should usually be prepended with "is" as they are usually used to perform a check, often inside an `if` construct. This promotes code readability. For example, `if (gridCell%isEmpty())`.

### Getters, setters, initialisers, creators and calculators
Procedure names should distinguish between the types of operations they perform by the use of the keywords "get", "set", "create", "init" and "calculate":
- `create()`: A class can have a create procedure that, if represent, must be called before anything else. It performs calculations and runs operation (e.g., gets data from a file) and populates the instance's type variables.
- `updateSomething()`: Update methods are primarily for updating the instance's type variables at each time step. This might include performing some calculations or simulations.
- `getSomething()`: Return a type variable ("something") or a simple modification of a class variable. For example, a RiverReach might have `getWidth()` to return a pre-calculated width.
- `setSomething(something)`: Set a type variable ("something")
- `calculateSomething(args)`: Calculate functions should be _pure_ functions that take input arguments, perform some calculation with them, and return an output. They shouldn't modify any variable except the input arguments and result variable. A calculate function is the analogy to a mathematical function, and might be called from within `create` and `init` procedures to set up class variables (though the calculate functions themselves shouldn't alter the type variables). Error handling should be used sparingly in calculate functions (see Error Handling section).

## Error Handling
- Calculate functions (`calculateSomething(args)`) should only check for error when these errors can not be picked up further up the stack. These functions are likely to be called the most throughout the model run and errors arising from bad input data should have been picked up previously. Thus, to limit computational demands, only check for errors when the error couldn't have been checked for before the function was called (e.g., from bad input data). An example of when error checking can be used in calculate functions is when performing numerical calculations; it is difficult to tell whether certain input data will cause instabilities in numerical calculations, thus it is only practicable to do this error checking during the numerical calculation itself.