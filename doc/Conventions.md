# Conventions

List of code style, formatting and naming conventions to follow. Based loosely on these style guides, as well as following some [Pythonic conventions](https://www.python.org/dev/peps/pep-0008/):
- [fortran90.org: Fortran Best Practices](http://www.fortran90.org/src/best-practices.html)
- [fortran.com: Fortran Style](http://www.fortran.com/Fortran_Style.pdf)

### Indentation, line length and encoding
- Use four spaces to indent. Most editors, such as Sublime Text and Atom, allow you to specify that the <kbd>TAB</kbd> key inserts spaces.
- Limit line length to 80 characters.
- Save Fortran files in UTF-8 encoding.

### Naming conventions
Though Fortran is insensitive to case, it's still important to follow some sort of naming convention to improve code readability.
- Use lowercase for all Fortran constructs (`do`, `subroutine`, `module`, etc).
- Follow the [Java](https://en.wikipedia.org/wiki/Naming_convention_(programming)#Java) convention of using lowerCamelCase for variable and procedure names (`settlingVelocity`, `calculateSettlingVelocity`) and UpperCamelCase for module, class, interface and type name (`RiverReachModule`, `RiverReach`).
- The exception to this is that mathematical variables should follow LaTeX-like notation and utilising underscores and capatilisation accordingly.
- Acronyms of three or more letters should use CamelCase instead of uppercase. For example, `nSizeClassesSpm`.
- Prepend integer variables that give a length or size with `n`. For example, number of RiverReaches in a SubRiver would be `nRiverReaches`, and the number of nanoparticle size class would be `nSizeClassesNP`.