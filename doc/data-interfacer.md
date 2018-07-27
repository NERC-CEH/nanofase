# Inputing data using the `DataInterfacer` class

The [`DataInterfacer`](/src/DataInterfacer/classDataInterfacer.f90) class provides a convenient interface for processing input data.

A `DataInterfacer` object, named `DATA`, is defined in [Globals.f90](/src/Globals.f90) and initialised in [main.f90](/src/main.f90). The object `DATA` is then available globally.

```fortran
call DATA%init(C%inputFile)   		! C%inputFile is the path to the data input file
```

## Setting the group
The `DATA` object has an attribute `class(NcGroup) :: grp`, which is represents the current NetCDF group being inquired by the object. Upon initialisation, this group is the root group. To set the group, an array of character strings can be passed to the `DATA%setGroup` method, where each element of the array represents another depth of the hierarcichal data.

For example, if the data structure is as such,

```json
{
	"Environment" : {
		"GridCell_1_1" : {
			"demands": {}
		}
	}
}
```

then the `demands` group can be inquired by calling:

```fortran
type(Result) :: r
r = DATA%setGroup(["Environment", "GridCell_1_1", "demands"])
```

Any errors encountered when trying to set the group (e.g. if the group wasn't found) will be contained in the `Result` object returned (see the [Fortran Error Handler](https://github.com/samharrison7/fortran-error-handler) framework docs).

## Getting data
Now we can get data from this group by passing the `DATA%get(varName, var)` a variable name `varName` and a variable to return the data in `var`. From example, say `demands` has a real variable called `total_population` that we want to retrieve:

```fortran
type(Result) :: r
real :: total_population

r = DATA%setGroup(["Environment", "GridCell_1_1", "demands"])
r = DATA%get('total_population', total_population)
```

### Type and rank
`integer`, `real` and `real(dp)` data up to a rank of 4 is supported.

### Defaulting
A `default` variable can be passed to the `DATA%get(varName, var, default)` method and this is used to fill `var` if `varName` doesn't exist in the dataset. This is done silently by default (no error is return in the `Result` object), but passing `warnIfDefaulting = .true.` means the `Result` object will contain a warning that a variable has defaulted.

Additionally, the default warning message can be changed by passing `defaultMessage = "Custom default method"`.

```fortran
r = DATA%get( &
	varName = 'total_population', &
	var = total_population, &
	default = 0.0, &
	warnIfDefaulting = .true. &
)
```

The default warning error message is as such:

```
Variable 'total_population' not found in group 'demands'. Defaulting to 0.0.
```

### Silently fail
Finally, if you wish for absolutely no errors to be added to the `Result` object, even if the variable can't be found and there is no default, you can pass `silentlyFail = .true.`:

```fortran
! r will contain no errors, even if total_population doesn't exist
r = DATA%get('total_population', total_population, silentlyFail = .true.)
```

Use this with care! It only really makes sense if the variable you are retrieving has already been set to some default earlier in the code.