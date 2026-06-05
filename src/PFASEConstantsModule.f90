module PFASEConstantsModule
    !! Central constants for P-FASE PFAS state indexing.
    !! All PFAS mass arrays use c(species, form, phase).
    implicit none
    integer, parameter, public :: PFAS_AQ   = 1  !! dissolved aqueous / porewater mass
    integer, parameter, public :: PFAS_SOL  = 2  !! sorbed to soil / bed sediment solids
    integer, parameter, public :: PFAS_SPM  = 3  !! sorbed to suspended particulate matter
    integer, parameter, public :: PFAS_AWI  = 4  !! air-water interface mass
    integer, parameter, public :: PFAS_FOAM = 5  !! foam / surface microlayer export mass
    integer, parameter, public :: PFAS_AIR  = 6  !! atmospheric gas/aerosol/resuspended mass
    integer, parameter, public :: PFAS_NPHASES = 6
    integer, parameter, public :: PFAS_MAX_NAME = 64
contains
    pure function pfase_phase_name(phase) result(name)
        integer, intent(in) :: phase
        character(len=16) :: name
        select case (phase)
        case (PFAS_AQ);   name = 'aqueous'
        case (PFAS_SOL);  name = 'solid_sorbed'
        case (PFAS_SPM);  name = 'spm_sorbed'
        case (PFAS_AWI);  name = 'awi'
        case (PFAS_FOAM); name = 'foam'
        case (PFAS_AIR);  name = 'air'
        case default;     name = 'unknown'
        end select
    end function pfase_phase_name
end module 
