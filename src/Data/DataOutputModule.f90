!> Module container for the DataOutput class
module DataOutputModule
    use Globals, only: C, dp
    use classDatabase, only: DATASET
    use spcEnvironment
    use classEnvironment1
    use spcGridCell
    use classRiverReach
    use classEstuaryReach
    use UtilModule
    use datetime_module
    implicit none

    !> The DataOutput class is responsible for writing output data to disk
    type, public :: DataOutput
        character(len=256) :: outputPath                    !! Path to the output directory
        type(EnvironmentPointer) :: env                     !! Pointer to the environment, to retrieve state variables
      contains
        procedure, public :: init => initDataOutput
        procedure, public :: update => updateDataOutput
        procedure, public :: finalise => finaliseDataOutput
        procedure, private :: writeHeaders => writeHeadersDataOutput
        procedure, private :: writeHeadersSimulationSummary => writeHeadersSimulationSummaryDataOutput
        procedure, private :: writeHeadersWater => writeHeadersWaterDataOutput
        procedure, private :: writeHeadersSediment => writeHeadersSedimentDataOutput
        procedure, private :: writeHeadersSoil => writeHeadersSoilDataOutput
        procedure, private :: updateWater => updateWaterDataOutput
        procedure, private :: updateSediment => updateSedimentDataOutput
        procedure, private :: updateSoil => updateSoilDataOutput
    end type

  contains
    
    subroutine initDataOutput(me, env)
        class(DataOutput)           :: me
        type(Environment1), target  :: env
        
        ! Point the Environment object to that passed in
        me%env%item => env

        ! Open the files to write to
        open(unit=100, file=trim(C%outputPath) // 'simulation_summary.md')
        if (C%writeCSV) then
            open(unit=101, file=trim(C%outputPath) // 'output_water.csv')
            open(unit=102, file=trim(C%outputPath) // 'output_sediment.csv')
            open(unit=103, file=trim(C%outputPath) // 'output_soil.csv')
            ! open(unit=104, file=trim(C%outputPath) // 'n_output_soil_biota.csv')
            ! open(unit=105, file=trim(C%outputPath) // 'n_output_water_biota.csv')
            ! open(unit=106, file=trim(C%outputPath) // 'n_output_rate_constants.csv')
        end if

        ! Write the headers for the files
        call me%writeHeaders()
    end subroutine

    !> Save the output from the current timestep to the output files
    subroutine updateDataOutput(me, t)
        class(DataOutput)   :: me       !! The DataOutput instance
        integer             :: t        !! The current timestep
        integer             :: x, y     ! Iterators
        type(datetime)      :: date
        character(len=100)  :: dateISO
        real                :: easts, norths

        ! Get the date for this timestep
        date = C%startDate + timedelta(t - 1)
        dateISO = date%isoformat()
        
        ! Loop through the grid cells and update each compartment
        do y = 1, size(me%env%item%colGridCells, dim=2)
            do x = 1, size(me%env%item%colGridCells, dim=1)
                easts = DATASET%x(x)
                norths = DATASET%y(y)
                call me%updateWater(t, x, y, dateISO, easts, norths)
                call me%updateSediment(t, x, y, dateISO, easts, norths)
                call me%updateSoil(t, x, y, dateISO, easts, norths)
            end do
        end do
    end subroutine

    !> Update the water output file for the current timestep
    subroutine updateWaterDataOutput(me, t, x, y, date, easts, norths)
        class(DataOutput)   :: me               !! The DataOutput instance
        integer             :: t                !! The current timestep
        integer             :: x, y             !! Grid cell indices
        character(len=*)    :: date             !! Datetime of this timestep
        real                :: easts, norths    !! Eastings and northings of this grid cell
        integer             :: w                ! Waterbody iterator
        character(len=3)    :: reachType        ! Is this a river of estuary?
        
        ! Loop through the waterbodies in this cell
        do w = 1, me%env%item%colGridCells(x,y)%item%nReaches
            associate (reach => me%env%item%colGridCells(x,y)%item%colRiverReaches(w)%item)
                ! Is it a reach or an estuary?
                select type (reach)
                    type is (RiverReach)
                        reachType = 'riv'
                    type is (EstuaryReach)
                        reachType = 'est'
                end select
                ! Write the data
                write(101, *) trim(str(t)), ",", trim(date), ",", trim(str(x)), ",", trim(str(y)), ",", &
                    trim(str(easts)), ",", trim(str(norths)), ",", trim(str(w)), ",", reachType, ",", &
                    trim(str(sum(reach%m_np))), ",", trim(str(sum(reach%C_np))), ",", &
                    trim(str(sum(reach%m_transformed))), ",", trim(str(sum(reach%C_transformed))), ",", &
                    trim(str(reach%m_dissolved)), ",", trim(str(reach%C_dissolved)), ",", &
                    trim(str(sum(reach%j_np_deposit()))), ",", &
                    trim(str(sum(reach%j_transformed_deposit()))), ",", &
                    trim(str(sum(reach%j_np_outflow()))), ",", &
                    trim(str(sum(reach%j_transformed_outflow()))), ",", &
                    trim(str(reach%j_dissolved_outflow())), ",", &
                    trim(str(sum(reach%m_spm))), ",", &
                    trim(str(sum(reach%C_spm))), ",", &
                    trim(str(reach%volume)), ",", &
                    trim(str(reach%depth)), ",", &
                    trim(str(reach%Q(1)/C%timeStep))
            end associate
        end do
    end subroutine

    !> Update the current sediment output file on the current timestep
    subroutine updateSedimentDataOutput(me, t, x, y, date, easts, norths)
        class(DataOutput)   :: me               !! The DataOutput instance
        integer             :: t                !! The current timestep
        integer             :: x, y             !! Grid cell indices
        character(len=*)    :: date             !! Datetime of this timestep
        real                :: easts, norths    !! Eastings and northings of this grid cell
        integer             :: w, l             ! Iterators
        character(len=3)    :: reachType        ! Is this a river of estuary?
       
        ! Loop through the waterbodies in this cell
        do w = 1, me%env%item%colGridCells(x,y)%item%nReaches
            associate (reach => me%env%item%colGridCells(x,y)%item%colRiverReaches(w)%item)
                ! Is this a river or estuary?
                select type (reach)
                    type is (RiverReach)
                        reachType = 'riv'
                    type is (EstuaryReach)
                        reachType = 'est'
                end select
                ! Write the data
                write(102, '(a)', advance='no') trim(str(t)) // "," // trim(date) // "," // trim(str(x)) // "," // trim(str(y)) &
                    // "," // trim(str(easts)) // "," // trim(str(norths)) // "," // trim(str(w)) // "," // reachType // "," // &
                    trim(str(sum(reach%bedSediment%get_m_np()) * reach%bedArea)) // "," // &            ! Converting from kg/m2 to kg
                    trim(str(sum(reach%bedSediment%get_C_np()))) // "," // &
                    trim(str(sum(reach%bedSediment%get_C_np_byMass()))) // ","
                ! Only include layer-by-layer breakdown if we've been asked to
                if (C%includeSedimentLayerBreakdown) then
                    write(102, '(*(a))', advance='no') (trim(str(sum(reach%bedSediment%get_C_np_l(l)))) // "," // &
                        trim(str(sum(reach%bedSediment%get_C_np_l_byMass(l)))) // ",", l=1, C%nSedimentLayers)
                end if
                write(102, '(a)') trim(str(sum(reach%bedSediment%get_m_np_buried() * reach%bedArea))) // "," // &
                    trim(str(reach%bedArea)) // "," // trim(str( .dp. reach%bedSediment%Mf_bed_all() * reach%bedArea))
            end associate
        end do
    end subroutine

    !> Update the sediment output file on the current timestep
    subroutine updateSoilDataOutput(me, t, x, y, date, easts, norths)
        class(DataOutput)   :: me               !! This DataOutput instance
        integer             :: t                !! The current timestep
        integer             :: x, y             !! Grid cell indices
        character(len=*)    :: date             !! Datetime of this timestep
        real                :: easts, norths    !! Eastings and northings of this grid cell
        integer             :: i, l             ! Iterators
        ! Loop through soil profiles and write row for each one
        do i = 1, me%env%item%colGridCells(x,y)%item%nSoilProfiles
            associate (profile => me%env%item%colGridCells(x,y)%item%colSoilProfiles(i)%item)
                write(103, '(a)', advance='no') trim(str(t)) // "," // trim(date) // "," // trim(str(x)) // "," // trim(str(y)) &
                    // "," // trim(str(easts)) // "," // trim(str(norths)) // "," // trim(str(i)) // "," // &
                    trim(profile%dominantLandUseName) // "," // trim(str(sum(profile%get_m_np()))) // "," // &
                    trim(str(sum(profile%get_m_transformed()))) // "," // trim(str(profile%get_m_dissolved())) // "," // &
                    trim(str(sum(profile%get_C_np()))) // "," // trim(str(sum(profile%get_C_transformed()))) // "," // &
                    trim(str(profile%get_C_dissolved())) // ","
                if (C%includeSoilStateBreakdown) then
                    write(103, '(a)', advance='no') trim(str(sum(freeNM(profile%get_C_np())))) // "," // &
                        trim(str(sum(freeNM(profile%get_C_transformed())))) // "," // &
                        trim(str(sum(attachedNM(profile%get_C_np())))) // "," // &
                        trim(str(sum(attachedNM(profile%get_C_transformed())))) // ","
                end if
                if (C%includeSoilLayerBreakdown) then
                    write(103, '(*(a))', advance='no') &
                        (trim(str(sum(profile%colSoilLayers(l)%item%C_np))) // "," // &
                         trim(str(sum(profile%colSoilLayers(l)%item%C_transformed))) // "," // &
                         trim(str(profile%colSoilLayers(l)%item%C_dissolved)) // ",", l = 1, C%nSoilLayers)
                    if (C%includeSedimentLayerBreakdown) then
                        write(103, '(*(a))', advance='no') &
                            (trim(str(sum(freeNM(profile%colSoilLayers(l)%item%C_np)))) // "," // &
                             trim(str(sum(freeNM(profile%colSoilLayers(l)%item%C_transformed)))) // ",", &
                             l = 1, C%nSoilLayers)
                        write(103, '(*(a))', advance='no') &
                            (trim(str(sum(attachedNM(profile%colSoilLayers(l)%item%C_np)))) // "," // &
                             trim(str(sum(attachedNM(profile%colSoilLayers(l)%item%C_transformed)))) // ",", &
                             l = 1, C%nSoilLayers)
                    end if
                end if
                write(103, '(a)') trim(str(sum(profile%m_np_eroded(:,:,2)))) // "," // &
                    trim(str(sum(profile%m_transformed_eroded(:,:,2)))) // "," // &
                    trim(str(sum(profile%m_np_buried))) // "," // &
                    trim(str(sum(profile%m_transformed_buried))) // "," // &
                    trim(str(profile%m_dissolved_buried)) // "," // &
                    trim(str(profile%bulkDensity))
            end associate
        end do
    end subroutine

    ! Finalise the data output by adding PECs to the simulation summary file and closing output files
    subroutine finaliseDataOutput(me)
        class(DataOutput)   :: me
        ! Write the final model summary info to the simulation summary file
        write(100, *) "\n## PECs"
        write(100, *) "- Soil, spatial mean on final timestep: " // &
            trim(str(sum(me%env%item%get_C_np_soil()))) // " kg/kg soil"
        write(100, *) "- Water, spatiotemporal mean: " // &
            trim(str(sum(sum(me%env%item%C_np_water_t, dim=1)) / size(me%env%item%C_np_water_t, dim=1))) // " kg/m3"
        write(100, *) "- Sediment, spatiotemporal mean: " // &
            trim(str(sum(sum(me%env%item%C_np_sediment_t, dim=1)) / size(me%env%item%C_np_sediment_t, dim=1))) // " kg/kg sediment"
        ! Close the files
        close(100); close(101); close(102); close(103)
    end subroutine

    ! Write the headers for the output files
    subroutine writeHeadersDataOutput(me)
        class(DataOutput)   :: me                   !! This DataOutput instance
        ! Write headers all of the output files
        call me%writeHeadersSimulationSummary()
        call me%writeHeadersWater()
        call me%writeHeadersSediment()
        call me%writeHeadersSoil()
    end subroutine

    !> Write headers for the simulation summary file, including basic info about the model run
    !! TODO currently doesn't work with batch runs
    subroutine writeHeadersSimulationSummaryDataOutput(me)
        class(DataOutput)   :: me                   !! This DataOutput instance
        type(datetime)      :: simDatetime          ! Datetime the simulation was run 
        type(datetime)      :: modelEndDate         ! End date of the model run

        ! Parse some datetimes
        simDatetime = simDatetime%now()
        modelEndDate = C%startDate + timedelta(C%nTimeSteps - 1)

        ! Summary file headers
        write(100, '(a)') "# NanoFASE model simulation summary"
        write(100, *) "- Description: " // trim(C%runDescription)
        write(100, *) "- Simulation datetime: " // simDatetime%isoformat()
        write(100, *) "- Is batch run? " // str(C%isBatchRun)
        write(100, *) "- Number of batches: " // str(C%nBatches)

        write(100, *) "\n## Temporal domain"
        write(100, *) "- Start date: " // C%startDate%strftime('%Y-%m-%d')
        write(100, *) "- End date: " // modelEndDate%strftime('%Y-%m-%d')
        write(100, *) "- Timestep length: " // trim(str(C%timeStep)) // " s"
        write(100, *) "- Number of timesteps: " // trim(str(C%nTimeSteps))
        
        write(100, *) "\n## Spatial domain"
        write(100, *) "- Grid resolution: " // trim(str(DATASET%gridRes(1))) // ", " // trim(str(DATASET%gridRes(2))) // " m" 
        write(100, *) "- Grid bounds: " // trim(str(DATASET%gridBounds(1))) // ", " // trim(str(DATASET%gridBounds(2))) // &
            ", " // trim(str(DATASET%gridBounds(3))) // ", " // trim(str(DATASET%gridBounds(4))) // " m"
        write(100, *) "- Grid shape: " // trim(str(DATASET%gridShape(1))) // ", " // trim(str(DATASET%gridShape(2)))
        write(100, *) "- Number of non-empty grid cells: " // trim(str(me%env%item%nGridCells))
    end subroutine

    !> Write the headers for the water output file
    subroutine writeHeadersWaterDataOutput(me)
        class(DataOutput)   :: me           !! This DataOutput instance
        
        ! Write metadata, if we're meant to 
        if (C%writeMetadataAsComment) then
            write(101, '(a)') "# NanoFASE model output data - water.\n# See simulation_summary.md for model run metadata."
            write(101, '(a)') "# Columns:\n#\tt: timestep index\n#\tdatetime: datetime of this timestep"
            write(101, '(a)') "#\tx, y: grid cell (eastings and northings) index"
            write(101, '(a)') "#\teasts, norths: eastings and northings at the centre of this grid cell (m)"
            write(101, '(a)') "#\tw: waterbody index within this grid cell"
            write(101, '(a)') "#\twaterbody_type: what type (river, estuary etc) is this waterbody?"
            write(101, '(a)') "#\tm_np(kg), m_transformed(kg), m_dissolved(kg): " // &
                "NM mass (untransformed, transformed and dissolved, kg)"
            write(101, '(a)') "#\tC_np(kg/m3), C_transformed(kg/m3), C_dissolved(kg/m3): NM concentration (kg/m3)"
            write(101, '(a)') "#\tm_np_outflow(kg), m_transformed_outflow(kg), m_dissolved_outflow(kg): " // &
                "downstream outflow NM masses (kg)"
            write(101, '(a)') "#\tm_np_deposited(kg), m_transformed_deposited(kg): mass of NM deposited (kg)"
            write(101, '(a)') "#\tm_spm(kg), C_spm(kg/m3): mass and concentration of SPM (kg, kg/m3)"
            write(101, '(a)') "#\tvolume(m3), depth(m), flow(m3/s): volume (m3), depth (m) and flow rate (m3/s) of this waterbody"
        end if
        ! Write the actual headers
        write(101, '(a)') "t,datetime,x,y,easts,norths,w,waterbody_type,m_np(kg),C_np(kg/m3)," // &
            "m_transformed(kg),C_transformed(kg/m3),m_dissolved(kg),C_dissolved(kg/m3)," // &
            "m_np_deposited(kg),m_transformed_deposited(kg),m_np_outflow(kg),m_transformed_outflow(kg)," // &
            "m_dissolved_outflow(kg),m_spm(kg),C_spm(kg/m3),volume(m3),depth(m),flow(m3/s)"
    end subroutine

    !> Write the headers for the sediment output file
    subroutine writeHeadersSedimentDataOutput(me)
        class(DataOutput)   :: me           !! This DataOutput instance
        integer             :: i            ! Iterator

        ! Write metadata, if we're meant to
        if (C%writeMetadataAsComment) then
            write(102, '(a)') "# NanoFASE model output data - sediment."
            write(102, '(a)') "# See simulation_sumamry.md for model run metadata."
            write(102, '(a)') "#\tx, y: grid cell (eastings and northings) index"
            write(102, '(a)') "#\teasts, norths: eastings and northings at the centre of this grid cell (m)"
            write(102, '(a)') "#\tw: waterbody index within this grid cell"
            write(102, '(a)') "#\twaterbody_type: what type (river, estuary etc) of waterbody is this sediment in?"
            write(102, '(a)') "#\tm_np_total(kg), C_np_total(kg/m3), C_np_total(kg/kg): " &
                // "NM mass (kg) and concentration (kg/m3 and kg/kg dry weight) for all layers"
            if (C%includeSedimentLayerBreakdown) then
                write(102, '(a)') "#\tC_np_li(kg/m3), C_np_li(kg/kg): NM conc for layer i (kg/m3 and kg/kg)"
            end if
            write(102, '(a)') "#\tm_np_buried(kg): NM mass buried on this timestep (kg)"
            write(102, '(a)') "#\tbed_area(m2): area of this bed sediment (m2)"
            write(102, '(a)') "#\tsediment_mass(kg): total mass of fine sediment in this bed sediment (kg, *not* kg/m2)"
        end if
        ! Write the actual headers
        write(102, '(a)', advance="no") "t,datetime,x,y,easts,norths,w,waterbody_type," // &
            "m_np_total(kg),C_np_total(kg/m3),C_np_total(kg/kg),"
        ! Should we include sediment layer breakdown?
        if (C%includeSedimentLayerBreakdown) then
            write(102, '(*(a))', advance="no") &
                ("C_np_l" // trim(str(i)) // "(kg/m3),C_np_l" // trim(str(i)) // "(kg/kg),", i = 1, C%nSedimentLayers) 
        end if
        write(102, '(a)') "m_np_buried(kg),bed_area(m2),sediment_mass(kg)"
    end subroutine

    !> Write the headers for the soil output file
    subroutine writeHeadersSoilDataOutput(me)
        class(DataOutput)   :: me           !! This DataOutput instance
        integer             :: i            ! Iterator

        if (C%writeMetadataAsComment) then
            write(103, '(a)') "# NanoFASE model output data - soil."
            write(103, '(a)') "# See simulation_summary.md for model run metadata."
            write(103, '(a)') "# Columns:\n#\tt: timestep index\n#\tdatetime: datetime of this timestep"
            write(103, '(a)') "#\tx, y: grid cell (eastings and northings) index"
            write(103, '(a)') "#\teasts, norths: eastings and northings at the centre of this grid cell (m)"
            write(103, '(a)') "#\tp: soil profile index within this cell"
            write(103, '(a)') "#\tland_use: dominant land use of this soil profile"
            write(103, '(a)') "#\tm_np_total(kg), m_transformed_total(kg), m_dissolved_total(kg): " // &
                "NM mass (untransformed, transformed and dissolved) in whole soil profile, sum of free and attached NM"
            write(103, '(a)') "#\tC_np_total(" // C%soilPECUnits // "), C_transformed_total(" // C%soilPECUnits // &
                "), C_dissolved_total(" // C%soilPECUnits // "): " // &
                "NM concentration averaged over all soil layers, sum of free and attached NM"
            ! Should we include a breakdown of NM state (free vs attached)?
            if (C%includeSoilStateBreakdown) then
                write(103, '(a)') "#\tC_np_free(" // C%soilPECUnits // "), C_transformed_free(" // C%soilPECUnits // &
                    "): free NM concentration averaged over all soil layers"
                write(103, '(a)') "#\tC_np_att(" // C%soilPECUnits // "), C_transformed_att(" // C%soilPECUnits // &
                    "): attached NM concentration averaged over all soil layers"
            end if
            ! Should we include a breakdown across the soil layers?
            if (C%includeSedimentLayerBreakdown) then
                write(103, '(a)') "#\tC_np_li(" // C%soilPECUnits // "), C_transformed_li(" // C%soilPECUnits // &
                "), C_dissolved_li(" // C%soilPECUnits // "): NM concentration for layer i, sum of free and attached"
                if (C%includeSoilStateBreakdown) then
                    write(103, '(a)') "#\tC_np_free_li(" // C%soilPECUnits // "), C_transformed_free_li(" // C%soilPECUnits // &
                        "): free NM concentration for layer i"
                    write(103, '(a)') "#\tC_np_att_li(" // C%soilPECUnits // "), C_transformed_att_li(" // C%soilPECUnits // &
                        "): attached NM concentration for layer i"
                end if
            end if
            write(103, '(a)') "#\tm_np_eroded(kg), m_transformed_eroded(kg): mass of NM eroded on this timestep"
            write(103, '(a)') "#\tm_np_buried(kg), m_transformed_buried(kg), m_dissolved_buried(kg): " // &
                "mass of NM buried on this timestep"
            write(103, '(a)') "#\tbulk_density(kg/m3): bulk density of this soil profile"
        end if
        ! Write the actual headers
        write(103, '(a)', advance="no") "t,datetime,x,y,easts,norths,p,land_use," // &
            "m_np_total(kg),m_transformed_total(kg),m_dissolved_total(kg)," // &
            "C_np_total(" // C%soilPECUnits // "),C_transformed_total(" // C%soilPECUnits // &
            "),C_dissolved_total(" // C%soilPECUnits // "),"
        ! Should we include state breakdown - free vs attached?
        if (C%includeSoilStateBreakdown) then
            write(103, '(a)', advance="no") "C_np_free("//C%soilPECUnits//"),C_transformed_free("//C%soilPECUnits//")," // &
                "C_np_att("//C%soilPECUnits//"),C_transformed_att("//C%soilPECUnits//"),"
        end if
        ! Should we include soil layer breakdown?
        if (C%includeSoilLayerBreakdown) then
            write(103, '(*(a))', advance="no") &
                ("C_np_l"//trim(str(i))//"("//C%soilPECUnits//"),C_transformed_l"//trim(str(i))//"("//C%soilPECUnits//"),"// &
                 "C_dissolved_l"//trim(str(i))//"("//C%soilPECUnits//"),", i = 1, C%nSoilLayers)
            if (C%includeSoilStateBreakdown) then
                write(103, '(*(a))', advance="no") &
                    ("C_np_free_l"//trim(str(i))//"("//C%soilPECUnits//"),C_transformed_free_l"//trim(str(i))// &
                     "("//C%soilPECUnits//"),", i = 1, C%nSoilLayers)
                write(103, '(*(a))', advance="no") &
                    ("C_np_att_l"//trim(str(i))//"("//C%soilPECUnits//"),C_transformed_att_l"//trim(str(i))// &
                     "("//C%soilPECUnits//"),", i = 1, C%nSoilLayers)
            end if
        end if
        write(103, '(a)', advance="no") "m_np_eroded(kg),m_transformed_eroded(kg),m_np_buried(kg),m_transformed_buried(kg),"
        write(103, '(a)') "m_dissolved_buried(kg),bulk_density(kg/m3)"
    end subroutine

end module