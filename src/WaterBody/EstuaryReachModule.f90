module EstuaryReachModule
    use GlobalsModule
    use ReachModule
    use UtilModule
    use ResultModule
    use BedSedimentModule
    use LoggerModule, only: LOGR
    use ReactorModule
    use ContaminantModule
    use DataInputModule, only: DATASET  
    implicit none

    type, public, extends(Reach) :: EstuaryReach
        real(dp) :: meanDepth           !! Mean estuary depth for use in tidal depth calculations [m]
        real(dp) :: distanceToMouth     !! Distance to the mouth of the estuary [m]
        real(dp) :: tidalM2             !! Tidal harmonic coefficient M2 [-]
        real(dp) :: tidalS2             !! Tidal harmonic coefficient S2 [-]
      contains
        procedure :: create => createEstuaryReach
        procedure :: update => updateEstuaryReach
        procedure :: updateDisplacement => updateDisplacementEstuaryReach
        procedure :: setDimensions
        procedure :: parseInputData => parseInputDataEstuaryReach
        procedure :: calculateDepth => calculateDepth
        procedure :: calculateVelocity => calculateVelocity
        procedure :: calculateDistanceToMouth => calculateDistanceToMouth
        procedure :: changeInVolume => changeInVolume
        procedure :: finalise => finaliseEstuaryReach  
    end type

contains

    function createEstuaryReach(me, x, y, w, distributionSediment) result(rslt)
        class(EstuaryReach), intent(inout)     :: me
        integer,           intent(in)           :: x, y, w
        real(dp),        intent(in)           :: distributionSediment(C%nSizeClassesSPM)
        type(Result)                          :: rslt
        integer                               :: i

        call rslt%addErrors(.errors. me%WaterBody%create(x, y, w, distributionSediment))
        me%ref = trim(ref("EstuaryReach", x, y, w))

        call rslt%addErrors(.errors. me%parseInputData())
        call rslt%addErrors(.errors. me%m_contaminant%create_from_data( &
            'estuary', &
            DATASET%contaminantDensity, &
            DATASET%soilAttachmentEfficiencyConstant, &
            DATASET%riverAttachmentEfficiency, &
            DATASET%estuaryAttachmentEfficiency, &
            DATASET%contaminant_k_diss_pristine, &
            DATASET%contaminant_k_diss_transformed, &
            DATASET%contaminant_k_transform_pristine, &
            DATASET%waterTemperature(C%startDate%yearday()) &
        ))

        call me%setDimensions(0)

        ! Allocate and zero SPM carrier arrays so reactor%create receives valid arrays.
        ! These are updated each timestep in updateEstuaryReach; zero is correct for initialisation.
        if (allocated(me%C_spm)) deallocate(me%C_spm)
        allocate(me%C_spm(C%nSizeClassesSpm))
        me%C_spm = 0.0_dp

        if (allocated(me%W_settle_spm)) deallocate(me%W_settle_spm)
        allocate(me%W_settle_spm(C%nSizeClassesSpm))
        me%W_settle_spm = 0.0_dp

        allocate(BedSediment :: me%bedSediment)
        allocate(Reactor :: me%reactor)
        call rslt%addErrors([ &
            .errors. me%bedSediment%create(me%x, me%y, me%w), &
            .errors. me%reactor%create( &
                me%x, me%y, 'estuary', &
                me%m_contaminant, me%volume, &
                DATASET%waterTemperature(C%startDate%yearday()), &
                C_spm=me%C_spm, W_settle_spm=me%W_settle_spm, &
                G=DATASET%shearRate, velocity=me%velocity) &
        ])

        allocate(me%biotaIndices(0))
        if (DATASET%hasBiota) then
            do i = 1, DATASET%nBiota
                if (trim(DATASET%biotaCompartment(i)) == 'water') then
                    me%nBiota = me%nBiota + 1
                    me%biotaIndices = [me%biotaIndices, i]
                end if
            end do
        end if
        allocate(me%biota(me%nBiota))
        do i = 1, me%nBiota
            call rslt%addErrors(.errors. me%biota(i)%create(me%biotaIndices(i)))
        end do

        call rslt%addToTrace('Creating ' // trim(me%ref))
        call LOGR%toFile("Creating " // trim(me%ref) // ": success")
    end function

    subroutine updateEstuaryReach(me, t, q_runoff, q_overland, j_spm_runoff, j_contaminant_runoff, &
                                  contributingArea, isWarmUp)
        class(EstuaryReach), intent(inout) :: me
        integer, intent(in) :: t
        real(dp), intent(in) :: q_runoff, q_overland
        real(dp), intent(in) :: j_spm_runoff(:)
        type(Contaminant), intent(in) :: j_contaminant_runoff
        real(dp), intent(in) :: contributingArea
        logical, intent(in) :: isWarmUp
        type(Result) :: rslt
        real(dp) :: changeInVolume
        real(dp) :: Q_outflow                   ! Provisional outflow, used only to decide the sense of the tide
        real(dp) :: j_spm_in_total(C%nSizeClassesSpm)
        type(Contaminant) :: j_contaminant_in_total
        integer :: i, nDisp
        real(dp) :: dt, dQ_in
        real(dp) :: dj_spm_erosion(C%nSizeClassesSpm)
        real(dp) :: dj_spm_inflow(C%nSizeClassesSpm)
        type(Contaminant) :: dj_contaminant_erosion_sources, dj_contaminant_inflow
        type(datetime) :: currentDate
        real(dp) :: T_water_t

        call me%emptyFlows()

        currentDate = C%startDate + timedelta(t-1)
        T_water_t = me%T_water(currentDate%yearday())

        ! Outflows are stored as negative, so subtract to accumulate a positive inflow
        do i = 1, me%nInflows
            me%Q%inflow = me%Q%inflow - me%inflows(i)%item%Q_final%outflow
            me%j_spm%inflow = me%j_spm%inflow - me%inflows(i)%item%j_spm_final%outflow
            call me%j_contaminant_inflow%add(me%inflows(i)%item%get_j_contaminant_outflow())
        end do

        me%Q%runoff = q_runoff * contributingArea

        if (.not. C%ignoreContaminant .and. .not. isWarmUp) then
            call me%updateSources(t)
        end if

        call me%setDimensions((t-1) * C%timeStep / C%minEstuaryTimestep)
        changeInVolume = me%changeInVolume((t-1)*24, t*24)
        ! Provisional outflow, telling us the direction of the tide: +ve is upstream (incoming) tidal
        ! flow, -ve is downstream. This must go in a local, NOT in me%Q%outflow: the displacement
        ! loop below accumulates dQ_out into me%Q%outflow, and since the accumulated total is this
        ! same quantity, storing it here as well doubles the outflow
        Q_outflow = changeInVolume - me%Q%inflow - me%Q%runoff - me%Q%transfers
        me%Q_in_total = me%Q%runoff + me%Q%transfers
        j_spm_in_total = me%j_spm%soilErosion + me%j_spm%transfers
        call rslt%addErrors(.errors. j_contaminant_in_total%create())
        call j_contaminant_in_total%add(j_contaminant_runoff)
        call j_contaminant_in_total%add(me%j_contaminant_transfers)
        call j_contaminant_in_total%add(me%j_contaminant_pointSources)
        call j_contaminant_in_total%add(me%j_contaminant_diffuseSources)
        ! NOTE: me%Q%outflow / me%j_spm%outflow / me%j_contaminant_outflow are all still zero here
        ! (emptyFlows above), so these three lines currently add nothing.
        ! TODO should this use Q_outflow?
        if (Q_outflow > 0) then
            me%Q_in_total = me%Q_in_total + me%Q%outflow
            j_spm_in_total = j_spm_in_total + me%j_spm%outflow
            call j_contaminant_in_total%add(me%j_contaminant_outflow)
        end if
        if (me%Q%inflow > 0.0_dp) then
            me%Q_in_total = me%Q_in_total + me%Q%inflow
            j_spm_in_total = j_spm_in_total + me%j_spm%inflow
            call j_contaminant_in_total%add(me%j_contaminant_inflow)
        end if
        me%velocity = me%calculateVelocity(me%depth, me%Q_in_total/C%timeStep, me%width)

        call me%setErosionYields(j_spm_runoff, q_overland, contributingArea, j_contaminant_runoff)

        call me%setResuspensionRate(me%Q_in_total / C%timeStep, T_water_t)
        call me%setSettlingRate(T_water_t)

        if (isZero(me%Q_in_total) .or. isZero(me%volume)) then
            nDisp = C%timeStep / C%minEstuaryTimestep
        else
            nDisp = max(ceiling(me%Q_in_total / me%volume), C%timeStep / C%minEstuaryTimestep)
        end if
        dt = C%timeStep / nDisp
        dQ_in = me%Q_in_total / nDisp
        dj_spm_erosion = (me%j_spm%soilErosion + me%j_spm%bankErosion) / nDisp
        dj_spm_inflow = me%j_spm%inflow / nDisp
        call rslt%addErrors(.errors. dj_contaminant_erosion_sources%create())
        call dj_contaminant_erosion_sources%multiply_scalar(j_contaminant_runoff, 1.0_dp/nDisp)
        call dj_contaminant_erosion_sources%add_scaled(me%j_contaminant_pointSources, 1.0_dp/nDisp)
        call dj_contaminant_erosion_sources%add_scaled(me%j_contaminant_diffuseSources, 1.0_dp/nDisp)
        call rslt%addErrors(.errors. dj_contaminant_inflow%create())
        call dj_contaminant_inflow%multiply_scalar(me%j_contaminant_inflow, 1.0_dp/nDisp)

        do i = 1, nDisp
            call me%updateDisplacement(t, i, dt, dQ_in, dj_spm_erosion, dj_spm_inflow, &
                                      dj_contaminant_erosion_sources, dj_contaminant_inflow, T_water_t)
        end do

        me%C_spm = divideCheckZero(me%m_spm, me%volume)

        ! Update the reactor with the total inflow contaminant mass (partitioning, transformation, foam, atmosphere).
        ! IMPORTANT: reactor%update must be called BEFORE j_contaminant_in_total is finalised,
        ! because the reactor optionally adds that inflow mass to me%m_contaminant via its pointer.
        if (.not. C%ignoreContaminant .and. .not. isZero(me%volume)) then
            call rslt%addErrors(.errors. me%reactor%update(j_contaminant_in_total, dt))
            ! me%reactor%contaminant IS a pointer to me%m_contaminant, so no copy is needed.
            if (me%volume > 0.0_dp) then
                me%C_dissolved = me%m_contaminant%m_dissolved / me%volume
            else
                me%C_dissolved = 0.0_dp
            end if
        end if

        call j_contaminant_in_total%finalise()
        call dj_contaminant_erosion_sources%finalise()
        call dj_contaminant_inflow%finalise()

        do i = 1, me%nBiota
            call rslt%addErrors(.errors. me%biota(i)%update(t, me%m_contaminant%divideCheckZero(me%volume)))
        end do

        call me%finaliseUpdate()

        call rslt%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
        call LOGR%toFile(errors = .errors. rslt)
        call ERROR_HANDLER%trigger(errors = .errors. rslt)
    end subroutine

    subroutine updateDisplacementEstuaryReach(me, t, d, dt, dQ_in, dj_spm_erosion, dj_spm_inflow, &
                                              dj_contaminant_erosion_sources, dj_contaminant_inflow, T_water)
        class(EstuaryReach), intent(inout) :: me
        integer, intent(in) :: t, d
        real(dp), intent(in) :: dt, dQ_in
        real(dp), intent(in) :: dj_spm_erosion(:), dj_spm_inflow(:)
        type(Contaminant), intent(in) :: dj_contaminant_erosion_sources, dj_contaminant_inflow
        real(dp), intent(in) :: T_water
        real(dp) :: dQ_out, changeInVolume
        real(dp) :: dj_spm_out(C%nSizeClassesSpm)
        type(Contaminant) :: dj_contaminant_out
        real(dp) :: dj_spm_in(C%nSizeClassesSpm)
        type(Contaminant) :: dj_contaminant_in
        real(dp) :: dj_spm_deposit(C%nSizeClassesSpm), dj_spm_resus(C%nSizeClassesSpm)
        real(dp) :: dj_spm_deposit_perArea(C%nSizeClassesSpm), dj_spm_resus_perArea(C%nSizeClassesSpm)
        real(dp) :: tmp_dj_spm_resus_perArea(C%nSizeClassesSpm)
        type(Contaminant) :: dj_contaminant_deposit, dj_contaminant_resus
        type(Result) :: rslt
        type(Result0D) :: res_contaminant
        type(Contaminant) :: m_contaminant

        call rslt%addErrors(.errors. dj_contaminant_out%create())
        call rslt%addErrors(.errors. dj_contaminant_in%create())
        call rslt%addErrors(.errors. dj_contaminant_deposit%create())
        call rslt%addErrors(.errors. dj_contaminant_resus%create())

        call me%setDimensions((t-1)*C%timeStep/3600 + d*(int(dt)/3600))
        changeInVolume = me%changeInVolume((t-1)*24 + (d-1)*(int(dt)/3600), (t-1)*24 + d*(int(dt)/3600))
        dQ_out = -dQ_in + changeInVolume
        call me%setResuspensionRate(abs(dQ_out) / dt, T_water)

        if (dQ_out < 0 .and. .not. isZero(me%volume)) then
            dj_spm_out = max(me%m_spm * dQ_out / me%volume, -me%m_spm)
            call dj_contaminant_out%multiply_scalar(me%m_contaminant, dQ_out / me%volume)
            dj_spm_in = dj_spm_erosion + dj_spm_inflow
            call dj_contaminant_in%add(dj_contaminant_erosion_sources)
            call dj_contaminant_in%add(dj_contaminant_inflow)
        else if (dQ_out > 0 .and. associated(me%outflow%item)) then
            dj_spm_out = min(me%outflow%item%C_spm_final * dQ_out, me%outflow%item%m_spm / me%outflow%item%nInflows)
            call dj_contaminant_out%multiply_scalar(me%outflow%item%m_contaminant, dQ_out / me%outflow%item%volume)
            dj_spm_in = dj_spm_erosion + dj_spm_inflow - min(me%m_spm * dQ_out / me%volume, me%m_spm)
            call dj_contaminant_in%add(dj_contaminant_erosion_sources)
            call dj_contaminant_in%add(dj_contaminant_inflow)
            call dj_contaminant_in%add_scaled(me%m_contaminant, -dQ_out / me%volume)
        else
            dj_spm_out = 0.0_dp
            call dj_contaminant_out%multiply_scalar(me%m_contaminant, 0.0_dp)
            dj_spm_in = dj_spm_erosion + dj_spm_inflow
            call dj_contaminant_in%add(dj_contaminant_erosion_sources)
            call dj_contaminant_in%add(dj_contaminant_inflow)
        end if

        me%m_spm = flushToZero(max(me%m_spm + dj_spm_in - dj_spm_out, 0.0_dp))
        call me%m_contaminant%add(dj_contaminant_in)
        call me%m_contaminant%add_scaled(dj_contaminant_out, -1.0_dp)

        dj_spm_deposit = min(me%k_settle * dt * me%m_spm, me%m_spm)
        dj_spm_resus = me%k_resus * me%bedSediment%Mf_bed_by_size() * dt

        dj_spm_deposit_perArea = divideCheckZero(dj_spm_deposit, me%bedArea)
        dj_spm_resus_perArea = divideCheckZero(dj_spm_resus, me%bedArea)
        tmp_dj_spm_resus_perArea = dj_spm_resus_perArea

        if (C%includeBedSediment) then
            call rslt%addErrors(.errors. me%bedSediment%resuspend(tmp_dj_spm_resus_perArea))
            dj_spm_resus_perArea = dj_spm_resus_perArea - tmp_dj_spm_resus_perArea
            call rslt%addErrors(.errors. me%depositToBed(dj_spm_deposit_perArea))
            if (.not. C%ignoreContaminant) then
                call dj_contaminant_deposit%multiply_scalar(me%m_contaminant, sum(me%k_settle * dt))
                res_contaminant = me%bedSediment%get_m_contaminant()
                if (res_contaminant%hasError()) then
                    call rslt%addErrors(res_contaminant%getErrors())
                    call LOGR%toFile(errors = .errors. rslt)
                    call ERROR_HANDLER%trigger(errors = .errors. rslt)
                    return
                end if
                select type (data => res_contaminant%data)
                    type is (Contaminant)
                        m_contaminant = data
                    class default
                        call rslt%addError(ErrorInstance(code=106, message="Invalid data type in Result0D"))
                        call LOGR%toFile(errors = .errors. rslt)
                        call ERROR_HANDLER%trigger(errors = .errors. rslt)
                        return
                end select
                call dj_contaminant_resus%multiply_scalar(m_contaminant, sum(me%k_resus * dt))
                call rslt%addErrors(.errors. me%bedSediment%transferContaminant(dj_contaminant_deposit))
            end if
        end if

        me%Q%outflow = me%Q%outflow + dQ_out
        me%j_spm%outflow = me%j_spm%outflow + dj_spm_out
        call me%j_contaminant_outflow%add(dj_contaminant_out)
        me%j_spm%deposition = me%j_spm%deposition - dj_spm_deposit
        me%j_spm%resuspension = me%j_spm%resuspension + dj_spm_resus
        call me%j_contaminant_deposition%add_scaled(dj_contaminant_deposit, -1.0_dp)
        call me%j_contaminant_resuspension%add(dj_contaminant_resus)

        call dj_contaminant_out%finalise()
        call dj_contaminant_in%finalise()
        call dj_contaminant_deposit%finalise()
        call dj_contaminant_resus%finalise()

        call rslt%addToTrace("Updating time displacement #" // trim(str(d)))
        call rslt%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
        call LOGR%toFile(errors = .errors. rslt)
        call ERROR_HANDLER%trigger(errors = .errors. rslt)
    end subroutine

    subroutine setDimensions(me, tHours)
        class(EstuaryReach), intent(inout) :: me
        integer, intent(in) :: tHours
        me%depth = me%calculateDepth(tHours)
        me%xsArea = me%depth * me%width
        me%bedArea = me%width * me%length * me%f_m
        me%surfaceArea = me%bedArea
        me%volume = me%depth * me%width * me%length * me%f_m
    end subroutine

    function changeInVolume(me, tStart, tFinal) result(volChange)
        class(EstuaryReach), intent(in) :: me
        integer, intent(in) :: tStart, tFinal
        real(dp) :: volChange
        volChange = (me%calculateDepth(tFinal) - me%calculateDepth(tStart)) * &
                    me%width * me%length * me%f_m
    end function

    function parseInputDataEstuaryReach(me) result(rslt)
        class(EstuaryReach), intent(inout) :: me
        type(Result) :: rslt
        me%distanceToMouth = me%calculateDistanceToMouth(DATASET%x(me%x), DATASET%y(me%y), &
                                                        DATASET%estuaryMeanderingFactor, &
                                                        DATASET%estuaryMouthCoords(1), &
                                                        DATASET%estuaryMouthCoords(2))
        me%width = DATASET%estuaryWidthExpA * exp(-DATASET%estuaryWidthExpB * me%distanceToMouth)
        me%meanDepth = DATASET%estuaryMeanDepthExpA * exp(-DATASET%estuaryMeanDepthExpB * me%distanceToMouth)
        me%f_m = DATASET%estuaryMeanderingFactor
        me%alpha_hetero = DATASET%estuaryAttachmentEfficiency
        me%alpha_resus = DATASET%resuspensionAlpha(me%x, me%y)
        me%beta_resus = DATASET%resuspensionBeta(me%x, me%y)
        me%a_stc = DATASET%sedimentTransport_a(me%x, me%y)
        me%b_stc = DATASET%sedimentTransport_b(me%x, me%y)
        me%c_stc = DATASET%sedimentTransport_c(me%x, me%y)
        me%T_water = DATASET%waterTemperature
        call rslt%addErrors(.errors. me%parseInflowsAndOutflow())
        call me%setReachLengthAndSlope()
        call rslt%addToTrace('Parsing input data')
    end function

    function calculateDistanceToMouth(me, x, y, f, x_mouth, y_mouth) result(distanceToMouth)
        class(EstuaryReach), intent(in) :: me
        real, intent(in) :: x, y, f, x_mouth, y_mouth
        real :: distanceToMouth
        distanceToMouth = f * sqrt((x_mouth - x)**2 + (y_mouth - y)**2)
    end function

    function calculateDepth(me, tHours) result(depth)
        class(EstuaryReach), intent(in) :: me
        integer, intent(in) :: tHours
        real(dp) :: depth
        depth = DATASET%estuaryTidalS2 * cos(2.0_dp*C%pi*tHours/12.0_dp) + &
                DATASET%estuaryTidalM2 * cos(2.0_dp*C%pi*tHours/12.42_dp) + &
                (0.75_dp) * ((me%distanceToMouth * DATASET%estuaryTidalM2 ** 2) / &
                (me%meanDepth * 22356.0_dp * sqrt(9.81_dp * me%meanDepth))) * &
                cos(2*C%pi*tHours/6.21_dp) + me%meanDepth
        if (depth < 0) depth = 0.0_dp
    end function

    function calculateVelocity(me, D, Q, W) result(v)
        class(EstuaryReach), intent(in) :: me
        real(dp), intent(in) :: D, Q, W
        real(dp) :: v
        if (isZero(Q) .or. isZero(W) .or. isZero(D)) then
            v = 0.0_dp
        else
            v = Q / (W * D)
        end if
    end function

    subroutine finaliseEstuaryReach(me)
        class(EstuaryReach), intent(inout) :: me
        integer :: i
        call me%WaterBody%finalise()
        if (allocated(me%biota)) then
            do i = 1, me%nBiota
                call me%biota(i)%finalise()
            end do
            deallocate(me%biota)
        end if
        if (allocated(me%biotaIndices)) deallocate(me%biotaIndices)
        if (allocated(me%bedSediment)) then
            call me%bedSediment%finalise()
            deallocate(me%bedSediment)
        end if
        if (allocated(me%reactor)) then
            call me%reactor%finalise()
            deallocate(me%reactor)
        end if
    end subroutine
end module