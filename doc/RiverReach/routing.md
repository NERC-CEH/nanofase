# RiverReach Routing

Routing of a RiverReach's water and SPM is performed by the [`update`](src/River/RiverReach/classRiverReach1.f08#L102) method. This method is called from the SubRiver, which is responsible from summing the outflow(s) from the previous RiverReach(es) and passing them to this RiverReach via the inflow `Qin` and SPM inflow `spmIn` parameters.