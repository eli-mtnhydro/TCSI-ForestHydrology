Sep. 20, 2022
Using sourcecode emailed by Zhuoran
Implements use of canopy height maps (overstory and understory)
Implements use of dynamic vegetation maps

Nov. 1, 2022
Edited MakeLocalMetData.c to directly multiply precip values by the PRISM map
(Redistributes precip but preserves mass balance of original gridMet data)

Dec. 29, 2022
Edited SnowInterception.c to implement LAI-based snow interception

Jan. 18, 2023
Edited the following files to implement use of soil field capacity maps (3 layers)
Copied edits from https://github.com/pnnl/DHSVM-PNNL/commit/1531b84270bf5785ece01fdb2a30f991c848cffa?diff=split
CanopyGapEnergyBalance.c
InitModelState.c
*InitTerrainMaps.c
MainDHSVM.c
MassEnergyBalance.c
RouteSubSurface.c
*VarID.c
data.h
settings.h
*Note that the variable ID for Soil.FCap was set to 015 instead of 014 since 014 was already used for Veg.Height

Jan. 18, 2023
Edited Aggregate.c to fix conversion of EPot from m/s to m
Copied edits from https://github.com/pnnl/DHSVM-PNNL/commit/902bb2a975f0f3faf33b311755b4cbabef61108c

Feb. 19, 2023
Edited settings.h to define MELTOUT_SWE as 0.05 m (for use with Margulis SWE data)
Edited SnowStats.c to incorporate the above change (replaced MIN_SWE with MELTOUT_SWE on line 69)