# Conceptual structure

Internally, the model represents the environment as a series of objects and their relationships, each representing different elements in the environment (rivers, soils, biota, etc) and their relationships. This can be represented on a UML diagram:

```{mermaid}
classDiagram
    direction LR
    Environment --o "1..*" GridCell
    GridCell --o "1..*" SoilProfile
    GridCell --o "1..8" WaterBody
    WaterBody <|-- Reach
    Reach <|-- RiverReach
    Reach <|-- EstuaryReach
    SoilProfile --o "1..*" SoilLayer
    WaterBody --o "1..*" Biota
    WaterBody --o "1" BedSediment
    BedSediment --o "1..*" BedSedimentLayer
    WaterBody --o "1" Reactor
    SoilLayer --o "1" Reactor
    BedSedimentLayer --o "1" Reactor
```

Here, `Environment` represents the geographical area that we wish to model (e.g. a river catchment), which is divided into a number of `GridCell`s to give spatial resolution. Each `GridCell` can one or more `SoilProfile`s, which are split vertically into one or more `SoilLayer`s to give vertical resolution down the soil profile. `GridCell`s can also contain up to eight `WaterBody` objects (see [](surface-water-network)), each of which is an abstraction of a specific type of waterbody, such a `RiverReach` or `EstuaryReach`. Unlike `SoilProfile`s, `WaterBody` objects are linked across `GridCell`s to model the flow of water, sediment and contaminants around the environment. Each `WaterBody` contains a `BedSediment`, which is further split into a vertical distribution of `BedSedimentLayer`s. The final object is the `Reactor`, which is responsible for modelling the physical and chemical state of the contaminant being modelled.

(surface-water-network)=
## Surface water network

*To be completed...*