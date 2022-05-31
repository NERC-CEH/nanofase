# Conceptual structure

Internally, the model represents the environment as a series of objects and their relationships, each representing different elements in the environment (rivers, soils, biota, etc) and their relationships. This can be represented on a UML diagram:

```mermaid
classDiagram
    Environment --o "1..*" GridCell
    GridCell --o "1..*" SoilProfile
    GridCell --o "1..*" WaterBody
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