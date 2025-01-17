This README was generated on 2025-01-17 by Stefan Vriend

# Title
Bud burst data of department of Animal Ecology/NIOO-KNAW

## Creator
Organisation: Department of Animal Ecology, Netherlands Institute of Ecology (NIOO-KNAW)

## Contact
Role: Databank manager
Organisation: Department of Animal Ecology, Netherlands Institute of Ecology (NIOO-KNAW)
Email address: AnE_Database@nioo.knaw.nl

## Description
The stage of bud burst has been measured from 1988 onwards on several tree species in different areas across the Netherlands to record the phenology of the trees.

## Keywords
bud burst, trees, ecology, plant phenology, oak

## Temporal coverage
Start date: 1988-04-21
End date: 2023-05-21

## Spatial coverage
Description: Several sites across the Netherlands have been sampled: The Nationalpark Hoge Veluwe, Oosterhout, Warnsborn, Doorwerth, Bennekom, Buunderkamp, Wolfheze, Rhene, Heveadorp, Goffert, Kernhem, Loenen
Geographic coordinates: 5.574453, 51.821770, 6.019378, 52.116720 (WGS84, EPSG:4326)

## Taxonomic coverage
Quercus robur, Quercus rubra, Betula pendula, Larix kaempferi, Pinus sylvestris, other vascular plants

## License
https://creativecommons.org/licenses/by/4.0/

## Data file description
- tbl_area.csv: table with information on study sites. AreaID links to tbl_area.csv to tbl_budburst.csv.
- tbl_budburst.csv: table with information on bud burst observation, including date, bud burst score per tree, and observer id
- tbl_tree.csv: table with information on individual trees and geographic location (elevation and coordinates). TreeID links tbl_tree.csv to tbl_budburst.csv.
- tbl_treeSpecies.csv: table with taxonomic information. TreeSpeciesID links tbl_treeSpecies.csv to tbl_tree.csv.