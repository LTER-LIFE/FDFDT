This data dictionary was created on 2025-04-25 by Stefan Vriend.

Dataset: https://doi.org/10.34894/5SOKTV

`tbl_area.csv`

|column|description|data type|values|required|
|:---|:-----|:---|:---|:---|
|AreaID|Unique ID of study area|integer|1--23|true|
|AreaShortName|Abbreviated name or code of study area|character||false|
|AreaName|Full name of study area|character||true|
|AreaID_GT|ID of study area as used in other database|integer|1–; missing values: NA|false|


`tbl_budburst.csv`

|column|description|data type|values|required|
|:---|:-----|:---|:---|:---|
|BudburstID|Unique ID of bud burst observation|integer|1--99732|true|
|Year|Calendar year of bud burst observation|integer|1988--2023|true|
|TreeID|ID of tree for which bud burst was recorded|integer|1--1054|true|
|AprilDate|Date of bud burst observation in number of days since 1 April|integer|-11--81|true|
|Day|Calendar day of bud burst observation|integer|1--31|true|
|Month|Calendar month of bud burst observation|integer|1--12|true|
|TreeTopScore|Score indicating bud burst stage of tree crown|float|0--3; with intervals of 0.25; missing values: NA|true|
|TreeAllScore|Score indicating bud burst stage of total tree|float|0--3; with intervals of 0.25; missing values: NA|true|
|Observer|Unique ID of observer|integer|53, 58, 162, 461; missing values: NA|false|

`tbl_tree.csv`

|column|description|data type|values|required|
|:---|:-----|:---|:---|:---|
|TreeID|ID of tree for which bud burst was recorded|integer|1--1054|true|
|AreaID|Unique ID of study area|integer|1--23|true|
|SiteNumber|Number of site nested within AreaID|integer|1--59|true|
|TreeNumber|Number of tree nested within SiteNumber|integer|0--106|true|
|TreeSpeciesID|ID of tree species|integer|1--6|true|
|Elevation|Elevation of tree location in metres above standard mean sea level|float|11.01--50.77; missing values: NA|false|
|Latitude|Latitude (north-south position) of tree location in decimal degrees|float|51.87--52.91; missing values: NA|false|
|Longitude|Longitude (east-west position) of tree location in decimal degrees|float|5.772--6.484; missing values: NA|false|

`tbl_treeSpecies.csv`

|column|description|data type|values|required|
|:---|:-----|:---|:---|:---|
|TreeSpeciesID|ID of tree species|integer|1--8|true|
|TreeSpeciesName|English common name of tree species|character|European oak, American oak, Birch, Larch, Pine, Beech, Unknown.|true|