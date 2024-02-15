# Create meta.xml for bud burst DwC-A

# Author: Stefan Vriend, Cherine Jantzen
# Created: 2023-12-20
# Last updated: 2024-02-15


# Create meta.xml for bud burst DwC-A -------------------------------------
source(here::here("R", "create-meta-xml-of-DwCA.R"))

create_meta_xml(core = c("Event" = here::here("data", "budburst_event.csv")),
                extensions = c("ExtendedMeasurementOrFact" = here::here("data", "budburst_extendedmeasurementorfact.csv"),
                               "Occurrence" = here::here("data", "budburst_occurrence.csv")),
                file = here::here("data", "budburst_meta.xml"))
