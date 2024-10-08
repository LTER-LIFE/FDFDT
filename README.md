# A hands-on guide to FAIR and structured ecological data 
> a product of the 'FAIR Data for Digital Twins' (FDFDT) project

<details>
<summary><h3>Table of Contents</h3></summary>

  - [Guide](#guide)
	- [Link](#link)
    - [Description](#description)  
    - [Authors](#authors)
    - [Feedback](#feedback)	    
  - [Example datasets and code](#example)
    - [General functions](#general-functions)
    - [Generate an API token for DataverseNL/DANS Datastation](#API-token)
    - [Bud burst data](#budburst) 
    - [Cricket data](#cricket) 
    - [CLUE data](#clue)
</details>

## Guide <a name="guide"/>

### Link <a name="link"/>
You can find the guide here: https://lter-life.github.io/FDFDT-Manual/

### Description <a name="description"/>
This repository contains the R code and the enhanced datasets of the practical work in the *FAIR Data for Digital Twins* project that underlies "A hands-on guide to FAIR and structured ecological data". 

For the code that builds the website of the guide, go to the [FDFDT-Manual repository](https://github.com/LTER-LIFE/FDFDT-Manual).

### Authors <a name="authors"/>
- [Cherine Jantzen](https://github.com/CherineJ) / NIOO-KNAW (ORCID: [0009-0006-0723-2682](https://orcid.org/0009-0006-0723-2682))
- [Stefan Vriend](https://github.com/StefanVriend) / NIOO-KNAW (ORCID: [0000-0002-9006-5988](http://orcid.org/0000-0002-9006-5988))

### Feedback <a name="feedback"/>
We are happy to receive your feedback, suggestions or questions on the guide or the code, scripts and datasets! For that, please open a new issue [here](https://github.com/LTER-LIFE/FDFDT/issues/new). 

## Example datasets and code <a name="example"/>

The R codes include general functions used for all example datasets and dataset-specific scripts. 

**Note**: Not all scripts are needed to reproduce the examples in the guide. 

### General functions <a name="general-functions"/>

Folder: [R](https://github.com/LTER-LIFE/FDFDT/tree/main/R)

Files:
- `assign_uuid.R`\
generates a universally unique identifier (UUID) for a dataset that can be used as `packageID` in the EML file; saves the UUID and dataset name in a look-up table and checks whether the entered dataset has a UUID already before generating a new one
- `create-meta-xml-of-DwCA.R`\
creates the meta.xml file of a Darwin Core Archive by looking up the IRI for each Darwin Core term (column name) in each file (core & extensions) of the Darwin Core Archive and saving them in the required format as a XML file
- `retrieveData-API-Dataverse.R`\
retrieves data through the API of DataverseNL, DataverseNL (demo version) and the DANS Data Station; requires the DOI of the target dataset and a personal API token ([see below](#API-token))

### Generate an API token for DataverseNL/DANS Data Station <a name="API-token"/>

To generate your personal API token for **DataverseNL**, visit the [Log In page](https://dataverse.nl/loginpage.xhtml?redirectPage=%2Fdataverse_homepage.xhtml). If you are affiliated with a partner institution, you can directly log in with your institution credentials to create an account. Otherwise click on "Sign up for a Dataverse account." and fill in the form. After you logged in, click on your user profile name (top right) and select "API Token" in the drop-down menu and create your personal API token by clicking on "Create API token". 

To generate your personal API token for the **DANS Data Station**, visit this [Log In page](https://lifesciences.datastations.nl/loginpage.xhtml?redirectPage=%2Fdataverse.xhtml) and choose a log in option (GitHub, Google, Institutional Account). After you logged in, click on your user profile name (top right) and select "API Token" in the drop-down menu and create your personal API token by clicking on "Create API token". 

### Bud burst data <a name="budburst"/>

Description: Long-term data on the leaf phenology of different tree species across the Netherlands collected by the [NIOO-KNAW](https://nioo.knaw.nl).

Folder: [R/budburst](https://github.com/LTER-LIFE/FDFDT/tree/readme/R/budburst)

Files:
- `01_budburst_retrieveData-SQLServer.R`\
**for NIOO-KNAW internal use only**; retrieves the raw data files from the local database and creates README for the dataset to prepare upload to DataverseNL
- `02_budburst_map-to-DarwinCore.R`\
maps the bud burst data retrieved from Dataverse to Darwin Core and creates core file, extension files and meta.xml file of the Darwin Core Archive (*requires DataverseNL API token to run*)
- `03_budburst_create-EML-xml.R`\
creates the EML file for the bud burst data
- `04_budburst_zip-files-to-DwC-Archive.R`\
collects all files of the Darwin Core Archive and saves them in a ZIP folder

### Cricket data <a name="cricket"/>

Description: Experimental data on a feeding experiment on the European field cricket (*Gryllus campestris*) in the Netherlands [(Vogels et al., 2021)](https://doi.org/10.3389/fevo.2021.659363).

Folder: [R/crickets](https://github.com/LTER-LIFE/FDFDT/tree/readme/R/crickets)

Files:
- `crickets-map-to-DarwinCore.R`\
maps the cricket data retrieved from the DANS Data Station to Darwin Core and creates core file, extension files and meta.xml file of the Darwin Core Archive (*requires DANS Data Station API token to run*)
- `crickets_create_EML-xml.R`\
creates the EML file for cricket data

### CLUE data <a name="clue"/>

Description: Vegetation data on a [long-term grassland biodiversity field experiment](https://nioo.knaw.nl/en/facilities/clue-field-veluwe-database) in the Netherlands. This data is not (yet) available online and only accessible internally.

**Note**: Consists of two datasets for two different experiments (i.e., exp1 & exp2). As the datasets are very similar, they are treated as one in the guide.

Folder: [R/CLUE](https://github.com/LTER-LIFE/FDFDT/tree/readme/R/CLUE)

Files:
- `CLUE-exp1_map-to-DwC.R`\
maps the CLUE data for experiment 1 to Darwin Core and creates core file, extension files and meta.xml file of the Darwin Core Archive (and removes these files again, as the data is not yet published)
- `CLUE-exp1_create-EML.R`\
creates the EML file for the CLUE data of experiment 1
- `CLUE-exp2_map-to-DwC.R`\
maps the CLUE data for experiment 2 to Darwin Core and creates core file, extension files and meta.xml file of the Darwin Core Archive (and removes these files again, as the data is not yet published) 
- `CLUE-exp2_create-EML.R`\
creates the EML file for the CLUE data of experiment 2
- `retrieve-taxonInformation-from-GBIF.R`\
retrieves taxonomic information from GBIF; both datasets include a large number of plant species with several misspellings in the scientific names or synonyms are used, which makes the automatic retrieval of taxonomic information difficult. This function corrects for these difficulties and checks in the [Global Name Resolver](https://resolver.globalnames.org/) for the most commonly used author information.
