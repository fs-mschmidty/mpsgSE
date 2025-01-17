# Species Evaluation Automation Process

2024-12-05

# Introduction

This document details the process of automating parts of the Mountain
State Planning Group’s (MPSG), Species Evaluations for Species of
Conservation Concern (SCC). The MPSG is tasked with helping National
Forests with planning efforts. Part of the Pre-assessment portion of
Forest Planning is writing Species Evaluations for SCC. Species
assessments are intended to inform selection of SCC by the Regional
Forester by providing the best available scientific (BASI) information
on if there is “substantial concern about the species’ capability to
persist over the long-term in the plan area” (36 CFR 219.9(c)). This
process is detailed in draft in the “Species of Conservation Concern
Identification Process” (CITE MPSG) document. This document is not
intended to outline the SCC process. Rather it will focus on documenting
the automation of the process outlined in the SCC process document.

Automation is recommended for this process for two primary reasons:
hundreds of Species Evaluations are required for any given planning
unit, resulting in a substantial workload. Automation of repeatable
steps decreases the workload of MPSG staff, produces more consistent and
repeatable documents, and can help define, programmatically or
otherwise, best sources of BASI and processes for retrieving BASI.

# The Process

# Species Evaluation Automation Process

Broadly there are three parts to automating: 1. Develop eligible list of
species for potential listing as SCC 2. Gather available relevant BASI
for species on the eligible list. 3. Automate SCC species evaluation
templates with as much data pre input as possible.

## Process Preface

### The Use of R Programming Language

The MPSG uses the R programming language (R Core Team 2023) for much of
the work we do in automation and will be referencing R code and packages
in this document because it helps explain how we accomplished a lot of
what we did. However, automation is not necessarily dependent on R. Many
of these processes could be completed by other means and R is irrelevant
to many of the data sources and structures we have developed at the
MPSG.

### Taxonomy

Automation would not be possible without first automating taxonomic
classification of species names. The process developed at the MPSG to
produce species evaluations relies on dozens of external resources for
information. Many of those resources have their own methods for
resolving taxonomy, but almost all of them differ in some way on how
they define and classify species. This is problematic for automating
species evaluations because almost every part of the process relies on
reliably cross referencing information. For example, for occurrence
records we rely on SEINet, GBIF and State EO polygons. It could be
possible that for the grass blue grama (*Bouteloua gracilis*) one uses
*Bouteloua gracilis* and another uses a recognized synonym *Chondrosum
gracile* we need a way to recognize both as the same plant. To resolve
this, we needed a way to algorithmically link like species referred to
by different names. It is important that it could be done
algorythmically because some unit database pulls returned hundreds of
thousands of records. Manual resolution of taxonomic classification
would not be possible.

A variety of resources were evaluated as potential solutions taxanomic
classification, but ultimately landed on using the
[`taxize`](https://github.com/ropensci/taxize) (Scott Chamberlain et al.
2020) R package to access the GBIF Backbone Taxonomy. NatureServe, ITIS,
and USDA plants were all tried before we ultimately decided on GBIF.
GBIF was chosen because it reliably returned the correct name. ITIS was
a close second, but it was routinely down and would fail when trying to
resolve long lists of names. NatureServe search returned incorrect names
too frequently. USDA plants was also considered but they do not provide
an APIfor name resolution.

Often when a name is searched in GBIF, the name that is returned does
not match the name in the NatureServe database. The Forest Service
Assessment Handbook (USDA Forest Service (2015)) directs the use of
NatureServe in species consideration lists. Therfore, scientific names
provided by Naturserve for are used for species evaluations. The taxomic
process we developed is agnostic to the scientific name source. It
instead relies on the a taxon_id that is associated with names
associated with a variety of synonyms. When we combine lists or data
sources we rely on the Taxonomic Identifier and not the names for list
joining.

While the system works well, it is not perfect, and occasionally many
scientific names recognized by NatureServe will resolve to the same
species in the GBIF Backbone Taxonomy. In this case manual resolution of
names is necessary and species evaluations must be completed manually.
This is necessary because without matching taxonomic identifiers we have
no way of telling if we are using the correct occurrence records or
federal and state rankings. It may be possible to resolve this in the
future with internal identification but for now these species much be
manually completed.

The process for resolving taxonomy is codified in a function in the
[mpsgSE](https://github.com/fs-mschmidty/mpsgSE) package called
[`get_taxonomies()`](https://github.com/fs-mschmidty/mpsgSE/blob/main/R/get_taxonomies.R).
Essentially the function takes a table of species names or spatial data
and classifies each species name returning full taxonomy and a taxon_id
field added on to the table that was originally input. It takes one
parameter which identifies the field with the scientific name.

### mpsgSE package

<!-- Write stuff and consider moving higher-->

## Build Eligible List

### NatureServe State List

The eligible list development begins with pulling all species in
NatureServe for a given unit. To do this we begin by pulling all species
for the state where a unit resides. In most cases one state pull is
needed, but for units that reside in multiple states each state must be
pulled and then the lists should be merged, eliminating duplicates.

A pull for any given state does note determine eligibility for a unit
list, instead it is the total number of species tracked by NatureServe
for any given state. Species found on the state list must also be
determined to occupy a given unit (determined to be Native and Known)
and meet other qualifying criteria (USDA Forest Service 2015).

Data used from the NatureServe state pull to determine eligibility are:

1.  Scientific Name
2.  NatureServe Global Rank (G/T Rank)
3.  State Rank for given state (S/T Rank)
4.  USFWS Status (Endangered, Threatened, Candidate, or Under Review)

A function was developed for this process for pulling data from any
given state called
[`get_ns_state_list()`](https://github.com/fs-mschmidty/mpsgSE/blob/main/R/get_ns_state_list.R).
It relies on the [natserve](https://github.com/cran/natserv)
(Chamberlain and Tracey 2024) package `ns_export` function. This could
also be accomplished by querying the [NatureServe API
directly](https://explorer.natureserve.org/api-docs/?gad_source=1&gclid=CjwKCAiA34S7BhAtEiwACZzv4QJvuZl31unU2neO0rDSs3JlxTfPvisTwUBKEfuogoeqspagN02w0BoCPeAQAvD_BwE#_export).
It takes one input, which is the state short code (ex: “CO” for
Colorado) and returns the full list of track NatureServe species for
that state.

After the list is aquired
[get_taxonomies](https://github.com/fs-mschmidty/mpsgSE/blob/main/R/get_taxonomies.R)
is use to get the taxon_id for each species for joining with other
lists.

### Unit Spatial Layers and Other Unit Relevant Spatial Layers

Spatial administrative boundary data (S_USA.AdministrativeForest) are
acquired from the Forest Service Enterprise Data Warehouse (EDW). Forest
or grassland boundaries are subseted using a Definition Query for that
Forest Service unit in ArcGIS Pro (ESRI Inc 2024). The administrative
boundary is used to clip (Analysis Toolbox, ArcGIS Pro, ESRI Inc. 2024)
Forest Service owned lands, or Plan Area, from the EDW basic ownership
data (S_USA.BasicOwnership). The administrative boundary and plan area
data are exported to a geodatabse for use through out the plan revision
process. One-kilometer buffers are created around the administrative
boundary and the plan area using the Buffer tool (Analysis Toolbox,
ArcGIS Pro, ESRI Inc. 2024) to capture observations immediately adjacent
to the plan area.

### Get Occurrence Data

As state in the 2012 Planning Rule species must be known to occur in the
planning area. To determine if species are known to occur, open source
occurrence databases are quiried for species occurrences within the
planning unit. While occurrence provide the first line of evidence for
known to occur we also rely on people who work on the planning unit for
additional information.
<!-- Matt I think it would be best if you wrote this section. -->
<!-- For now these lists are just the lists have occurrence numbers and not the actual spatial data! (see section below "Load Occurrence Spatial Data") -->

The following datasets are assigned a taxonomic ID using the
[`mpsgSE::get_taxonomies()`](https://github.com/fs-mschmidty/mpsgSE/blob/main/R/get_taxonomies.R)
function and summarized to the species, subspecies, or variety level.

#### Global Biodiversity Information Facility

Global Biodiversity Information Facility (GBIF; Global Biodiversity
Information Facility 2022a) is a repository of externally sourced
species occurrence records from museum collections, academic studies,
and citizen science programs.

GBIF records requests are staged on GBIF servers and have to be
downloaded (see [Getting Occurrence Data From
GBIF](https://docs.ropensci.org/rgbif/articles/getting_occurrence_data.html)
for details).

An R script submits a records request using the 1-km buffer around the
administrative boundary to spatially query GBIF records.

The data are downloaded in [Darwin Core Archive
format](https://www.gbif.org/darwin-core) (GBIF, 2022b) for full data
provenance.

The data are unzipped and read in to R once the request is available
using the `rgbif` package (version 3.7.8, Chamberlain et al. 2023).

#### SEINet

[SEINet](https://swbiodiversity.org/seinet/index.php) is a data portal
that provides a suite of data access tools, including species occurrence
data from museums, collections, and state and federal agencies. SEINet
data are available through an online data portal and are downloaded
manually. A polygon box, or well-known text (WKT) footprint, is drawn
around the administrative boundary to query species observations using
the Taxonomic Criteria search page
(<https://swbiodiversity.org/seinet/collections/harvestparams.php>). The
query results are downloaded in Darwin Core Archive format, manually
unzipped, and a script reads them into R.

#### EO State Data (NHP Data)

State Natural Heritage Programs (NHPs) provide species occurrence data,
and habitat and distribution models for federal, state, and
non-governmental agencies throughout their state. Element occurrence
(EO) spatial data are requested from an HNP which are often provided in
a geodatabase. The EO data are read to R using an R script.

#### Integrated Monitoring in Bird Conservation Regions

The [Integrated Monitoring in Bird Conservation Regions
(IMBCR)](https://www.birdconservancy.org/what-we-do/science/monitoring/imbcr-program/)
is a long-term avian monitoring program coordinated by the [Birds
Conservancy of the Rockies](https://www.birdconservancy.org/), and
maintains monitoring plots on public lands throughout Forest Service
Regions 1-4. IMBCR data spanning 2008-2023 were obtained for Forest
Service lands on 12 December, 2023, for use in these analyses. These
data were received in and Excel file and an script reads them into R.

<!-- I'm not currently pulling NABat data because the process is prohibitive to do so across the MPSG footprint. -->

-   Maybe why we don’t us Idigbio anymore

The iDigBio Project (iDigBio 2023) is a repository of digitized
vouchered natural history collections including the [Consortium of
Lichen Herbaria](https://lichenportal.org/portal/), the [Consortium of
Bryophyte Herbaria](https://bryophyteportal.org/portal/), and [Rocky
Mountain Herbarium](https://rmh.uwyo.edu/data/search.php). iDigBio
records are available through an API and can be accessed using the
`spocc` package (version `r packageVersion('spocc')`, Owens and
Chamberlain 2023). An R script submits a records request using the 1-km
buffer around the administrative boundary to spatially query iDigBio
records, which are read directly into R.

<!--
Do we want to a include data limitations section?
-->

#### A Note on Limitations of Species Occurrence Data

#### Build Summary Occurrence Dataset

Once all occurrence records are retrieved, each dataset is summarized
with the following fields per species(or taxon_id):

-   \_nObs - Total number of observations within the unit
-   \_minYear - Minimum year of any record
-   \_maxYear - Max year of any occurrence record

### Get Qualifying Lists

In addition to global and state ranks provided by NatureServe, four
additional sources are needed to determine if any given species is
eligible for SCC evaluation:

-   State Ranks: State Threatened and Endangered, and State Wildlife
    Action Plan (SWAP) Tier 1 Lists
-   Regional Sensitive Species Lists
-   Neighboring Unit SCC lists
-   USFWS Status (see above in NatureServe)

#### State Ranks

States have a variety of wildlife and plant conservation lists for
imperiled species. These lists are typically developed by State Natural
Resource or Natural Heritage organizations. Conservation lists, most
commonly come in the form of State Wildlife Actions Plans, which make
states eligible for federal conservation funding, and Threatened,
Endangered and Sensitive Wildlife Lists. The MPSG interprets the that
from these conservation lists species with Tier 1 and Species of
Greatest Conservation Need (SGCN) rankings as well as Threatened and
Endangered rankings are eligible for SCC species evaluations. Some state
develop rankings that do not adhere to these ranking criteria and may
need to be evaluated on a case by case bases. South Dakota for example
has a three number ranking system (species were given 1,2 or 3 ranks).
The MPSG, based on the description of the ranking criteria, determined
that just those species ranked 1 would qualify for evaluations.

#### Regional Sensitive Species Lists

If a species is ranked as sensitive by the Regional Forester in the
region where the planning unit resides that species is considered
eligible for ranking if it is native and known to occur on the planning
unit. These lists are acquired from Forest Service websites
(i.e. [Region 2 Sensitive Species
List](https://www.fs.usda.gov/detail/r2/landmanagement/?cid=stelprdb5390116))

#### Neighboring Unit SCC Lists

If SCC are developed for a neighboring Forests, they should be
considered eligible for SCC if they are native and known on the unit.
Current SCC lists can be found here on the Forest Service planning
public site. Some region make them available grouped by region and
others are available at the Forest Level. A few examples:

-   Region 1 -
    <https://www.fs.usda.gov/detail/r1/landmanagement/planning/?cid=fseprd500402>
-   Region 2, GMUG NF-
    <https://www.fs.usda.gov/detail/carson/landmanagement/planning/?cid=stelprdb5443166>.
-   Region 4, Manti La Sal NF -
    <https://www.fs.usda.gov/main/mantilasal/landmanagement/planning>

#### USFWS Lists

The USFWS, rankings are retrieved from Nature Serve. Species that have
been delisted within the last 5 years or have had a positive 90 day
finding and are under review for listing are eligible.

#### A Note About Tribal Species of Concern

The MPSG is still developing procedures to incorporate species
recognized by “federally recognized Tribes”. Those species will be
included in the future.

#### Data Cleaning and Encorporation

Most commonly these lists come in PDF form but can also come in csv or
Excel formats. They typically require extensive cleaning and/or
conversion from their original format. To aid in the parsing of these
lists the MPSG has developed standardized variables for each species
based on multiple ranks. These values are as follows:

-   `status_area`: the area the ranking applies to (i.e. Colorado, North
    Dakota, United States)
-   `status_type`: the type of ranking

### Make Preliminary Eligible List

-   To the NatureServe State List, which already includes global, state
    and USFWS ranks, full_join (So that if species is not on one list it
    does not get dropped) by taxon_id:
    -   State SWAP Lists
    -   State T and E list
    -   Regional Foresters List
    -   Neighboring SCC lists
-   Then to create all potential species eligible filter the list by:
    -   G/T 1,2 or 3 Ranks.
    -   Any S/T 1 or 2 Ranks.
    -   State SWAP Tier 1 Ranks
    -   State T and E Ranks
    -   Regional Foresters Sensitive Species Lists
    -   Neighboring Regional SCC Designation
-   Join all occurrence lists to all potential species eligible filtered
    list
-   Filter by those species that have one occurrence record in the
    planning unit.

#### Notes on Taxonomic Problems in the Process

### Manually Check Accidental Transients, Native and Known, and Local Concern, to Produce Final SCC Eligible List

<!-- I think we should add into this process the Units local concern species -->

#### Transient Bird List

-   Get ebird maps from ebirdst
-   Do check on each species
-   Output for manual checking

#### Filter those species that need native and known checks

-   Those species with less than 6 occurrences combined from each list
    in the planning area.
-   Those species with their most recent detection being greater than 40
    years ago.

#### Species with uncertain taxanomic determinations

-   Multiple species may be taxonomically identified as the same species
    with `get_taxonomies()`. Becuase we cannot determine their taxonomic
    status programatically, we cannot join them to occurrence lists or
    status lists. Therefore they must be manually vetted.

#### Get user feedback

#### Use user feedback to refine list

Based on manual checks remove species that are: \* Determined to not be
native and known. \* Determined to be accidental or transients \* !
Determined to be of local concern by the unit

## Retrieve External Data for Automating Reports

### Get Synonyms

### Get Data for Making Automated Maps

#### Load Occurrence Spatial Data

GBIF data are acquired using the
[`mpsgSE::get_gbif()`](https://github.com/fs-mschmidty/mpsgSE/blob/main/R/get_gibif.R)
function. This function submits a record request, and downloads and read
the data into R when the request if complete. The data are then
summarized using the
[`mpsgSE::gbif_spp()`](https://github.com/fs-mschmidty/mpsgSE/blob/main/R/gbif_spp.R)
function and a list of species recorded in the plan area and 1-km buffer
of the plan area is created using the
[`mpsgSE::compile_gbif_list()`](https://github.com/fs-mschmidty/mpsgSE/blob/main/R/compile_gbif_list.R)
function. The resulting data are saved in an \*.RData file, a data
format specific to the R coding language, and loaded back into R when
the automated reports are created.

Currently, SEINet, State NHP EO, and IMBCR data are read into R and
summarized using individual R scripts that follow similar workflows as
the GBIF data. These scripts will be converted to R functions and
included in the `mpsgSE` package in the near future.

#### Load Species Distribution Models

-   IUCN
-   BIEN Plant Maps
-   Ebird Maps

#### Get Other Base Map Information

-   State Boundaries
-   United States - lower 48
-   North America
-   The Americas (North and South America)
-   Get Open Street Map Highways (“highway”)
    -   motorway
    -   trunk
    -   primary
-   Clean Unit Names

#### Get NatureServe Habitats From the NatureServe API and Manually Crosswalk to Ecology

#### Retrieve, Clean, and Build Narratives for IMBCR Trend Information

#### Retrieve, Clean, and Build Narratives for Breeding Bird Survey

## Automate Reports

<!-- Look at the qmd and write out each section.  (Maybe should wait until we finish revamping the reports) -->

Chamberlain, Scott, and Christopher Tracey. 2024. “Natserv:
’NatureServe’ Interface.” <https://docs.ropensci.org/natserv/>.

ESRI Inc. 2024. “ArcGIS Pro 3.3.”

R Core Team. 2023. “R: A Language and Environment for Statistical
Computing.” <https://www.R-project.org/>.

Scott Chamberlain, Zachary Foster, Zebulun Arendsee, Eduard Szoecs, Carl
Boettiger, Karthik Ram, Ignasi Bartomeus, et al. 2020. “Taxize:
Taxonomic Information from Around the Web.”
<https://github.com/ropensci/taxize>.

USDA Forest Service. 2015. “Handbook 1909.12. Land Management Planning
Handbook. Chapter 10 - the Assessments.”
