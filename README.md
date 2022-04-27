# 942_nhp_demogr_module_inputs

This project holds the code used to compile demographic data required as inputs for the demographic module of the NHP model (project 942).

If you have any questions please contact paulseamer@nhs.net

## Population projections
Reads 2018-based national population projections(NPP) and sub-national population projections (SNPP).
https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/bulletins/nationalpopulationprojections/2018based
https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/bulletins/subnationalpopulationprojectionsforengland/2018based


## Estimates of the very old (90+)
SNPP are cut at age 90+. NPP age groups are 0-104, 105-109 and 110+. We take the age distribution for ages 90-99 and 100+ from NPP and apply to SNPP to create sub-national estimates for ages 0-99 and 100+. There are more NPP variant projections (x17) than SNPP variants (x4). We implement a mapping of the two variant sets to ensure the most appropriate NPP distribution is used for each SNPP variant. 

## Expectation of life (life tables)
Reads 2018-based and 2020-based past and projected period and cohort life tables. This information is used to parameterise the health-status adjustment.
https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/bulletins/pastandprojecteddatafromtheperiodandcohortlifetables/1981to2068
https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/bulletins/pastandprojecteddatafromtheperiodandcohortlifetables/2020baseduk1981to2070

High and low life expectancy variants (alongside the principal projection) were produced for the 2018-based release (no variant projections were produced for the 2020-based release). We implement a mapping of the expectation of life variants to the SNPP variants to ensure consistency between assumptions about future life expectancy and projected population changes.

## Provider catchments
The module requires estimates of population change specific to individual hospital providers. Local authority district (LAD) is the lowest geography that ONS projectionsare produced for so catchments are built on LAD. This is different from the input report where catchments are built using LSOAs (to mirror PHE methodology). However, the input report only requires catchments for historic and current time periods - not future time periods. Catchments are built using only inpatient activity - we assume that this activity is most broadly representative of the population served by a hospital. Catchments are calculated for a set of provider organisations ...

https://app.powerbi.com/view?r=eyJrIjoiODZmNGQ0YzItZDAwZi00MzFiLWE4NzAtMzVmNTUwMThmMTVlIiwidCI6ImVlNGUxNDk5LTRhMzUtNGIyZS1hZDQ3LTVmM2NmOWRlODY2NiIsImMiOjh9

To limit the number of LADs associated with a single provider (i.e. to avoid a long tail of LADs that contribute negligible activity) we only include LADs with a 5% or greater share of inpatient activity. We then redistribute the retained activity to ensure the shares across LADs sums to one.

A providers final catchment population comprises a weighted share of multiple LAD populations where LADs that contribute a greater share of the providers activity take a greater weight. 
