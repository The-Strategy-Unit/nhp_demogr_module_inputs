# nhp_demogr_module_inputs

This project holds the code used to compile demographic data required as inputs for the demographic module of the NHP model (project 942).

If you have any questions please contact [paulseamer\@nhs.net](mailto:paulseamer@nhs.net)

## Population projections

Reads [2018-based national population projections(NPP)](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/bulletins/nationalpopulationprojections/2018based) and [sub-national population projections (SNPP)](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/bulletins/subnationalpopulationprojectionsforengland/2018based)



## Estimates of the very old (90+)

SNPP are cut at age 90+. NPP age groups are 0-104, 105-109 and 110+. We take the age distribution for ages 90-99 and 100+ from NPP and apply to SNPP to create sub-national estimates for ages 0-99 and 100+. There are more NPP variant projections (x17) than SNPP variants (x4). We implement a mapping of the two variant sets to ensure the most appropriate NPP distribution is used for each SNPP variant.

## Expectation of life (life tables)

Reads [2018-based](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/bulletins/pastandprojecteddatafromtheperiodandcohortlifetables/1981to2068) and [2020-based](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/bulletins/pastandprojecteddatafromtheperiodandcohortlifetables/2020baseduk1981to2070) past and projected period and cohort life tables. This information is used to parameterise the health-status adjustment.

High and low life expectancy variants (alongside the principal projection) were produced for the 2018-based release (no variant projections were produced for the 2020-based release). We implement a mapping of the expectation of life variants to the SNPP variants to ensure consistency between assumptions about future life expectancy and projected population changes.

## Custom sub-national projection variants

A set of 17 variant projections were published alongside the 2018-based NPPs. The 2018-based SNPPs include a much smaller set of 4 variant projections, limited to different assumptions about migration levels and trends. In order to provide a wider range of scenarios for use in the model, we created a custom set of 17 sub-national variant projections by applying the percentage differences between NPP variants (by age/sex/year) and the NPP principal projection to the 2018 baseline for each LA from the SNPP.

## Trust catchments

The module requires estimates of population change specific to individual hospital trusts. Local authority district (LAD) is the lowest geography that ONS projections are produced for so catchments are built on LAD. This is different from the input report where catchments are built using LSOAs (to mirror PHE methodology). However, the input report only requires catchments for historic and current time periods - not future time periods. Catchments are built using only inpatient activity - we assume that this activity is most broadly representative of the population served by a hospital.

To limit the number of LADs associated with a single trust (i.e. to avoid a long tail of LADs that contribute negligible activity) we only include LADs with a 5% or greater share of inpatient activity. We then redistribute the retained activity to ensure the shares across LADs sum to one.

## Weighted trust catchments

A trust's final catchment population comprises a weighted sum of its catchment LAD populations where LADs that contribute a greater share of the trust's activity take a greater weight.
