
# nhp_demogr_module_inputs

This repo holds the code for the demographic module of the NHP model (project 942).

The module performs 3 main tasks:

1. sources and organises the demographic information required by the NHP model
2. builds activity weighted population catchments for hospital trusts
3. describes and implements a method for adjusting demand forecasts for future changes in population health status 

If you have any questions please contact [paulseamer\@nhs.net](mailto:paulseamer@nhs.net).

## Required inputs

The following datasets are used as inputs to this module:

**Population projections**

* [2018-based national population projections (NPP)](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/bulletins/nationalpopulationprojections/2018based)
* [sub-national population projections (SNPP)](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/bulletins/subnationalpopulationprojectionsforengland/2018based)

**Life tables**

* [2018-based life tables](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/bulletins/pastandprojecteddatafromtheperiodandcohortlifetables/1981to2068)
* [2020-based life tables](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/bulletins/pastandprojecteddatafromtheperiodandcohortlifetables/2020baseduk1981to2070)

## model-syoa-100-snpp-2018b.R

Sub-national population projections SNPP are cut at age 90+. National population projections NPP age groups are 0-104, 105-109 and 110+. In order to model activity by single year of age to age 100, we take the age distribution for ages 90-99 and 100+ from NPP and apply to SNPP to create sub-national estimates for ages 0-99 and 100+. There are more NPP variant projections (x17) than SNPP variants (x4). We implement a mapping of the two variant sets to ensure the most appropriate NPP distribution is used for each SNPP variant.

## custom-variants-snpp-2018b.R

The 2018-based NPPs include a set of 17 variant projections. The 2018-based SNPPs include a much smaller set of 4 variant projections, limited to different assumptions about migration levels and trends. In order to provide a wider range of scenarios for use in the model, we create a custom set of 17 sub-national variant projections by applying the percentage differences between NPP variants (by age/sex/year) and the NPP principal projection to the 2018 baseline for each local authority LA from the SNPP.

## weighted-trust-catchments.R

The model requires estimates of population change specific to individual hospital trusts. Local authority district LAD is the lowest geography that Office for National Statistics ONS projections are produced for so catchments are built on LAD. This is different from the inputs app. where catchments are built using lower level super output areas LSOAs (to mirror [Office for Health Improvement & Disparities OHID methodology](https://app.powerbi.com/view?r=eyJrIjoiODZmNGQ0YzItZDAwZi00MzFiLWE4NzAtMzVmNTUwMThmMTVlIiwidCI6ImVlNGUxNDk5LTRhMzUtNGIyZS1hZDQ3LTVmM2NmOWRlODY2NiIsImMiOjh9))
However, the inputs app. only requires catchments for historic and current time periods -- not future time periods. Catchments are built using only inpatient activity -- we assume that this activity is most broadly representative of the population served by a hospital.

To limit the number of LADs associated with a single trust (i.e. to avoid a long tail of LADs that contribute negligible activity) we only include LADs with a 5% or greater share of inpatient activity. We then redistribute the retained activity to ensure the shares across LADs sum to one.

A trust's final catchment population comprises a weighted sum of its catchment LAD populations where LADs that contribute a greater share of the trust's activity take a greater weight.

## model-pop-health-status.QMD

This notebook describes and implements a method for adjusting demand forecasts for future changes in population health status.

