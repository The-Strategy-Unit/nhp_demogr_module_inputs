
# nhp_demogr_module_inputs

This repo assembles the inputs needed for the demographic module of the NHP model (project 1163).

The repo performs 3 main tasks:

1. sources and organises the demographic information required by the NHP model
2. builds activity weighted catchment populations for hospital trusts
3. describes a method for adjusting demand forecasts for future changes in population health status 

If you have any questions please contact [paulseamer\@nhs.net](mailto:paulseamer@nhs.net).

## Inputs
The following datasets are required as inputs to the demographic module.

**Population projections**
* [2018-based national population projections (NPP)](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/bulletins/nationalpopulationprojections/2018based)
* [2018-based sub-national population projections (SNPP)](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/bulletins/subnationalpopulationprojectionsforengland/2018based)
* [2018-based national population projections (NPP) births by age of mother](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/bulletins/nationalpopulationprojections/2018based)
* [2018-based sub-national population projections (SNPP) births by age of mother](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/birthsbyageofmotherz3)

**Life tables**
* [2018-based life tables](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/bulletins/pastandprojecteddatafromtheperiodandcohortlifetables/1981to2068)
* [2020-based life tables](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/bulletins/pastandprojecteddatafromtheperiodandcohortlifetables/2020baseduk1981to2070)

**Local authority district names and codes, and digital vector boundaries**
* [LAD names and codes](https://geoportal.statistics.gov.uk/search?collection=Dataset&sort=-created&tags=all(NAC_LAD))
* [LAD boundary files](https://geoportal.statistics.gov.uk/search?collection=Dataset&sort=-created&tags=all(NAC_LAD))

## Read-in inputs
The following sctripts will read-in the required input datatsets.

* `read-snpp-2018b.R`
* `read-npp-2018b.R`
* `read-snpp-2018b-births.R`
* `read-npp-2018b-births.R`
* `read-life-tables-2018b.R`
* `read-life-tables-2020b.R`
* `create-lookups.R`

## Modeling activity for the very old
Sub-national population projections are cut at age 90+. National population projections use 0-104, 105-109 and 110+. In order to model activity by single year of age to age 100, we take the age distribution for ages 90-100+ from NPP and apply to SNPP to create sub-national estimates for ages 0-99 and 100+. We implement a mapping across the two variant sets to ensure the most appropriate NPP distribution is used for each SNPP variant.

* `model-syoa-100-snpp-2018b.R`

## Expanding the sub-national variant set
National population projections include a set of 17 variant projections. Sub-national projections include a much smaller set of 4 variants, limited to different assumptions about migration levels and trends. In order to provide a wider range of scenarios for use in the model, we create a custom set of 17 sub-national variant projections by applying the percentage differences between NPP variants (by age/sex/year) and the NPP principal projection to the 2018 baseline for each local authority from the SNPP.

* `custom-variants-snpp-2018b.R`

## Modeling maternity activity
If maternity activity is modeled in the same way as other types of activity then future demand will depend on changes in the number of women of child-bearing age (assuming maternity activity is recorded against the mother, as oppose to the child) e.g. maternity activity for 35-year-old women will increase or decrease inline with changes in the number of 35-year-old females in the general population. A better approach is for maternity activity to depend on changes in the number of births by age of the mother e.g. maternity activity for 35-year-old women increases or decreases inline with the number of births expected to 35-year-old mothers. We use data on projections of births by age of mother to implement this approach.

* `custom-variants-snpp-2018b-births.R`
* `notebooks/model-maternity-activity.R`

## Weighted trust catchments
The NHP model requires estimates of population change specific to individual hospital trusts. Local authority district is the lowest geography that Office for National Statistics projections are produced for so catchments are built on LAD. This is different from the inputs app where catchments are built using lower level super output areas (to mirror [Office for Health Improvement & Disparities OHID methodology](https://app.powerbi.com/view?r=eyJrIjoiODZmNGQ0YzItZDAwZi00MzFiLWE4NzAtMzVmNTUwMThmMTVlIiwidCI6ImVlNGUxNDk5LTRhMzUtNGIyZS1hZDQ3LTVmM2NmOWRlODY2NiIsImMiOjh9))
However, the inputs app only requires catchments for historic and current time periods - not future time periods. Catchments are built using only inpatient activity - we assume that this activity is most broadly representative of the population served by a hospital.

To limit the number of LADs associated with a single trust (i.e. to avoid a long tail of LADs that contribute negligible activity) we only include LADs with a 5% or greater share of inpatient activity. We then redistribute the retained activity to ensure the shares across LADs sum to one.

A trust's final catchment population comprises a weighted sum of its LAD catchment populations where LADs that contribute a greater share of the trust's activity take a greater weight. 

* `trust-catchments.R`
* `weighted-trust-catchments.R`
* see `/figures` folder for maps showing trust catchments

## Modeling changes in population health status
This [notebook](https://connect.strategyunitwm.nhs.uk/modeling-changes-in-population-health-status/) describes and implements a method for adjusting demand forecasts for future changes in population health status.

* `notebooks/model-pop-health-status.qmd`

## Outputs
The following files are passed to the NHP model.

* `data/cohort4_trust_wt_catchment_pops.csv`
* `data/life_expectancy_chnage.csv`
* `data/split_normal_parameters.csv`
* `data/lookup_proj_var.csv`