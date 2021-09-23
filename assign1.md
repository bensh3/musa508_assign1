Assignment 1
================
Sisun Cheng, Benjamin She, Xiaoyi Wu
9/23/2021

``` r
library(tidyverse)
library(tidycensus)
library(tidytransit)
library(tigris)
library(sf)
library(kableExtra)
library(gt)

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")
```

## First get boundary of Minneapolis-St Paul urbanized area

``` r
twincities <- urban_areas(cb=TRUE) %>%
  filter(UACE10 == "57628") %>% 
  select(geometry)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  16%  |                                                                              |============                                                          |  17%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |================                                                      |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |=====================                                                 |  31%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |=====================================                                 |  53%  |                                                                              |=====================================                                 |  54%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |========================================                              |  58%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |===================================================                   |  74%  |                                                                              |====================================================                  |  74%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  77%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  79%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  91%  |                                                                              |=================================================================     |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================| 100%

## Then get ACS tract data for whole state and intersect by boundary

``` r
getACSyr <- function(yearACS) {
  temp<-get_acs(geography = 'tract',
                year = yearACS,
                variables = c("B25026_001","B25058_001","B08301_001","B08301_010"),
                state = "MN",
                geometry = TRUE,
                output = "wide") %>%
    mutate(year = yearACS) %>%
    mutate(TotalPop = B25026_001E) %>% 
    # normalize population as expected of choropleths; use square miles
    mutate(PopDens = B25026_001E / as.numeric(st_area(geometry) / 2.59e+6)) %>% 
    mutate(MedRent = B25058_001E) %>% 
    mutate(PctTransit = B08301_010E / B08301_001E * 100) %>% 
    select(!(B25026_001E:B08301_010M))
  
  st_join(temp, twincities, join=st_within, left=FALSE) %>% 
    st_transform('EPSG:2812')
}

tracts09 <- getACSyr(2009)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=====                                                                 |   8%  |                                                                              |===========                                                           |  15%  |                                                                              |==================                                                    |  26%  |                                                                              |==============================                                        |  43%  |                                                                              |====================================                                  |  51%  |                                                                              |===============================================                       |  66%  |                                                                              |====================================================                  |  74%  |                                                                              |======================================================                |  77%  |                                                                              |=========================================================             |  82%  |                                                                              |===============================================================       |  89%  |                                                                              |====================================================================  |  97%  |                                                                              |======================================================================| 100%

``` r
tracts19 <- getACSyr(2019)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |====                                                                  |   5%  |                                                                              |========                                                              |  11%  |                                                                              |===========                                                           |  16%  |                                                                              |=============                                                         |  18%  |                                                                              |================                                                      |  23%  |                                                                              |==================                                                    |  25%  |                                                                              |====================                                                  |  29%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  45%  |                                                                              |==================================                                    |  48%  |                                                                              |======================================                                |  54%  |                                                                              |========================================                              |  57%  |                                                                              |============================================                          |  63%  |                                                                              |=====================================================                 |  75%  |                                                                              |=======================================================               |  79%  |                                                                              |===========================================================           |  84%  |                                                                              |============================================================          |  86%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================== |  98%  |                                                                              |======================================================================| 100%

# wrangle in LEHD Workforce Area Characteristics, which has job counts by block group. Summarize by tract

``` r
lehdwac08 <- read_csv("data/mn_wac_S000_JT00_2008.csv") %>% 
  select(w_geocode,C000) %>%
  mutate(GEOID = str_sub(w_geocode,1,11)) %>%
  group_by(GEOID) %>% 
  summarise(jobs=sum(C000))
lehdwac18 <- read_csv("data/mn_wac_S000_JT00_2018.csv") %>% 
  select(w_geocode,C000) %>%
  mutate(GEOID = str_sub(w_geocode,1,11)) %>%
  group_by(GEOID) %>% 
  summarise(jobs=sum(C000))
```

### like population, normalize jobs by square mile area

``` r
tracts09 <- tracts09 %>% left_join(lehdwac08,by="GEOID") %>% 
  mutate(JobDens = jobs / as.numeric(st_area(geometry) / 2.59e+6))
tracts19 <- tracts19 %>% left_join(lehdwac18,by="GEOID") %>% 
  mutate(JobDens = jobs / as.numeric(st_area(geometry) / 2.59e+6))

allTracts <- rbind(tracts09,tracts19)
```

# Rail stops and centroid buffers

``` r
metrotransit <- read_gtfs("data/gtfs.zip")

service_id <- filter(metrotransit$calendar, monday==1) %>% pull(service_id)
route_id <- filter(metrotransit$routes, route_id==c('901','902')) %>% pull(route_id)
railStops <- filter_stops(metrotransit,service_id,route_id) %>% 
  group_by(stop_name) %>% slice(1) %>% stops_as_sf(2812)
railLines <- get_route_geometry(gtfs_as_sf(metrotransit), route_id, service_id)

# include one-foot buffer for multipleRingBuffer
railBuffers <- bind_rows(
  st_buffer(railStops,2640) %>% 
    mutate(Legend = "Buffer") %>% 
    dplyr::select(stop_name,Legend),
  st_union(st_buffer(railStops,2640)) %>% 
    st_sf() %>% 
    mutate(Legend = "Unionized Buffer"),
  st_union(st_buffer(railStops,1320)) %>% 
    st_sf() %>% 
    mutate(Legend = "Quarter-mile Buffer"),
  st_union(st_buffer(railStops,1)) %>% 
    st_sf() %>% 
    mutate(Legend = "1-foot Buffer")
)

buffer <- filter(railBuffers, Legend=="Unionized Buffer")
quarter <- filter(railBuffers,Legend=="Quarter-mile Buffer")
onefoot <- filter(railBuffers, Legend=="1-foot Buffer")
```

# select TOD tracts by buffer, centroid method

``` r
allTracts.group <- 
  rbind(
    st_centroid(allTracts)[buffer,] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(allTracts)[buffer, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD")) %>%
  mutate(MedRent = ifelse(year == 2009, MedRent * 1.275, MedRent))
```

# Outputs begin here

``` r
palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")
```

# Define TOD and non-TOD tracts

``` r
ggplot() +
  geom_sf(data=allTracts.group, aes(fill = TOD), color="transparent") +
  geom_sf(data=railLines, aes(color = route_id),size=1) +
  facet_wrap(~year) +
  scale_fill_manual(name = "Census tract type",
                    values = c("#5ab4ac", "#d8b365")) +
  scale_color_manual(name = "Light rail line",
                     values = c('blue', 'darkgreen'),
                     labels = c('Blue Line', 'Green Line')) + 
  mapTheme() +
  labs(title = "Census tracts by transit-oriented development classification",
       subtitle = "Minneapolis-St. Paul, MN-WI Urbanized Area",
       caption = "Note: Green Line did not open until 2014")
```

![](Figs/unnamed-chunk-9-1.png)<!-- -->

# Step 2: Time-space indicator maps

``` r
plotStep2 <- function(indicator, indTitle, units) {
  ggplot() +
    geom_sf(data=allTracts.group, aes(fill = q5(eval(parse(text=indicator)))), color="transparent") +
    geom_sf(data=buffer, color = "red", fill = "transparent") +
    scale_fill_manual(values = palette5,
                      labels = qBr(allTracts.group, indicator),
                      name = paste0(indTitle,"\n",units,"\n(Quintile breaks)")) +
    labs(title = paste0(indTitle,", 2009-2019"),
         subtitle = "Minneapolis-St. Paul, MN-WI Urbanized Area",
         caption = "Red border = 1/2 mile buffer around light rail stations") + 
    mapTheme() + 
    facet_wrap(~year)
}

plotStep2("PopDens","Population density","(per sq. mi)")
```

![](Figs/unnamed-chunk-10-1.png)<!-- -->

``` r
plotStep2("JobDens","Employment density","(per sq. mi)")
```

![](Figs/unnamed-chunk-10-2.png)<!-- -->

``` r
plotStep2("MedRent", "Median rent","($2019)")
```

![](Figs/unnamed-chunk-10-3.png)<!-- -->

``` r
plotStep2("PctTransit", "Transit mode share","(%)")
```

![](Figs/unnamed-chunk-10-4.png)<!-- -->

# Step 3 & 4: Grouped bar plot and table

``` r
allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%
  group_by(year, TOD) %>%
  summarize(Pop_Density = mean(PopDens, na.rm = T),
            Job_Density = mean(JobDens, na.rm = T),
            Median_Rent = mean(MedRent, na.rm = T),
            Pct_Transit = mean(PctTransit, na.rm = T)) %>% 
  gather(Variable, Value, -year, -TOD)

ggplot(allTracts.Summary, aes(factor(year), Value, fill = TOD)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
  facet_wrap(~Variable, scales = "free", ncol=2) +
  labs(title = "Comparing TOD and non-TOD Census tracts, 2009 and 2019",
       subtitle = "Minneapolis-St. Paul, MN-WI Urbanized Area") +
  plotTheme() + theme(legend.position = "bottom",
                      axis.title = element_blank())
```

![](Figs/unnamed-chunk-11-1.png)<!-- -->

``` r
# table using kable
allTracts.Summary %>%
  unite(year.TOD, year, TOD, sep = ": ", remove = T) %>%
  mutate(Value = round(Value, 2)) %>% 
  spread(year.TOD, Value) %>% 
  kable(caption = "Comparing TOD and non-TOD Census tracts, 2009 and 2019") %>%
  kable_material("striped", full_width = F, html_font = "Helvetica") %>%
  footnote(footnote_as_chunk = T,
           general_title = "",
           general = "Minneapolis-St. Paul, MN-WI Urbanized Area")
```

<table class=" lightable-material lightable-striped" style="font-family: Helvetica; width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
<caption>
Comparing TOD and non-TOD Census tracts, 2009 and 2019
</caption>
<thead>
<tr>
<th style="text-align:left;">
Variable
</th>
<th style="text-align:right;">
2009: Non-TOD
</th>
<th style="text-align:right;">
2009: TOD
</th>
<th style="text-align:right;">
2019: Non-TOD
</th>
<th style="text-align:right;">
2019: TOD
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Job\_Density
</td>
<td style="text-align:right;">
1579.25
</td>
<td style="text-align:right;">
6328.69
</td>
<td style="text-align:right;">
1783.81
</td>
<td style="text-align:right;">
8755.36
</td>
</tr>
<tr>
<td style="text-align:left;">
Median\_Rent
</td>
<td style="text-align:right;">
1081.90
</td>
<td style="text-align:right;">
880.73
</td>
<td style="text-align:right;">
1138.66
</td>
<td style="text-align:right;">
906.10
</td>
</tr>
<tr>
<td style="text-align:left;">
Pct\_Transit
</td>
<td style="text-align:right;">
5.34
</td>
<td style="text-align:right;">
12.67
</td>
<td style="text-align:right;">
5.22
</td>
<td style="text-align:right;">
13.44
</td>
</tr>
<tr>
<td style="text-align:left;">
Pop\_Density
</td>
<td style="text-align:right;">
3866.91
</td>
<td style="text-align:right;">
7439.57
</td>
<td style="text-align:right;">
4111.53
</td>
<td style="text-align:right;">
8622.07
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; " colspan="100%">
<sup></sup> Minneapolis-St. Paul, MN-WI Urbanized Area
</td>
</tr>
</tfoot>
</table>

``` r
# table using gt
allTracts.Summary %>% 
  unite(year.TOD, year, TOD, sep = ": ", remove = T) %>%
  mutate(Value = round(Value, 2)) %>% 
  spread(year.TOD, Value) %>% 
  gt(rowname_col = "Variable") %>% 
  tab_spanner_delim(delim = ':') %>%
  tab_header(title = "Comparing TOD and non-TOD Census tracts, 2009 and 2019",
             subtitle = "Minneapolis-St. Paul, MN-WI Urbanized Area") %>% 
  opt_align_table_header(align="left") %>% 
  tab_style(style = list(cell_text(align = "right")),
            locations = cells_stub(rows = TRUE)) %>%
  tab_options(table.font.names = 'Helvetica',
              stub.font.weight = 'bold')
```

<div id="kyxxztbxja" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: Helvetica;
}

#kyxxztbxja .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#kyxxztbxja .gt_heading {
  background-color: #FFFFFF;
  text-align: left;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#kyxxztbxja .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#kyxxztbxja .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#kyxxztbxja .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kyxxztbxja .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#kyxxztbxja .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#kyxxztbxja .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#kyxxztbxja .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#kyxxztbxja .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#kyxxztbxja .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#kyxxztbxja .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#kyxxztbxja .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#kyxxztbxja .gt_from_md > :first-child {
  margin-top: 0;
}

#kyxxztbxja .gt_from_md > :last-child {
  margin-bottom: 0;
}

#kyxxztbxja .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#kyxxztbxja .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: bold;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#kyxxztbxja .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#kyxxztbxja .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#kyxxztbxja .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#kyxxztbxja .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#kyxxztbxja .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#kyxxztbxja .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kyxxztbxja .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#kyxxztbxja .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#kyxxztbxja .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#kyxxztbxja .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#kyxxztbxja .gt_left {
  text-align: left;
}

#kyxxztbxja .gt_center {
  text-align: center;
}

#kyxxztbxja .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#kyxxztbxja .gt_font_normal {
  font-weight: normal;
}

#kyxxztbxja .gt_font_bold {
  font-weight: bold;
}

#kyxxztbxja .gt_font_italic {
  font-style: italic;
}

#kyxxztbxja .gt_super {
  font-size: 65%;
}

#kyxxztbxja .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="5" class="gt_heading gt_title gt_font_normal" style>Comparing TOD and non-TOD Census tracts, 2009 and 2019</th>
    </tr>
    <tr>
      <th colspan="5" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Minneapolis-St. Paul, MN-WI Urbanized Area</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1"></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2">
        <span class="gt_column_spanner">2009</span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2">
        <span class="gt_column_spanner">2019</span>
      </th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1"> Non-TOD</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1"> TOD</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1"> Non-TOD</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1"> TOD</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left gt_stub" style="text-align: right;">Job_Density</td>
<td class="gt_row gt_right">1579.25</td>
<td class="gt_row gt_right">6328.69</td>
<td class="gt_row gt_right">1783.81</td>
<td class="gt_row gt_right">8755.36</td></tr>
    <tr><td class="gt_row gt_left gt_stub" style="text-align: right;">Median_Rent</td>
<td class="gt_row gt_right">1081.90</td>
<td class="gt_row gt_right">880.73</td>
<td class="gt_row gt_right">1138.66</td>
<td class="gt_row gt_right">906.10</td></tr>
    <tr><td class="gt_row gt_left gt_stub" style="text-align: right;">Pct_Transit</td>
<td class="gt_row gt_right">5.34</td>
<td class="gt_row gt_right">12.67</td>
<td class="gt_row gt_right">5.22</td>
<td class="gt_row gt_right">13.44</td></tr>
    <tr><td class="gt_row gt_left gt_stub" style="text-align: right;">Pop_Density</td>
<td class="gt_row gt_right">3866.91</td>
<td class="gt_row gt_right">7439.57</td>
<td class="gt_row gt_right">4111.53</td>
<td class="gt_row gt_right">8622.07</td></tr>
  </tbody>
  
  
</table>
</div>

# Step 5: graduated symbol maps

``` r
stationStat <- st_join(filter(railBuffers,Legend=="Buffer"),
                       st_centroid(filter(allTracts,year==2019))) %>% 
  st_drop_geometry() %>% 
  group_by(stop_name) %>% 
  summarise(Pop = sum(TotalPop, na.rm=T), Rent = median(MedRent, na.rm=T)) %>% 
  full_join(select(railStops,stop_name,geometry),by="stop_name")

ggplot() +
  geom_sf(data=filter(allTracts.group,TOD=="TOD"), color="#666666", lwd=.1) +
  geom_sf(data=stationStat, aes(geometry=geometry, size=Pop, color=Pop), alpha=0.7) +
  scale_size_binned() + scale_color_gradient(low="#0868ac",high="#bae4bc") +
  guides(size=guide_legend(), color=guide_legend()) + mapTheme() +
  labs(title="Light rail stations by total population within 1/2 mile (2019)",
       subtitle="Minneapolis-St. Paul, MN-WI Urbanized Area")
```

![](Figs/unnamed-chunk-12-1.png)<!-- -->

``` r
ggplot() +
  geom_sf(data=filter(allTracts.group,TOD=="TOD"), color="#666666", lwd=.1) +
  geom_sf(data=stationStat, aes(geometry=geometry, size=Rent, color=Rent), alpha=0.7) +
  scale_size_binned() + scale_color_gradient(low="#0868ac",high="#bae4bc") +
  guides(size=guide_legend(), color=guide_legend()) + mapTheme() + 
  labs(title="Light rail stations by median rent within 1/2 mile (2019)",
       subtitle="Minneapolis-St. Paul, MN-WI Urbanized Area")
```

![](Figs/unnamed-chunk-12-2.png)<!-- -->

# Step 6: geom\_line & multiple ring buffer

``` r
allTracts.rings <-
  st_join(st_centroid(dplyr::select(allTracts, GEOID, year)), 
          multipleRingBuffer(onefoot, 32000, 2640)) %>%
  st_drop_geometry() %>%
  left_join(select(allTracts, GEOID, MedRent, year), 
            by=c("GEOID"="GEOID", "year"="year")) %>%
  st_sf() %>%
  mutate(distance = distance / 5280) %>% 
  st_drop_geometry() %>% 
  group_by(year, distance) %>% 
  summarise(RingRent = mean(MedRent, na.rm=T)) %>% 
  arrange(year, distance)

ggplot(allTracts.rings, aes(x=distance,y=RingRent,color=factor(year))) +
  geom_line() +
  geom_point() +
  scale_color_manual(name="Year",values = c("#bae4bc", "#0868ac")) +
  scale_x_continuous(breaks=seq(0,6,by=1)) +
  plotTheme() +
  labs(title="Rent as a function of distance from light rail stations",
       subtitle="Minneapolis-St. Paul, MN-WI Urbanized Area",
       x="Radial distance from station (miles)",
       y="Average rent ($)")
```

![](Figs/unnamed-chunk-13-1.png)<!-- -->

# Step 7: Crime and 311 data

``` r
data311 <- read_csv("data/Public_311_2019.csv") %>% 
  st_as_sf(coords = c('Lon', 'Lat'), crs = 4326) %>% 
  st_transform(st_crs(allTracts))

choro311 <- st_join(filter(allTracts,year==2019), data311, join = st_intersects, left=F) %>%
  group_by(GEOID) %>% 
  summarise(tot311 = n()) %>% 
  mutate(dens311 = tot311 / as.numeric(st_area(geometry) / 2.59e+6))

ggplot() +
  geom_sf(data=choro311, aes(fill=q5(dens311)), color = "transparent") + 
  geom_sf(data=quarter, color = "red", fill = "transparent") +
  coord_sf(crs=st_crs(4326),xlim=c(-93.36,-93.15),ylim=c(44.82,45.1), expand=FALSE) +
  scale_fill_manual(values = palette5,
                  labels = qBr(choro311, "dens311"),
                  name = paste0("311 requests\n(per sq. mi.)\n(Quintile breaks)")) +
  mapTheme() +
  labs(title = "Density of 311 requests in Minneapolis",
       subtitle = "Red border = 1/4 mile buffer around light rail stations")
```

![](Figs/unnamed-chunk-14-1.png)<!-- -->
