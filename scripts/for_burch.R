library(googledrive)
library(tidyverse)
library(tidycensus)
library(sf)


## function for downloading firststreet data; you'll need to create the first_street_risk folder in the data/original folder
download_firststreet <- function(filelst){
  gdid <- as_id(filelst$id)
  fname <- sub("\\_.*", "", filelst$name)
  drive_download(gdid, path = paste0(here::here("data/original/first_street_risk/"), fname, ".csv"), overwrite = TRUE)                  
  return(paste0(here::here("data/original/first_street_risk/"), fname, ".csv"))
}

#get file ids in drive
firststreet.drive <- drive_ls(drive_get(as_id("https://drive.google.com/drive/folders/1P0JP72BpLc8FUKhG_NdRQwbhLnMzpk-8")))

#download each file locally and return a vector of filenames
firststreet.files <- unlist(lapply(1:nrow(firststreet.files), function(x) download_firststreet(filelst = firststreet.drive[x,])))

## each of the tidycensus calls below downloads the geometry of the unit (tract or block group) and converts to centroids. If you don't want the centroids for the fishnet eliminate the st_centroid line

## Download percent of households that have limited English as sf
st <-  tigris::fips_codes %>% 
  filter(state_name %in% c(state.name[state.name != c("Hawaii", "Alaska")], "District of Columbia")) %>% 
  distinct(., state_code) %>% c() %>% unname() %>% unlist()
  
english.limited <- map(st, function(x) tidycensus::get_acs(geography = "block group", year = 2019, 
                    variables = c(
                      totalhouse = "C16002_001",
                      spanishlimited = "C16002_004",
                      ielimited = "C16002_007",
                      aplimited = "C16002_010",
                      otherlimited = "C16002_013"), 
                    survey = "acs5", state = x, geometry = TRUE) %>% 
             pivot_wider(., id_cols = c("GEOID", "geometry"), names_from = "variable", values_from = "estimate") 
           
           ) %>% 
  bind_rows() %>% 
  mutate(., houselim = (spanishlimited + ielimited + aplimited + otherlimited)/totalhouse) %>% 
  st_centroid(.)

## Download median income
medinc <- map(st, function(x) get_acs(geography = "tract", year = 2019, 
                            variables = c(householdinc = "S1901_C01_012"), 
                            survey = "acs5", state = x, geometry = TRUE) %>% 
      pivot_wider(., id_cols = c("GEOID", "geometry"), names_from = "variable", values_from = "estimate") 
    
) %>% 
  bind_rows() %>% 
  st_centroid(.) %>% 
  drop_na(householdinc) %>% 
  mutate(., x = st_coordinates(.)[,1],
         y= st_coordinates(.)[,2])

## Download Age

age <- map(st, function(x) get_acs(geography = "block group", year = 2020, 
                            variables = c(totalmale = "B01001_002",
                                          totalfemale = "B01001_026",
                                          totalmale5 = "B01001_003",
                                          totalfemale5 = "B01001_027",
                                          totalmale65 = "B01001_020",
                                          totalmale67 = "B01001_021",
                                          totalmale70 = "B01001_022",
                                          totalmale75 = "B01001_023",
                                          totalmale80 ="B01001_024" ,
                                          totalmale85 = "B01001_025",
                                          totalfemale65 ="B01001_044",
                                          totalfemale67 = "B01001_045",
                                          totalfemale70 = "B01001_046",
                                          totalfemale75 = "B01001_047",
                                          totalfemale80 = "B01001_048",
                                          totalfemale85 = "B01001_049"), 
                            survey = "acs5", state = x, geometry = TRUE) %>% 
      pivot_wider(., id_cols = c("GEOID", "geometry"), names_from = "variable", values_from = "estimate") 
) %>% 
  bind_rows() %>% 
  mutate(prop5 = (totalmale5 + totalfemale5)/(totalmale + totalfemale),
         prop65 = (totalmale65 +totalmale67 +totalmale70 + totalmale75 + totalmale80 +totalmale85 + totalfemale65 +totalfemale67 +totalfemale70 + totalfemale75 + totalfemale80 +totalfemale85)/(totalmale + totalfemale))  %>%
  st_centroid(.) %>% 
  drop_na(proppoverty) %>% 
  mutate(., x = st_coordinates(.)[,1],
         y= st_coordinates(.)[,2])


## Download Poverty Status
pov.status <- map(st, function(x) get_acs(geography = "tract", year = 2019, 
                            variables = c(totalpovertystatus = "S1701_C01_001",
                                          belowpovstatus = "S1701_C02_001"), 
                            survey = "acs5", state = x, geometry = TRUE) %>% 
      pivot_wider(., id_cols = c("GEOID", "geometry"), names_from = "variable", values_from = "estimate") 
) %>% 
  bind_rows() %>% 
  mutate(proppoverty = belowpovstatus/totalpovertystatus)  %>%
  st_centroid(.) %>% 
  drop_na(proppoverty) %>% 
  mutate(., x = st_coordinates(.)[,1],
         y= st_coordinates(.)[,2])