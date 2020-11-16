


get_legis_geos <- function (state) {
  
  states <- tigris::states(cb = T) 
  state1 <- subset(states, NAME == state)
  
  upper_house <- tigris::state_legislative_districts(
    state = state, house = 'upper', cb = T) %>%
    mutate(district_code = as.integer(substr(SLDUST, 2, 3))) 
  
  lower_house<- tigris::state_legislative_districts(
    state = state, house = 'lower', cb = T) %>%
    mutate(district_code = as.integer(substr(SLDLST, 2, 3))) 
  
  counties <- tigris::counties(state = state, cb = T)
  cds_all <- tigris::congressional_districts(cb = T) 
  cds <- subset(cds_all, STATEFP == state1$GEOID) 
  places <- tigris::places(state = state, cb =T)
  
  list('upper_house' = upper_house, 
       'lower_house' = lower_house, 
       'counties' = counties, 
       'cds' = cds, 
       'places' = places)
}