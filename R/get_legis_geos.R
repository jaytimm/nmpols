


get_legis_geos <- function (state) {

  states <- tigris::states(cb = T)
  state1 <- subset(states, NAME == state)

  upper_house <- tigris::state_legislative_districts(
    state = state, house = 'upper', cb = T)
  upper_house$district_code <- as.integer(substr(upper_house$SLDUST, 2, 3))

  lower_house <- tigris::state_legislative_districts(
    state = state, house = 'lower', cb = T)
  lower_house$district_code <- as.integer(substr(lower_house$SLDLST, 2, 3))

  counties <- tigris::counties(state = state, cb = T)
  cds_all <- tigris::congressional_districts(cb = T)
  cds <- subset(cds_all, STATEFP == state1$GEOID)
  places <- tigris::places(state = state, cb =T)
  precincts <- tigris::voting_districts(state = state)
  precincts <- precincts %>%
    mutate(NAME10 = gsub('^Do.* Ana County',
                         'Dona Ana County',
                         NAME10))

  list('upper_house' = upper_house,
       'lower_house' = lower_house,
       'counties' = counties,
       'cds' = cds,
       'places' = places,
       'precincts' = precincts)
}
