
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)


census_api_key('4a0c9141ee3b2dd6e3920c7d11237a1151200029')



acs = load_variables(2018, 'acs5', cache = TRUE) 



pull_data = function(my_state, my_county){
  
  concepts = acs %>% 
    filter(concept == 'TENURE BY OCCUPANTS PER ROOM')
  
  concepts_string = concepts %>% 
    select(1) %>% 
    pull() # this creates a character vector of just the concepts we want
  
  
  raw =  get_acs(geography = 'tract', variables = concepts_string, year = 2018, state = my_state, 
                 county = my_county,geometry = TRUE, cache = TRUE)
  
  fixed_data = concepts %>% 
    select(name, label) %>% 
    right_join(raw, by = c('name' = 'variable')) %>% 
    select(2,3,5,7) %>% 
    spread(key = label, value = estimate, -4)
  
  names(fixed_data) = make.names(names(fixed_data))
  
  fill_data = fixed_data %>% 
    mutate(
      total_under_1 = Estimate..Total..Owner.occupied..0.50.or.less.occupants.per.room + Estimate..Total..Owner.occupied..0.51.to.1.00.occupants.per.room + Estimate..Total..Renter.occupied..0.50.or.less.occupants.per.room + Estimate..Total..Renter.occupied..0.51.to.1.00.occupants.per.room,
      total_over_1 = Estimate..Total..Owner.occupied..1.01.to.1.50.occupants.per.room + Estimate..Total..Owner.occupied..1.51.to.2.00.occupants.per.room + Estimate..Total..Owner.occupied..2.01.or.more.occupants.per.room + Estimate..Total..Renter.occupied..1.01.to.1.50.occupants.per.room + Estimate..Total..Renter.occupied..1.51.to.2.00.occupants.per.room + Estimate..Total..Renter.occupied..2.01.or.more.occupants.per.room
    ) %>% 
    select(GEOID,geometry ,total_under_1, total_over_1, Estimate..Total) %>% 
    mutate(ratio_over_1 = total_over_1/ Estimate..Total)

  roads_data = primary_secondary_roads(my_state) %>% 
    filter(RTTYP %in% c('U','S','I'))
  
  counties_data = counties(cb = TRUE)
  water_data = area_water(my_state,my_county)
  
  cities_data = us.cities %>% 
    filter(country.etc %in% my_state)  %>% 
    mutate(fixed_name = str_replace(name,country.etc,''))
  
  return(list(fill = fill_data, roads = roads_data, counties = counties_data, water =  water_data, cities = cities_data))
}


input_data = bay_area
city_size_floor = 80000

mapper = function(input_data, city_size_floor = 100000000, legend_position = 'bottom left'){ #accepts list object from pull_data function
  map_frame = input_data$fill
  limits = st_bbox(map_frame$geometry)
  roads = input_data$roads
  county = input_data$counties
  water = input_data$water
  cities = input_data$cities %>% 
    filter(pop > city_size_floor & long > as.numeric(limits$xmin) & long < as.numeric(limits$xmax) &
             lat>(as.numeric(limits$ymin) & lat < as.numeric(limits$ymax))) 
  
  legend_placer = function(position = 'bottom left'){
    if(position == 'bottom left'){
      ret = (list(c(0,0),c(0.02,0.04)))
    }else if(position == 'top left'){
      ret =(list(c(0,1),c(0.02,0.96)))
    }else if (position == 'bottom right'){
      ret=(list(c(1,0),c(0.98,0.04)))
    }else if(position == 'top right'){
      ret=(list(c(1,1),c(0.98,0.96)))
    }else(stop('invalid input: must supply one of: bottom left, bottom right, top left, top right'))
    return(theme(legend.justification = ret[[1]], legend.position = ret[[2]]))
  }
  
  ggplot() +
    theme_void()  + #whiteout
    legend_placer(legend_position) +
    labs(fill = 'Share of housing\nunits with more\nthan one person\nper room')+ #label legend
    geom_sf(data = county, alpha = 1, fill = '#e6e6e6', color = NA,aes(geometry = geometry)) + #grey landmass
    geom_sf(data = map_frame, alpha = 1,color = NA, aes(geometry = geometry, fill = ratio_over_1)) + #our census map
    scale_fill_gradient(low = '#ffff99', high = '#b10026', na.value = '#ffffcc' ) + # custom fill palette
    geom_sf(data = water, fill = '#cce6ff',color = NA, aes(geometry = geometry)) + #add water
    theme(panel.background = element_rect(fill = '#cce6ff', color = NA)) + #blue background to mesh with water
    geom_sf(data = roads, color = 'white', aes(geometry = geometry)) + #add roads
    geom_text(color = 'black',data = cities,check_overlap = TRUE ,aes(x = long, y = lat, label = fixed_name)) + #add city names
    coord_sf(xlim = c(as.numeric(limits$xmin),as.numeric(limits$xmax)), ylim = c(as.numeric(limits$ymin),as.numeric(limits$ymax))) + #set framing(very important)
    NULL
}

#county = counties(state = 'California')
bay_area = pull_data('CA',c('San Francisco','San Mateo','Santa Cruz','Santa Clara','Alameda','Contra Costa'))
nyc = pull_data('NY',c('Bronx','New York','Queens','Kings'))

atx = pull_data('TX',c('Travis','Williamson','Hays','Caldwell','Bastrop'))

cali = pull_data('CA',c('San Francisco','San Mateo','Santa Cruz','Santa Clara','Alameda','Contra Costa','Los Angeles'))

mapper(bay_area, 70000)

mapper(atx, 40000, legend_position = 'top left')

mapper(cali)
mapper(nyc, legend_position = 'top left')





