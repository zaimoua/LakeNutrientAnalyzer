# helper_functions.R

library(sf)
library(leaflet)
library(dplyr)
library(rmapshaper)
library(memoise)
library(stringdist)
library(DT)
library(geojsonio)

# Function to load and simplify GeoJSON
load_geojson <- function(path, keep = 0.01) {
  tryCatch({
    if (!file.exists(path)) {
      stop(paste("File not found:", path))
    }
    geojson <- geojson_read(path, what = "sp")
    geojson <- st_as_sf(geojson)
    geojson <- st_transform(geojson, crs = 4326)
    
    # More aggressive simplification
    geojson <- ms_simplify(geojson, keep = keep, keep_shapes = TRUE)
    
    geojson
  }, error = function(e) {
    message(paste("Error loading GeoJSON:", path))
    message("Error details:", e$message)
    NULL
  })
}

# Function to create WBID map
create_wbid_map <- function(geojson) {
  leaflet(geojson) %>%
    addProviderTiles(providers$Esri.WorldTopoMap) %>%
    setView(lng = -81.5158, lat = 27.6648, zoom = 6) %>%
    addPolygons(
      fillColor = "blue",
      color = "black",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.5,
      layerId = ~WBID,
      label = ~WBID,
      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
    ) %>%
    addLegend(position = "bottomright", 
              colors = c("blue", "red"), 
              labels = c("Unselected WBIDs", "Selected WBIDs"), 
              title = "WBID Status")
}

# Function to handle map clicks
handle_map_click <- function(clicked_wbid, current_selection) {
  if (!is.null(clicked_wbid)) {
    if (clicked_wbid %in% current_selection) {
      setdiff(current_selection, clicked_wbid)
    } else {
      c(current_selection, clicked_wbid)
    }
  } else {
    current_selection
  }
}

# Function to update map selection
update_map_selection <- function(map, selected, all_data) {
  tryCatch({
    print("Selected WBIDs:")
    print(selected)
    print("Structure of all_data:")
    print(str(all_data))
    
    selected_data <- all_data[all_data$WBID %in% selected, ]
    print("Structure of selected_data:")
    print(str(selected_data))
    
    map %>%
      clearGroup("selected") %>%
      addPolygons(
        data = selected_data,
        fillColor = "red",
        color = "red",
        weight = 2,
        opacity = 1.0,
        fillOpacity = 0.7,
        group = "selected",
        layerId = ~paste0("selected_", WBID),
        label = ~sprintf("%s - %s", WBID, WATERBODY_NAME),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      )
  }, error = function(e) {
    print(paste("Error in update_map_selection:", e$message))
    print(traceback())
    # Return the map unchanged if there's an error
    map
  })
}

# Function to perform search
perform_search <- function(search_term, waterbody_info, geojson, max_distance = 0.2) {
  search_term <- toupper(trimws(search_term))
  
  if (nchar(search_term) == 0) {
    return(data.frame())  # Return an empty dataframe if search term is empty
  }
  
  # Function to calculate similarity
  calc_similarity <- function(x, y) {
    1 - stringdist(x, y, method = "lv") / pmax(nchar(x), nchar(y))
  }
  
  # Search in WBID
  wbid_match <- waterbody_info %>%
    mutate(similarity = calc_similarity(WBID, search_term)) %>%
    filter(similarity > (1 - max_distance))
  
  # Search in WATERBODY_NAME
  name_match <- waterbody_info %>%
    mutate(similarity = calc_similarity(WATERBODY_NAME, search_term)) %>%
    filter(similarity > (1 - max_distance))
  
  # Combine results
  results <- bind_rows(wbid_match, name_match) %>%
    distinct(WBID, .keep_all = TRUE) %>%
    select(WBID, WATERBODY_NAME, similarity, everything()) %>%
    arrange(desc(similarity))
  
  if (nrow(results) == 0) {
    showNotification("No matching WBID found. Please refine your search.", type = "warning")
    return(data.frame())  # Return an empty dataframe if no results found
  }
  
  # Merge with geojson to get spatial data
  results <- merge(results, geojson, by = "WBID", all.x = TRUE, suffixes = c("", ".y"))
  
  # Remove duplicate columns and rename as needed
  results <- results %>%
    select(-ends_with(".y")) %>%
    rename_with(~sub("\\.x$", "", .), ends_with(".x"))
  
  return(results)
}

# Function to get search suggestions
get_search_suggestions <- function(partial_term, waterbody_info, max_results = 10) {
  if (is.null(partial_term) || partial_term == "") {
    return(character(0))
  }
  
  partial_term <- toupper(trimws(partial_term))
  
  tryCatch({
    suggestions <- waterbody_info %>%
      filter(grepl(partial_term, WBID, fixed = TRUE) | 
               grepl(partial_term, WATERBODY_NAME, ignore.case = TRUE, fixed = TRUE)) %>%
      mutate(display = paste(WBID, "-", WATERBODY_NAME)) %>%
      select(display) %>%
      distinct() %>%
      head(max_results)
    
    return(suggestions$display)
  }, error = function(e) {
    message("Error in get_search_suggestions: ", e$message)
    message("partial_term: ", partial_term)
    message("Structure of waterbody_info:")
    print(str(waterbody_info))
    return(character(0))
  })
}

# Updated function to display search results
display_search_results <- function(search_results) {
  if (nrow(search_results) == 0) {
    return(p("No results found."))
  }
  
  datatable(
    search_results,
    options = list(pageLength = 10, scrollX = TRUE, scrollY = "300px"),
    selection = "single"
  )
}

# Function to update WBID selection based on search results
update_wbid_selection <- function(session, search_results) {
  if (nrow(search_results) > 0) {
    updateSelectizeInput(session, "wbid", 
                         choices = search_results$WBID,
                         selected = search_results$WBID[1])
  }
}

# Function to update map highlight based on search results
update_map_highlight <- function(map, search_results, all_wbids) {
  if (nrow(search_results) > 0) {
    selected_wbid <- search_results$WBID[1]  # Zoom to the first match
    selected_data <- all_wbids[all_wbids$WBID == selected_wbid, ]
    
    if (nrow(selected_data) > 0) {
      bbox <- st_bbox(selected_data)
      
      map %>%
        clearGroup("searched") %>%
        addPolygons(
          data = selected_data,
          fillColor = "yellow",
          color = "yellow",
          weight = 2,
          opacity = 1.0,
          fillOpacity = 0.7,
          group = "searched",
          layerId = ~paste0("searched_", WBID)
        ) %>%
        fitBounds(
          lng1 = bbox["xmin"], lat1 = bbox["ymin"],
          lng2 = bbox["xmax"], lat2 = bbox["ymax"],
          options = list(padding = c(20, 20))
        )
    } else {
      showNotification("No data available for the selected WBID.", type = "warning")
      map %>% clearGroup("searched")
    }
  } else {
    showNotification("No search results found.", type = "warning")
    map %>% clearGroup("searched")
  }
}

# Function to prepare WBID choices
prepare_wbid_choices <- function(wbids, waterbody_info) {
  choices <- waterbody_info %>%
    filter(WBID %in% wbids) %>%
    mutate(label = paste(WBID, "-", WATERBODY_NAME)) %>%
    select(WBID, label)
  
  setNames(choices$WBID, choices$label)
}

# Function to zoom to Florida
zoom_to_florida <- function(map) {
  map %>% setView(lng = -81.5158, lat = 27.6648, zoom = 6)
}

# Function to reset map view and clear selections
reset_map <- function(map) {
  map %>%
    clearGroup("selected") %>%
    clearGroup("searched") %>%
    setView(lng = -81.5158, lat = 27.6648, zoom = 6)
}

# Function to load flowlines
load_lake_flowlines <- function(path) {
  tryCatch({
    if (!file.exists(path)) {
      stop(paste("File not found:", path))
    }
    flowlines <- st_read(path)
    flowlines <- st_transform(flowlines, crs = 4326)  # Ensure it's in WGS84
    return(flowlines)
  }, error = function(e) {
    message(paste("Error loading lake flowlines:", path))
    message("Error details:", e$message)
    return(NULL)
  })
}
