#' Loads the squidpop template into R as a list of dataframes
#'
#' Imports all the tabs in the squidpop template into a list of r dataframes
#'
#' @importFrom magrittr "%>%"
#' @param xlsx path to the squidpop excel workbook
#' @return r dataframe
#' @export
readSquid <- function(xlsx) {
  metadata <- readxl::read_excel(xlsx, sheet = "Metadata", skip = 5, col_names = c("var", "value")) %>% tidyr::spread(var, value)

  locations <- readxl::read_excel(xlsx, sheet = "Locations")
  data <- readxl::read_excel(xlsx, sheet = "Data")
  vocab <- readxl::read_excel(xlsx, sheet = "Vocab")
  schema <- readxl::read_excel(xlsx, sheet = "Schema")

  # tabs as dataframe

  tab_dfs <- list("metadata"=metadata, "locations"=locations, "data"=data, "vocab"=vocab, "schema"=schema)

  return(tab_dfs)
}

#' Flattens the squidpop data to a singlefile for archiving and analysis
#'
#' Joins the data and locations tabs using the locationIDs and saves result
#'
#' @importFrom magrittr "%>%"
#' @param df_tabs list of dataframes
#' @return r dataframe
#' @export
flattenSquid <- function(df_tabs) {
  data <- df_tabs$data
  locations <- df_tabs$locations

  # joins locations to the data
  data_w_coords <- dplyr::left_join(data, locations, by=c("location"="locationID"))

  return(data_w_coords)
}


#' Creates a simple features (sf) object from a dataframe with latitude and longitude values
#'
#' @importFrom magrittr "%>%"
#' @param dataframe dataframe with latitude and longitude
#' @param longitude fieldname that contains the longitude values
#' @param latitude fieldname that contains the latitude values
#' @param default_crs coordinate reference system
#' @return simple feature object
#' @export
df2sf <- function(dataframe, longitude="decimalLongitude", latitude="decimalLatitude", default_crs="+proj=longlat +datum=WGS84"){
  data_sf <- dataframe %>% sf::st_as_sf(coords=c(longitude, latitude), crs=default_crs)
  return(data_sf)
}


#' Writes the squidpop xlsx to an eml file
#'
#' @importFrom magrittr "%>%"
#' @param xlsx path to the input squidpop excel workbook
#' @param csv_file path to the export flattend csv
#' @param eml_file path to the export eml
#' @return simple feature object
#' @export
packageSquid <- function(xlsx, csv_file, eml_file){
  tabs <- readSquid(xlsx) # returns a named list of dataframes



  # Generate EML
  print("Making EML")
  attributes <- tabs$schema %>%
    dplyr::filter(Tab %in% c("Data", "Locations")) %>%
    dplyr::select(-c(Tab))

  # TODO - forces all number to be reals
  attributes <- attributes %>%
    dplyr::mutate(numberType=ifelse(attributeType=="numeric", "real", NA)) %>%
    dplyr::mutate(definition=attributeDefinition)

  # factors
  factors <- tabs$vocab %>%
    dplyr::rename("attributeName"="Field", "code"="Value", "definition"="Definition")

  # attribute types
  attribute_classes <- attributes$attributeType

  # attribute list
  attributeList <- EML::set_attributes(attributes, as.data.frame(factors), col_classes = attribute_classes)

  # data file format "physical"
  physical <- EML::set_physical(csv_file)

  dataTable <- new("dataTable",
                   entityName=csv_file,
                   entityDescription="data with lat/longs",
                   physical=physical,
                   attributeList=attributeList)

  # coverage

  # create flatten file
  data_coords <- flattenSquid(tabs)

  # get the first and last timestamps to use as the time coverage range
  timestamp_range <- data_coords %>%
    dplyr::summarise(min_timestamp = min(deploymentTimestamp), max_timestamp = max(checkTimestamp))
  first_date <- lubridate::date(timestamp_range$min_timestamp[1])
  last_date <- lubridate::date(timestamp_range$max_timestamp[1])

  #bounding box calculated above
  bbox <- sf::st_bbox(df2sf(data_coords))

  coverage <- EML::set_coverage(begin = toString(first_date), end = toString(last_date), geographicDescription = "test description", west=bbox["xmax"], east=bbox["xmin"], north=bbox["ymax"], south=bbox["ymin"])

  # people
  creator <- as(as.person(paste(tabs$metadata$Creator, " <", tabs$metadata$CreatorContact, "> ", "[cre]", sep="")), "creator")
  more_people <- tabs$metadata$AdditionalPeople %>% stringr::str_split(pattern="\\|", simplify = TRUE) %>% stringr::str_trim(side = "both")

  p <- as.person(more_people)
  # change roles to authors
  for(i in 1:length(p)){
    p[i]$role <- "aut"
  }

  associatedParty <- as(p, "associatedParty")

  # publisher
  publisher <- new("publisher", organizationName="Smithsonian Marine Global Earth Observatory")

  # contact
  data_contact <- as.person("Andy Bell <bellan@si.edu> [com]")
  c <-as(data_contact, "creator")
  contact <- new("contact", individualName=c@individualName, electronicMail=c@electronicMailAddress, organization="Smithsonian Marine Global Eath Observatory")

  #
  keywordSet <- c(new("keywordSet", keywordThesaurus="MarineGEO squidpops", keyword=c("bait", "squidpop", "fish")))
  pubDate <- lubridate::now() %>% lubridate::date()
  title <- tabs$metadata$Title
  abstract <- tabs$metadata$Abstract

  dataset <- new("dataset",
                 title=title,
                 creator=creator,
                 pubDate=toString(pubDate),
                 abstract=abstract,
                 associatedParty=associatedParty,
                 keywordSet=keywordSet,
                 coverage=coverage,
                 contact=contact,
                 dataTable=dataTable)
  eml <- new("eml",
             packageId = "bcfb56f1-2de6-4f2e-b09b-11e833e27187",
             system='uuid',
             dataset=dataset)

  EML::write_eml(eml, eml_file)
  #EML::eml_validate(eml_file)

  return()
}
