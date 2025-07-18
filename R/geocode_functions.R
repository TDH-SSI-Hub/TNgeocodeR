
#' API URLs for the TN geocoder
#'
#' @param service NA or geocodeAddresses. If NA, returns the URL for the geocode server.
#'  If geocodeAddresses, returns URL for geocode addresses service.
#'
#' @return URL
#' @export
tn_geocoder_url <- function(service=NA){
  base_url<-"https://tnmap.tn.gov/arcgis/rest/services/LOCATORS/TN_ADDRESSPOINTS/GeocodeServer"
  if(is.na(service)){
    return(base_url)
  }else if(grepl('geocodeAddresses',service,ignore.case = T)){
    return(paste0(base_url,'/geocodeAddresses'))
  }else{
    stop('Invalid service. Must be NA or geocodeAddresses')
  }
}

#' Special input/outputs handled by tn_geocode_addresses()
#'
#' These names are not recognized by the geocoding api, but are handled by tn_geocode_addresses().
#'
#' @return dataframe of aliases and the official names they correspond to.
#' @export
tn_geocoder_specialcases<-function(){
  data.frame(alias=c('State','County','Zip', 'Street','Lat','Lon','Latitude','Longitude')
            ,name=c('region','subregion','postal','address','y','x','y','x'))
}


#' Valid input fields for tn_geocode_addresses()
#'
#' @param special_cases T/F Include output aliases such as Street, Lat, Lon which are not handled by the geocoder,
#'  but which tn_geocode_addresses() handles.
#'
#' @return Vector of names
#' @export
tn_api_inputs<-function(special_cases=F){
  test<-httr::POST('https://tnmap.tn.gov/arcgis/rest/services/LOCATORS/TN_ADDRESSPOINTS/GeocodeServer', body=list(f='pjson'))
  test_res<-jsonlite::fromJSON(rawToChar(test$content))
  ret_val<-unlist(strsplit(unique(c(test_res$addressFields$name,test_res$addressFields$alias)),' or '))
  if(special_cases) ret_val<-c(tn_geocoder_specialcases()$alias,ret_val)
  ret_val
}

#' Valid output fields for tn_geocode_addresses()
#'
#' @param special_cases T/F Include output aliases such as Street, Lat, Lon which are not handled by the geocoder,
#'  but which tn_geocode_addresses() handles.
#'
#' @return Vector of names
#' @export
tn_api_outputs<-function(special_cases=F){
  test<-httr::POST('https://tnmap.tn.gov/arcgis/rest/services/LOCATORS/TN_ADDRESSPOINTS/GeocodeServer', body=list(f='pjson'))
  test_res<-jsonlite::fromJSON(rawToChar(test$content))
  ret_val<-unlist(strsplit(unique(c(test_res$candidateFields$name,test_res$candidateFields$alias)),' or '))
  if(special_cases) ret_val<-c(tn_geocoder_specialcases()$alias,ret_val)
  ret_val
}


#' Function to interface with the TN geocoder.
#'
#' Interface to https://tnmap.tn.gov/arcgis/rest/services/LOCATORS/TN_COMPOSITE/GeocodeServer/geocodeAddresses
#'
#' @param df dataframe to use
#' @param match_on Named character vector of columns in the dataframe that map to geocoded elements (geocoder_name=df_name).
#'  Or an unnamed vector of columns that are valid geocoder inputs.
#'  Or a partially named vector where columns that are not valid geocoder inputs are named as valid inputs.
#'  Valid geocoder inputs can be found using tn_api_inputs(). You may also specify a single column with a combined address using 'SingleLine'='column_name'.
#'  This will cause any other input fields to be ignored.
#' @param return_fields Character vector of valid fields to return. Valid geocoder inputs can be found using tn_api_outputs().
#'  To return all possible fields, use '*','','All', or NA. To return the default minimum fields, use 'None'.
#'  Invalid fields generate a warning and will be ignored.
#' @param exclude_default_fields T/F Should default geocoding fields not listed in return_fields be excluded from the output?
#'  Default fields include: Address, X, Y, and Score. This parameter is ignored if return_fields is '*','','All','None', or NA.
#'
#' @return Dataframe with added columns from geocoder
#' @import progress
#' @export
tn_geocode_addresses<-function(df,
                          match_on=c('Address'='Address'
                                     ,'City'='City'
                                     ,'State'='State'
                                     ,'Postal'='Postal'
                          )
                          , return_fields=c('Score','Match_addr','County','X','Y')
                          , exclude_default_fields=T
){



  # Validate that matching columns exist in df
  missing_col<-match_on[!match_on %in% colnames(df)]
  if(length(missing_col)>0){
    stop(paste0('The following column(s) do not exist in the dataframe: ',paste0(missing_col, collapse=', ')))
  }

  # Load valid inputs and outputs
  valid_in<-tn_api_inputs(T)
  valid_out<-tn_api_outputs(T)

  # Load special case info
  special_in<-tn_geocoder_specialcases()$alias
  special_out<-tn_geocoder_specialcases()$name

  # Add ID col to df
  df$OBJECTID<-1:nrow(df)

  # Create df to send to geocoder using match columns
  geo_df<-data.frame(OBJECTID=df$OBJECTID)

  # Set names to send to geocoder
  new_col<-names(match_on)
  if(is.null(new_col)) new_col <- match_on
  new_col[new_col=='']<-match_on[new_col=='']
  for(sc in 1:length(special_in)){
    new_col[tolower(new_col)==tolower(special_in[sc])]<-special_out[sc]
  }

  single_match<- 'SingleLine' %in% names(match_on)
  if(single_match & length(match_on)>1){
    match_on<-match_on['SingleLine']
    message('Geocoding method set to SingleLine; all other input fields are ignored.')
  }

  # Validate that input fields are recognized by geocoder
  invalid_in<-new_col[!tolower(new_col) %in% tolower(valid_in)]
  invalid_in<-invalid_in[invalid_in!='SingleLine']
  if(length(invalid_in)>0){
    stop(paste0('The following input(s) are not recognized by the geocoder: ',paste0(invalid_in), collapse=', '),
         '\nValid inputs are the following: ',paste0(c(special_in,valid_in), collapse=', '))
  }

  # Validate that output fields are recognized by geocoder
  invalid_out<-return_fields[!tolower(return_fields) %in% tolower(c(valid_out,'State','County','Zip','None','*',''))]
  if(length(invalid_out)>0){
    return_fields<-return_fields[!return_fields %in% invalid_out]
    warning(paste0('The following return field(s) are not recognized by the geocoder and will be ignored: ',paste0(invalid_out), collapse=', '),
            '\nValid return fields are the following: None, ',paste0(c(special_in,valid_out), collapse=', '))
  }

  # Create output field string for geocoder
  outfields<-paste0(trimws(return_fields), collapse = ',')
  for(sc in 1:length(special_in)){
    outfields<-gsub(special_in[sc],special_out[sc],outfields, ignore.case = T)
  }
  outfields_vec<-unlist(strsplit(outfields,','))

  # If set output to everything if no outputs are specified
  if(length(outfields)==0||outfields==''||outfields=='*'||outfields=='NA'||tolower(outfields)=='all'){
    outfields<-'*'
    message('No return fields specified; returning all possible fields')
  }

  # Add score if it isn't there
  if(!grepl('score',outfields,ignore.case = T)&outfields!='*'){
    outfields<-paste0(outfields,',Score')
  }

  # Add data to geocoding df
  for(j in 1:length(match_on)){
    geo_df[,new_col[j]]<-df[,match_on[j]]
  }

  # Create list to fill with output data
  geocode_response<-list()

  # Process in batches of 1000
  batch_size<-1000
  batch_num<-ceiling((nrow(geo_df)/batch_size))

  pb <- progress::progress_bar$new(
    format = "  Geocoding [:bar] :current/:total batches (:percent) in :elapsed eta: :eta",
    total = batch_num, clear = FALSE, width= 80)

  pb$tick(0)
  for(i in 1:batch_num){
    # Limit to current batch
    jlist<-geo_df[((i-1)*batch_size+1):min(i*batch_size,nrow(geo_df)),]

    # Add attributes as a name
    res <- lapply( seq_len( nrow( jlist ) ), function(i) {
      lst <- list( as.list( jlist[i, , drop = F] ) )
      attr( lst, "names" ) <- "attributes"
      lst
    })


    # create post body, content and response format
    body<-list(addresses=jsonlite::toJSON(list( records = res ), auto_unbox = T)
               , f='pjson'
               , outfields=outfields
    )

    # POST
    response <- httr::POST(tn_geocoder_url('geocodeAddresses')
                     , body = body
                     , encode = 'form'
                     #, verbose()
    )

    # Parse the JSON response
    res_df<-tidyr::unnest(as.data.frame(jsonlite::fromJSON(rawToChar(response$content))),everything())

    # add to list
    geocode_response[[length(geocode_response)+1]] <- res_df
    pb$tick(1)

    #message(paste0('Geocoded ',nrow(jlist),' records in batch ',i,' of ',ceiling((nrow(geo_df)/batch_size))))
  }

  # Combine outputs
  all_data<-as.data.frame(data.table::rbindlist(geocode_response, use.names = T))

  # Score summary stats
  message(paste0('Match score mean: ',round(mean(all_data$Score, na.rm = T),1), ' (',round(mean(all_data$Score[all_data$Score!=0], na.rm = T),1),' excluding failed matches)'))
  message(paste0('Match score median: ',round(median(all_data$Score, na.rm = T),1), ' (',round(median(all_data$Score[all_data$Score!=0], na.rm = T),1),' excluding failed matches)'))
  message(paste0('Match score above 90: ',round(100*sum(all_data$Score>90, na.rm = T)/nrow(all_data),1),'% (',round(100*sum(all_data$Score[all_data$Score!=0]>90, na.rm = T)/sum(all_data$Score!=0),1),'% excluding failed matches)'))

  # Only keep one of x/y and X/Y
  if('x' %in% colnames(all_data)&'X' %in% colnames(all_data)){
    remove_x<-'x'
    if('x' %in% return_fields) remove_x<-'X'
    all_data<-all_data[,colnames(all_data)!=remove_x]
  }
  if('y' %in% colnames(all_data)&'Y' %in% colnames(all_data)){
    remove_y<-'y'
    if('y' %in% return_fields) remove_y<-'Y'
    all_data<-all_data[,colnames(all_data)!=remove_y]
  }

  # Optional filter to output columns
  if(exclude_default_fields&outfields!='*'&tolower(return_fields[1])!='none'){
    for(oc in outfields_vec){
      colnames(all_data)<-gsub(paste0('^',oc,'$'),oc,colnames(all_data),ignore.case = T)
    }
    all_data<-all_data[,c('ResultID',outfields_vec)]
  }

  # Rename output fields to match case of return_fields
  for(sc in 1:length(special_in)){
    if(any(grepl(paste0('^',special_out[sc],'$'),outfields_vec, ignore.case = T))& any(grepl(paste0('^',special_in[sc],'$'),return_fields, ignore.case = T))){
      repw<-grep(paste0('^',special_in[sc],'$'),return_fields, ignore.case = T, value=T)
      colnames(all_data)<-gsub(paste0('^',special_out[sc],'$'),repw,colnames(all_data), ignore.case = T)
    }
  }

  # Merge geocoded data to df
  out_data<-merge(df,all_data, by.x = 'OBJECTID', by.y = 'ResultID', all = T)
  out_data<-out_data[,colnames(out_data)!='OBJECTID']

  return(out_data)
}


#' Function to interface with the TN geocoder.
#'
#' A wrapper around tn_geocode_addresses() that takes a vector of addresses as input
#'
#' @param x A vector of addresses
#' @param return_fields Character vector of valid fields to return. Valid geocoder inputs can be found using tn_api_outputs().
#'  To return all possible fields, use '*','','All', or NA. To return the default minimum fields, use 'None'.
#'  Invalid fields generate a warning and will be ignored.
#' @param exclude_default_fields T/F Should default geocoding fields not listed in return_fields be excluded from the output?
#'  Default fields include: Address, X, Y, and Score. This parameter is ignored if return_fields is '*','','All','None', or NA.
#'
#' @return Dataframe with added columns from geocoder
#' @export
tn_geocode_vector<-function(x
                            ,return_fields=c('Score','Match_addr','County','X','Y')
                            , exclude_default_fields=T
                            ){
  tn_geocode_addresses(data.frame(SingleLine=x), match_on = 'SingleLine',return_fields=return_fields,exclude_default_fields=exclude_default_fields)
}


#' Convert TN counties to health regions
#'
#' @param county vector of counties in TN
#'
#' @return vector of health regions
#' @export
tn_county_to_region<-function(county){
  county<-tolower(gsub(' |county','',county, ignore.case = T))
  c2r<-tn_counties
  c2r$County<-tolower(gsub(' ','',c2r$County))
  unlist(sapply(county, function(x) {
    mc<-c2r$Health_Region[x==c2r$County]
    if(length(mc)==0){
      return(NA)
    }else{
      return(mc[1])
    }
  }, USE.NAMES = F)
  )
}

#' Standardize TN county names
#' 
#' Removes " county" and standardizes capitalization and spacing for TN counties.
#'
#' @param county vector of counties in TN
#'
#' @return vector of standardized county names
#' @export
tn_validate_county<-function(county){
  county <- tolower(gsub(" |county", "", county, ignore.case = T))
  c2r <- tn_counties
  c2r$County_merge <- tolower(gsub(" ", "", c2r$County))
  unlist(sapply(county, function(x) {
    mc <- c2r$County[x == c2r$County_merge]
    if (length(mc) == 0) {
      return(NA)
    }
    else {
      return(mc[1])
    }
  }, USE.NAMES = F))
}

#' Validate a county group dataframe
#' 
#' Check that a data frame meets the requirements to function in tn_county_group.
#' There must be 
#'
#' @param county vector of counties in TN
#'
#' @return vector of standardized county names
validate_county_group<-function(group_df){
  cnames<-colnames(group_df)
  if(length(cnames)==2 |
     sum(grepl('county|fips',cnames,ignore.case = T))==1
  ){
    gvar<-cnames[!grepl('county|fips',cnames,ignore.case = T)]
    return(gvar)
  }else{
    return(NA)
  }
}


#' Merge county shapefiles by a group definition
#' 
#' When you provide a dataframe of counties and a grouping variable, this function will create a merged shapefile.
#' You can specify a CRS to use, but the default is the CRS for tn_county_shapefiles.
#'
#' @param group_df Dataframe with exactly 2 columns. One must be the county name or FIPS code (column name must have 'county' or 'fips' in it). The other must be the grouping variable.
#' @param crs EPSG code for the desired output CRS. NA defaults to the CRS for tn_county_shapefile (6576)
#' @buffer buffer Amount to buffer each county before merging.
#'
#' @return Shapefile for the merged counties
#' @export
tn_county_group<-function(group_df, crs=NA, buffer=.000001){
  
  gvar<-validate_region_group(group_df)
  
  if(is.na(gvar)){
    stop('Could not determine grouping variable. Grouping df must have 2 columns (county/fips and a grouping column)')
  }
  
  cvar<-colnames(group_df)[colnames(group_df)!=gvar]
  group_df$Group<-group_df[,gvar]
  
  if(grepl('county',cvar,ignore.case = T)){
    group_df[,cvar]<-tn_validate_county(group_df[,cvar])
    byx<-'NAME'
  }else if(grepl('fips',cvar,ignore.case = T)){
    group_df[,cvar]<-as.integer(group_df[,cvar])
    tn_county_shapefiles$CNTY_FIPS<-as.integer(tn_county_shapefiles$CNTY_FIPS)
    byx<-'CNTY_FIPS'
  }
  
  if(is.na(crs)) crs <- st_crs(tn_county_shapefiles)
  tn_county_shapefiles$NAME[tn_county_shapefiles$NAME=='De Kalb']<-'DeKalb'
  
  merge<-merge(tn_county_shapefiles,group_df, regions, by.x=byx,by.y=cvar, all=T) |>
    dplyr::group_by(Group) |>
    dplyr::summarise(geometry=st_union(st_buffer( geometry,dist=buffer))) |>
    st_set_crs(crs)
}
