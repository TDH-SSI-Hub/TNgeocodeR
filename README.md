# TNgeocodeR

## Overview

This package includes convenience functions for interfacing with the TN
geocoder API, mapping counties to regions, and accessing shapefiles.

### Download this package

``` r
devtools::install_github("TDH-SSI-Hub/TNgeocodeR")
library("TNgeocodeR")
```

If you can’t install this package, you likely need to install `devtools`
and/or the correct version of `rtools` for your R.

## TN Geocoding

The TN geocoding API should be used for geocoding whenever possible,
since it is updated monthly. `TNgeocodeR` includes functions which
provide an easy interface to the geocoder.

Currently, the only supported service is the geocode address
functionality - `tn_geocode_addresses()`. The first argument is a
dataframe with columns that can be used as inputs to the geocoder. The
`match_on` parameter specifies which columns are to be sent to the
geocoder, and what they represent. In the example below, the dataframe
has a street address column “locAddress”, so we specify that
`Address="locAddress"`. We do the same for city, but the state field is
already named correctly and doesn’t need to be named in the parameter.

``` r
tdh<-data.frame(locAddress='710 James Robertson Pkwy'
           ,locCity='Nashville'
           ,State='TN')

tn_geocode_addresses(tdh, match_on = c(Address='locAddress'
                                       ,City='locCity'
                                       ,'State')
                     )
```

                    locAddress   locCity State Score
    1 710 James Robertson Pkwy Nashville    TN   100
                                          Match_addr   County         X        Y
    1 710 JAMES ROBERTSON PKWY, NASHVILLE, TN, 37203 DAVIDSON -86.78719 36.16781

The default output includes the match score, match address, county, and
X/Y coordinates (longitude and latitude). You can specify the fields the
return by using the `return_fields` parameter. In addition to specific
parameters, you can use `'*'`,`'All'`,`''`, or `NA` to return all
fields, or `'None'` to return the default minimum fields.

``` r
tn_geocode_addresses(tdh, match_on = c(Address='locAddress'
                                       ,City='locCity'
                                       ,'State')
                     , return_fields = c('County','Postal')
                     )
```

                    locAddress   locCity State   County Postal
    1 710 James Robertson Pkwy Nashville    TN DAVIDSON  37203

For a list of valid inputs and outputs use `tn_api_inputs()` and
`tn_api_outputs()`. By default, these return the parameters accepted or
returned by the geocoder. `tn_geocode_addresses()` can handle some
common aliases for important fields (e.g., ‘Street’ is recognized as
‘Address’). Running `tn_api_inputs(special_cases=T)` or
`tn_api_outputs(special_cases=T)` will include additional aliases you
can use with `tn_geocode_addresses()`. In the example below, we use
‘Street’ as an alias for ‘Address’ and ‘Zip’ instead of ‘Postal’.

``` r
tn_geocode_addresses(tdh, match_on = c(Street='locAddress'
                                       ,City='locCity'
                                       ,'State')
                     , return_fields = c('County','Zip')
                     )
```

                    locAddress   locCity State   County   Zip
    1 710 James Robertson Pkwy Nashville    TN DAVIDSON 37203

If addresses are contained in a single field, `tn_geocode_vector()` can
be used. This will still return a dataframe, with one row per input
address.

``` r
addresses<-c('1301 Riverfront Pkwy, Chattanooga'
             ,'665 Mainstream Drive, Nashville')

tn_geocode_vector(addresses, return_fields = c('County','X','Y'))
```

                             SingleLine   County         X        Y
    1 1301 Riverfront Pkwy, Chattanooga HAMILTON -85.32024 35.04179
    2   665 Mainstream Drive, Nashville DAVIDSON -86.81848 36.19551

`tn_county_to_region()` allows you to use the county to derive the
health region. This is case-/space-insensitive and works whether or not
the strings contain ‘county’. This function relies on the `tn_counties`
dataframe, which is also included in the package.

``` r
tn_county_to_region(c('VanBuren','Van Buren','vanburen county','Sumner'))
```

    [1] "Upper Cumberland" "Upper Cumberland" "Upper Cumberland" "Mid-Cumberland"  

## Mapping

Shapefiles for counties, regions, zip codes, and census tracts are
included and named using `tn_*_shapefiles`.

``` r
ggplot(tn_region_shapefiles, aes(fill=NAME)) + 
  geom_sf(show.legend = F)
```

![](README_files/figure-commonmark/unnamed-chunk-7-1.png)

## 
