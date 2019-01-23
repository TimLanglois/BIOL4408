function(easting, northing, zone, format_out = c('deg', 'dms')){
  
  format_out <- match.arg(format_out)
  
  # Helper function to convert from decimal dms to dd:mm:ss
  Deg2dms <- function(x){
    dd <- trunc(x)
    mm <- trunc(60*(x - dd))
    ss <- (x-dd-(mm/60))*3600
    paste(dd, abs(mm), abs(ss), sep = ":")
  } 
  
  #_____________________________________________
  # define ellipsoid
  
  # GRS80 - as used in Australia
  # http://www.ga.gov.au/geodesy/datums/redfearn_grid_to_geo.jsp
  # https://en.wikipedia.org/wiki/GRS_80
  # http://www.ga.gov.au/geodesy/datums/redfearn_grid_to_geo.jsp
  
  
  axis_semimajor <- 6378137.0
  inv_flattening <- 298.257222101
  easting_false <- 500000
  northing_false <- 10000000
  central_scalefactor <- 0.9996
  zone_width <- 6 # degrees
  centralmeridian_zone0 <- -183
  
  # Derived values
  axis_semiminor <- axis_semimajor*(1 - 1/inv_flattening)
  eccentricity <- 2*(1/inv_flattening) - (1/inv_flattening)^2
  n <- (axis_semimajor - axis_semiminor)/(axis_semimajor + axis_semiminor)
  G <- (pi/180) * axis_semimajor * (1 - n) * (1 - n^2) * (1 + 9*n^2/4 + 225*n^4/64)
  
  # Calculations
  
  easting_rel <- easting - easting_false
  easting_rel_scaled <- (easting - easting_false)/central_scalefactor
  
  northing_rel <- northing - northing_false
  northing_rel_scaled <- (northing - northing_false)/central_scalefactor
  
  
  xsigma <- northing_rel_scaled*(pi/180)/G
  
  latitude_footpoint <- xsigma + (3*n/2 - 27*n^3/32)*sin(2*xsigma) +
    (21*n^2/16 - 55*n^4/32)*sin(4*xsigma) +
    (151*n^3/96)*sin(6*xsigma) +
    (1097*n^4/512)*sin(8*xsigma)
  
  
  # Radii of curvature
  rho <- axis_semimajor * (1 - eccentricity)/(1 - eccentricity*sin(latitude_footpoint)^2)^1.5
  nu <- axis_semimajor / (1 - eccentricity*sin(latitude_footpoint)^2)^0.5
  radii_ratio <- nu/rho
  
  
  x1 <- easting_rel_scaled/nu
  x2 <- (easting_rel_scaled)^2/(nu * rho)
  Tan1 <- tan(latitude_footpoint)
  
  # Latitude 
  
  latitude_rad <- latitude_footpoint + 
    -(Tan1/(central_scalefactor * rho)) * x1*easting_rel/2 +
    (Tan1/(central_scalefactor * rho)) * x1^3 * easting_rel/24 *
    (-4 * radii_ratio^2 + 9 * radii_ratio * (1- Tan1^2) + 12 * Tan1^2) +
    -(Tan1/(central_scalefactor*rho)) *
    (x1^5*easting_rel/720) * 
    (8*radii_ratio^4 * (11 - 24*Tan1^2) -
       12*radii_ratio^3 * (21 - 71*Tan1^2) +
       15*radii_ratio^2 * (15 - 98*Tan1^2 + 15*Tan1^4) + 
       180*radii_ratio*(5*Tan1^2 - 3*Tan1^4) + 360*Tan1^4) +
    -(Tan1/(central_scalefactor*rho)) *
    (x1^7*easting_rel/40320) *
    (1385 + 3633 * Tan1^2 + 4095*Tan1^4 + 1575*Tan1^6)
  
  latitude <- latitude_rad*180/pi
  
  
  # Longitude
  central_meridian_rad <- (pi/180) * (zone * zone_width +  centralmeridian_zone0 )
  
  longitude_rad <- central_meridian_rad + 
    1/cos(latitude_footpoint) * x1 +
    -1/cos(latitude_footpoint)*(x1^3/6)*(radii_ratio+2*Tan1^2) +
    1/cos(latitude_footpoint)*(x1^5/120)*(-4*radii_ratio^3*(1-6*Tan1^2)+radii_ratio^2*(9-68*Tan1^2)+72*radii_ratio*Tan1^2+24*Tan1^4) +
    -1/cos(latitude_footpoint)*(x1^7/5040)*(61+662*Tan1^2+1320*Tan1^4+720*Tan1^6)
  
  longitude <- longitude_rad*180/pi
  
  if(format_out == "dms"){
    latitude <- Deg2dms(latitude)
    longitude <- Deg2dms(longitude)
  }
  
  
  return(data.frame (lat = latitude, lon = longitude))
  
  
}
