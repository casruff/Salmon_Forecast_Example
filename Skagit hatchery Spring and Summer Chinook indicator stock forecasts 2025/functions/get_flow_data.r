get_flow_data<-function(flow_site,min_year,max_year){
  
  ## flow gage ID
  flow_site <- flow_site  
  ## get URL for flow data from USGS
  flow_url <- paste0("https://waterdata.usgs.gov/nwis/dv",
                     "?cb_00060=on",
                     "&format=rdb",
                     "&site_no=",flow_site,
                     "&begin_date=",min(dat$Year),"-01-01",
                     "&end_date=",max(dat$Year),"-12-31")
  
  
  ## raw flow data from USGS
  flow_raw <- read_lines(flow_url)
  ## lines with metadata
  hdr_flow <- which(lapply(flow_raw, grep, pattern = "\\#")==1, arr.ind = TRUE)
  ## print flow metadata
  print(flow_raw[hdr_flow], quote = FALSE)
  
  ## flow data for years of interest
  dat_flow <-  read_tsv(flow_url,
                        col_names = FALSE,
                        col_types = "ciDdc",
                        skip = max(hdr_flow)+2)
  colnames(dat_flow) <- unlist(strsplit(tolower(flow_raw[max(hdr_flow)+1]),
                                        split = "\\s+"))
  head(dat_flow)
  
  ## get URL for flow data from USGS
  flow_url <- paste0("https://waterdata.usgs.gov/nwis/dv",
                     "?cb_00060=on",
                     "&format=rdb",
                     "&site_no=",flow_site,
                     "&begin_date=",min(dat$Year),"-01-01",
                     "&end_date=",max(dat$Year),"-12-31")
  
  
  ## keep only relevant columns
  flow <- dat_flow[c("datetime", grep("[0-9]$", colnames(dat_flow), value = TRUE))]
  ## nicer column names
  colnames(flow) <- c("Date","CFS")
  
  return(flow)
  
  # ## flow by year & month
  # dat_flow$year <- as.integer(format(dat_flow$date,"%Y"))
  # dat_flow$month <- as.integer(format(dat_flow$date,"%m"))
  # dat_flow <- dat_flow[,c("year","month","flow")]
  # 

  
  # flow<-read_table(flow_url,skip=max(hdr_flow))%>%
  #   dplyr::rename(Date=`20d`,CFS=`14n`)%>%
  #   dplyr::select(Date,CFS)
}