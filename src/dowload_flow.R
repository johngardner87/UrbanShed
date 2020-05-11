

library(dataRetrieval)
library(tidyverse)
library(sf)


gauge <- read_csv("in/SitestoJG.csv") %>%
  mutate(site_no = str_pad(STAID , 8, pad="0")) %>%
  st_as_sf(coords= c("LNG_GAGE", "LAT_GAGE"), crs=4269)


#sites <- readNWISsite(gauge$site_no[1:4555]) %>% as_tibble() %>%
#  rename(id=site_no)


#download all flow data from all gages
for(i in 1:nrow(gauge)) {
  
  site <- gauge$site_no[i]
  
  dat <- readNWISdv(siteNumbers = site, parameterCd = "00060")
  
  if(length(dat) ==0 | nrow(dat)==0) {next} 
  
  if(any(grepl("Final", names(dat)) ==T)) {
    
    dat <- addWaterYear(dat) %>%
      dplyr::rename(source = agency_cd,  date=Date, flow = X_.Final._00060_00003, 
                    flag = X_.Final._00060_00003_cd) %>%
      dplyr::select(source, site_no, date, waterYear, flow, flag) %>%
      mutate(flow_cms = flow * 0.0283168,
             ice_flag = ifelse(grepl("e", flag), "ice", NA))
    
    write_feather(dat, paste("out/usgsFlow/", "usgs_", site, "_", ".feather", sep="" ))
    
  } else if(any(grepl("Primary", names(dat)) ==T)) {
    
    dat <- addWaterYear(dat) %>%
      dplyr::rename(source = agency_cd,  date=Date, flow = X_.Primary.Stream.Flow._00060_00003, 
                    flag = X_.Primary.Stream.Flow._00060_00003_cd) %>%
      dplyr::select(source, site_no, date, waterYear, flow, flag) %>%
      mutate(flow_cms = flow * 0.0283168,
             ice_flag = ifelse(grepl("e", flag), "ice", NA))
    
    
    write_feather(dat, paste("out/usgsFlow/", "usgs_", site,"_", ".feather", sep="" ))
    
  } else if(any(grepl("Powerhouse", names(dat)) ==T)) {
    
    dat <- addWaterYear(dat) %>%
      dplyr::rename(source = agency_cd,  date=Date, flow = X_Powerhouse.Releases_00060_00003, 
                    flag = X_Powerhouse.Releases_00060_00003_cd) %>%
      dplyr::select(source, site_no, date, waterYear, flow, flag) %>%
      mutate(flow_cms = flow * 0.0283168,
             ice_flag = ifelse(grepl("e", flag), "ice", NA))
    

    write_feather(dat, paste("out/usgsFlow/", "usgs_", site,"_",".feather", sep="" ))
    
  } else if(any(grepl("Combined", names(dat)) ==T)) {
    
    dat <- addWaterYear(dat) %>%
      dplyr::rename(source = agency_cd,  date=Date, flow = X_.Combined._00060_00003, 
                    flag = X_.Combined._00060_00003_cd) %>%
      dplyr::select(source, site_no, date, waterYear, flow, flag) %>%
      mutate(flow_cms = flow * 0.0283168,
             ice_flag = ifelse(grepl("e", flag), "ice", NA))
    
    write_feather(dat, paste("out/usgsFlow/", "usgs_", site,"_", ".feather", sep="" ))
  } 
  
  
  else{
    
    dat <- addWaterYear(dat) %>%
      dplyr::rename(source = agency_cd,  date=Date, flow = X_00060_00003, 
                    flag = X_00060_00003_cd) %>%
      dplyr::select(source, site_no, date, waterYear, flow, flag) %>%
      mutate(flow_cms = flow * 0.0283168,
             ice_flag = ifelse(grepl("e", flag), "ice", NA) )
    
    write_feather(dat, paste("out/usgsFlow/", "usgs_", site,"_",".feather", sep="" ))
  }
  
}


#####################################################################
## read in all gages, count data, do annual means etc.

#path_flow <- "D:/Dropbox/projects/TSS/output/usgsFlow"

flow_stack <- list.files(path="out/usgsFlow", pattern=".feather", full.names = T) %>%
  map_df(~ read_feather(.))


# filter NA flow
# group by site, and count, filter ones with less 10000 Q 
# later filter years that dont have enough data (<360)
flow_stack <- flow_stack %>%
  filter(!is.na(flow_cms), flow_cms >=0) %>%
  group_by(site_no, waterYear) %>%
  mutate(n_days_Wyear = n()) %>%
  filter(n_days_Wyear > 360) %>%
  ungroup()

#calculate long term flow stats
flow_stack_sum <- flow_stack %>%
  group_by(site_no) %>%
  summarise(mean_flow_record = mean(flow_cms, na.rm = T), 
            median_flow_record =median(flow_cms, na.rm=T),
            sd_flow_record = sd(flow_cms, na.rm = T),
            max_flow_record= max(flow_cms, na.rm = T),
            Q10_flow_record= quantile(flow_cms, probs=c(0.1)),
            Q90_flow_record= quantile(flow_cms, probs=c(0.9)),
            mean_ndays = mean(n_days_Wyear))
     
  
# join long term means to site info
flow_stack_sum_join <- flow_stack_sum %>%
  left_join(gauge, by="site_no") %>%
  mutate(mean_flow_record_mm = mean_flow_record *3600 * 24 * mean_ndays *(1/(DRAIN_SQKM*10^6)) *1000,
          median_flow_record_mm = median_flow_record *3600 * 24 * mean_ndays *(1/(DRAIN_SQKM*10^6)) *1000,
          sd_flow_record_mm = sd_flow_record *3600 * 24 * mean_ndays *(1/(DRAIN_SQKM*10^6)) *1000 ,
          max_flow_record_mm = max_flow_record *3600 * 24 * mean_ndays *(1/(DRAIN_SQKM*10^6)) *1000)

# check data to see if makes sense. Probably need to filter ice flags, and redo
# calculation without estimates flows.
ggplot()+
  geom_sf(data=flow_stack_sum_join, 
          aes(geometry=geometry, color=log10(median_flow_record_mm))) +
  scale_color_viridis_c()

# annual summary by water year, join full record summary
flow_stack_sum_yr <- flow_stack %>%
  group_by(site_no, waterYear) %>%
  summarise(mean_flow = mean(flow_cms, na.rm = T), 
            median_flow =median(flow_cms, na.rm=T),
            sd_flow = sd(flow_cms, na.rm = T),
            max_flow = max(flow_cms, na.rm = T),
            Q10_flow = quantile(flow_cms, probs=c(0.1)),
            Q90_flow = quantile(flow_cms, probs=c(0.9)),
            n_days_Wyear = n_days_Wyear[1]) %>%
  ungroup() %>%
  left_join(flow_stack_sum_join %>%
              dplyr::select(-geometry), by="site_no") %>%
  mutate(mean_flow_mm=  mean_flow *3600 * 24 * n_days_Wyear *(1/(DRAIN_SQKM*10^6)) *1000,
         median_flow_mm = median_flow *3600 * 24 * n_days_Wyear *(1/(DRAIN_SQKM*10^6)) *1000,
         sd_flow_mm = sd_flow *3600 * 24 * n_days_Wyear *(1/(DRAIN_SQKM*10^6)) *1000 ,
         max_flow_mm = max_flow *3600 * 24 * n_days_Wyear *(1/(DRAIN_SQKM*10^6)) *1000,
         Q10_flow_mm = Q10_flow *3600 * 24 * n_days_Wyear *(1/(DRAIN_SQKM*10^6)) *1000 ,
         Q90_flow_mm = Q90_flow *3600 * 24 * n_days_Wyear *(1/(DRAIN_SQKM*10^6)) *1000)

# check a few sites
ggplot(flow_stack_sum_yr %>% 
         filter(STANAME  %in% unique(flow_stack_sum_yr$STANAME )[400:420]))+
  geom_line(aes(x=waterYear, y=median_flow_mm)) +
  facet_wrap(~STANAME , scales = "free_x")


## write data to file
write_csv(flow_stack_sum_join %>%
                dplyr::select(-geometry) %>%
                as.data.frame(), "D:/Dropbox/projects/UrbanShed/out/flow_stack_sum_by_site.csv")

write_feather(flow_stack_sum_yr, "out/flow_stack_sum_Wyr.feather")

write_csv(flow_stack_sum_yr, "out/flow_stack_sum_Wyr.csv")

