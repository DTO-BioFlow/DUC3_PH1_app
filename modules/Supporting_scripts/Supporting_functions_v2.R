#functions for PH1/FW5 code


#function for adding an assessment area ID to the data, depending on whether using polygon or multipolygon data. 
create_assess_id <- function(x){
  
  if("polygon_wkt" %in% colnames(x)){
    polys <- factor(unique(x$polygon_wkt), levels=unique(x$polygon_wkt))
    polys_new <- paste0(as.numeric(polys))
    
    df_assess_id <- data.frame(polygon_wkt=polys,
                               assess_id=polys_new)
    
    temp <- x %>%
      left_join(df_assess_id) %>%
      mutate(polygon_wkt = assess_id,
             assess_id=NULL) %>%
      rename("assess_id" = polygon_wkt)

  } else {
    
    df_assess_id <- data.frame(polygon_wkt=NA,
                               assess_id="1")
    
    temp <- x %>%
      mutate(assess_id = "1")
    
  }
  
  output <- list()
  output[[1]] <- temp
  output[[2]] <- df_assess_id
  return(output)
}

#function for plotting the polygons in maps to provide a spatial reference for multipolygon data
plot_polys <- function(x, buff=2){
  
  if(all(!is.na(x$polygon_wkt))){
    
    #load spatial packages
    list.of.packages <- c("rnaturalearth", "sf")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages, repos = "http://packages.ropensci.org", type = "source")
    lapply(list.of.packages, require, character.only = TRUE)
    rm(list.of.packages, new.packages)
    
    #load shapefile of European landmass for display
    coast <- rnaturalearth::ne_countries(returnclass = "sf", scale = "small")
    
    #convert WKT string to sf object
    polys <- st_as_sf(x, wkt="polygon_wkt")
    st_crs(polys) <- 4326
    polys <- st_transform(polys, crs=st_crs(coast))
    
    #determine appropriate plot limits
    buff <- buff
    lims <- as.numeric(st_bbox(polys))
    extent_rad <- max(c(lims[3] - lims[1], lims[4] - lims[2]))/2 + buff
    centre <- c(mean(c(lims[1], lims[3])), mean(c(lims[2], lims[4])))
    lims <- c(centre[1]-extent_rad, centre[2]-extent_rad, centre[1]+extent_rad, centre[2]+extent_rad)
    
    gg_list <- list()
    for(i in 1:nrow(x)){
      
      temp_poly <- as.character(x$assess_id)[[i]]
      
      gg_temp <- ggplot()+
        geom_sf(data=coast, inherit.aes=F, fill="grey80", colour="grey40", lwd = 0.2)+
        geom_sf(data=polys, colour="black", fill=NA, show.legend=FALSE, lwd = 0.2)+
        geom_sf(data=filter(polys, assess_id==temp_poly), fill="red", alpha=0.5, lwd = 0.2, show.legend=FALSE)+
        coord_sf(xlim=c(lims[1], lims[3]), ylim=c(lims[2], lims[4]), expand=FALSE)+
        theme_bw()+
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust=1, vjust=1),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.title=element_text(size=rel(0.5)),
              legend.text=element_text(size=rel(0.4)),
              legend.key=element_rect(fill=NA),
              legend.key.size = unit(0.27, 'cm'),
              legend.margin=margin(0,0,0,0))+
        guides(fill=guide_legend(ncol=1))
      
      gg_list[[temp_poly]] <- gg_temp
      
    }
    return(gg_list)
  }  else {
    return(NA)
  }
}

#function for log transforming the abundance data
log_transform <- function(x, method=1){
  
  if(method==1){
    
    output <- x %>%
      group_by(assess_id, lifeform) %>%
      filter(!all(abundance == 0)) %>%
      dplyr::mutate(min_non_zero = min(abundance[abundance != min(abundance, na.rm=T)], na.rm=T)) %>%
      dplyr::mutate(abundance = log10(abundance + min_non_zero*0.5)) %>%
      ungroup() %>%
      dplyr::select(-min_non_zero)
    
  } else if(method==2) {
    
    output <- x %>%
      dplyr::mutate(abundance = log10(abundance + 1))
    
  } else {
    
    print("ERROR: invalid input for method")
    
  }
  
  return(output)
}

#function for remove years from time series with less than n months of interpolated data and determine proportion of years removed
clean_years <- function(x, thr){
  
  temp <- x %>%
    group_by(assess_id, lifeform) %>%
    dplyr::mutate(orig_year = length(unique(year))) %>% #count number of years per cell in the original data
    ungroup() %>%
    filter(!is.na(abundance)) %>%
    group_by(assess_id, lifeform, year) %>% 
    dplyr::mutate(n=n()) %>% #count number of months of data within each year per cell
    ungroup() %>%
    filter(n < thr) %>% #extract years with less than n months of real data
    group_by(assess_id, lifeform) %>%
    dplyr::mutate(prop_years_removed = length(unique(year)) / orig_year) %>% #determine proportion of years of data removed per grid cell
    ungroup()
  
  if(nrow(temp) > 0){
    
    print_out <- temp %>%
      dplyr::select(assess_id, prop_years_removed) %>%
      distinct() %>%
      bind_rows(data.frame(assess_id=unique(x$assess_id)[!unique(x$assess_id) %in% temp$assess_id] )) %>%
      mutate(prop_years_removed = ifelse(is.na(prop_years_removed), 0, prop_years_removed)) %>%
      arrange(as.integer(assess_id))
    
    output <- x %>%
      left_join(data.frame(assess_id=temp$assess_id,
                           year=temp$year,
                           exclude=TRUE),
                by=c("assess_id", "year")) %>%
      mutate(exclude = ifelse(is.na(exclude),FALSE,TRUE)) %>%
      mutate(abundance = ifelse(exclude==TRUE, NA, abundance)) %>%
      dplyr::select(-exclude)
    
    print(print_out)
    
  } else {
    
    output <- x
    
    print(paste("No years removed"))
  }
  
   
  
  return(output)
}

#function for filling month gaps in the time series
fill_gaps <- function(x, max_gap = 3){
  
  #arrange the data to allow for NA interpolation across time
  temp <- x %>% 
    arrange(assess_id, lifeform, year, month) %>%
    group_by(assess_id, lifeform) %>%
    dplyr::mutate(abundance_interp = zoo::na.approx(abundance, maxgap = max_gap, rule = 2)) %>%
    ungroup()
  
  #print the number of NA months replaced in each assess_id
  print_out <- temp %>%
    group_by(assess_id, lifeform, year) %>%
    filter(!all(is.na(abundance))) %>%
    ungroup() %>%
    arrange(assess_id, lifeform, year, month) %>%
    filter(is.na(abundance)) %>%
    dplyr::select(assess_id, lifeform, abundance, abundance_interp) %>%
    group_by(assess_id, lifeform) %>%
    dplyr::summarise(prop_months_filled = length(!is.na(abundance_interp) & is.na(abundance)) / length(is.na(abundance)),
                     .groups="drop") %>%
    dplyr::select(-lifeform) %>%
    distinct() %>%
    arrange(as.integer(assess_id))
  
  print(print_out)
  
  output <- temp %>%
    mutate(abundance = abundance_interp) %>%
    dplyr::select(-abundance_interp) %>%
    filter(!is.na(abundance))
  
  return(output)
}

#construct a dataframe of relevant lifeform pair comparisons
df_lf <- rbind(data.frame(V1 = "diatom", V2 = "dinoflagellate"),
               data.frame(V1 = "tychopelagic_diatoms", V2 = "pelagic_diatoms"),
               data.frame(V1 = "lg_copepods", V2 = "sm_copepods"),
               data.frame(V1 = "holoplankton", V2 = "meroplankton"),
               data.frame(V1 = "lg_phyto", V2 = "sm_phyto"),
               data.frame(V1 = "phytoplankton", V2 = "non_carniv_zoo"),
               data.frame(V1 = "crustacean", V2 = "gelatinous"),
               data.frame(V1 = "gelatinous", V2 = "fish_larvae")
)


#function for extracting a dataframe for a particular time period
dataSelect <- function(x, lf, lims){
  
  lifeforms <- intersect(unique(x$lifeform), as.vector(unlist(lf)))
  
  output <- x %>%
    filter(lifeform %in% all_of(as.vector(unlist(lf))),
           year>=lims[1] & year<=lims[2]) %>%
    pivot_wider(names_from = lifeform, values_from = abundance) %>%
    arrange(assess_id, year, month) %>%
    relocate(assess_id) %>% 
    drop_na()

  return(output)
}


#quality control steps to ensure PI results are reliable
qc_ref <- function(x, ind_years = 3, ind_months = 30, rep_months = 2){
  
  output <- x %>%
    dplyr::select(year, month, assess_id, num_samples) %>%
    filter(num_samples > 0) %>%
    group_by(assess_id) %>%
    filter(length(unique(year)) >= ind_years) %>% # at least three years of non-interpolated samples
    filter(length(unique(paste(year,month))) >= ind_months) %>% # at least 30 individual months represented
    ungroup() %>%
    group_by(assess_id, month) %>%
    filter(length(unique(year)) >= rep_months) %>% # each month represented at least twice
    ungroup() %>%
    dplyr::select(assess_id) %>%
    distinct() %>%
    left_join(df_ref, by="assess_id") #subset the reference data to remove assessment areas which do not meet minimum criteria
  
  #message to user to inform why the script fails if no assessment areas pass this step
  if(nrow(output) == 0){
    print("ERROR: The reference data do not meet the minimum criteria for the indicator assessment")
  }
  
  return(output)
}


#function to prepare the reference envelopes for the multiple lifeform pairs comparisons
find_envAll <- function(x, lf){
  
    #determine relevant lifeform pairs for the dataset
    x_data_id <- x

    #find the relevant lifeform pairs
    x_data_id <- x_data_id[,colSums(is.na(x_data_id))<nrow(x_data_id)]
    
    lf_data_id <- lf %>%
      filter(V1 %in% all_of(colnames(x_data_id)),
             V2 %in% all_of(colnames(x_data_id)))
    
    main_outer <- data.frame()
    main_inner <- data.frame()
    for(i in 1:nrow(lf_data_id)){
      temp_lf <- as.vector(unlist(lf_data_id[i,]))
      
      temp_x <- x_data_id %>%
        dplyr::select(assess_id, all_of(temp_lf))
      
      assess_id_list <- sort(unique(temp_x$assess_id))
      
      df_outer <- data.frame()
      df_inner <- data.frame()
      for(j in 1:length(assess_id_list)){
        temp_pair <- subset(temp_x, assess_id == assess_id_list[j])
        
        #command to skip envelope fitting for data with no variance
        abort <- ifelse(sd(as.vector(unlist(temp_pair[,2]))) == 0 | 
                          sd(as.vector(unlist(temp_pair[,3])))==0 |
                          all(is.na(as.vector(unlist(temp_pair[,2])))) |
                          all(is.na(as.vector(unlist(temp_pair[,3])))), TRUE, FALSE)
        
        if(abort==FALSE){
          
          envPts <- findEvn(as.vector(unlist(temp_pair[,2])),
                            as.vector(unlist(temp_pair[,3])),
                            p=0.9,
                            sc=TRUE)
          envPts_unlist <- rbindlist(envPts, fill=TRUE)
          temp_outer <- data.frame(outX=envPts_unlist$outX[complete.cases(envPts_unlist$outX)],
                                   outY=envPts_unlist$outY[complete.cases(envPts_unlist$outY)],
                                   assess_id = assess_id_list[j])
          temp_inner <- data.frame(inX=envPts_unlist$inX[complete.cases(envPts_unlist$inX)],
                                   inY=envPts_unlist$inY[complete.cases(envPts_unlist$inY)],
                                   assess_id = assess_id_list[j])
          
          df_outer <- rbind(df_outer, temp_outer)
          df_inner <- rbind(df_inner, temp_inner)
        }
      }
      
      if(nrow(df_outer) > 0 & nrow(df_inner) > 0){
        
        df_outer$lf_pair <- paste(temp_lf, collapse="-")
        df_inner$lf_pair <- paste(temp_lf, collapse="-")

        main_outer <- rbind(main_outer, df_outer)
        main_inner <- rbind(main_inner, df_inner)
        
        }

  }
  main_list <- list(main_outer, main_inner)
  return(main_list)
}


#function to find the lifeform pairs indicator from the reference envelopes and comparison data
PIcalcAll <- function(x, y, z, lf){
  
  main_outer_data_id <- x[[1]]
  main_inner_data_id <- x[[2]]
    
    main_output <- data.frame()
    for(i in 1:length(unique(main_outer_data_id$lf_pair))){
      temp_lf <- unlist(strsplit(sort(unique(main_outer_data_id$lf_pair))[i], "-"))
      
      df_outer <- subset(main_outer_data_id, lf_pair == paste(temp_lf, collapse="-"))
      df_inner <- subset(main_inner_data_id, lf_pair == paste(temp_lf, collapse="-"))
      
      df_y <- y %>%
        dplyr::select(assess_id, all_of(temp_lf))
      
      df_z <- z %>%
        dplyr::select(assess_id, all_of(temp_lf))
      
      assess_ids <- intersect(unique(df_y$assess_id), unique(df_z$assess_id))
      
      piList <- data.frame()
      for(j in 1:length(assess_ids)){
        
        assess_id_temp <- assess_ids[j]
        
        temp_outer <- subset(df_outer, assess_id == assess_id_temp)
        temp_inner <- subset(df_inner, assess_id == assess_id_temp)
        temp_y <- subset(df_y, assess_id == assess_id_temp)
        
        #arrange the envelope data back into a list
        envelopePts <- list("EnvOuter"=data.frame("outX" = temp_outer$outX,"outY"=temp_outer$outY),
                            "EnvInner"=data.frame("inX" = temp_inner$inX,"inY" = temp_inner$inY))
        
        compDat <- data.frame(y1 = as.vector(unlist(temp_y[,2])),
                              y2 = as.vector(unlist(temp_y[,3])))
        
        #add labelling variables to PI results dataframe
        df_refPoints <- df_z %>%
          group_by(assess_id) %>%
          dplyr::summarise(refPoints = n())
        
        #command to skip envelope fitting for data with no reference envelope
        abort <- ifelse(nrow(envelopePts[[1]]) == 0 & nrow(envelopePts[[2]]) == 0, TRUE, FALSE)
        
        if(abort==FALSE){
          piPts <- PIcalc(compDat, envelopePts, 0.9)
          piPts <- do.call(cbind.data.frame, piPts)
          piPts$assess_id <- assess_id_temp
          piPts$refPoints <- df_refPoints$refPoints[1]
          
          piList <- rbind(piList, piPts)
        }
      }
      
      piList <- cbind(lf_pair = paste(temp_lf, collapse="-"), piList)
      main_output <- rbind(main_output, piList)
    }
    
    main_output <- dplyr::rename(main_output, binomialProbability = 'binomial probability') %>%
      relocate(assess_id) %>%
      arrange(as.integer(assess_id), lf_pair)

   return(main_output)
}

#function to find the lifeform pairs indicator from the reference envelopes and comparison data
PIcalcAnnual <- function(x, y, z, lf){
  
  main_outer <- x[[1]]
  main_inner <- x[[2]]
  
  main_output <- data.frame()
  for(i in 1:length(unique(main_outer$lf_pair))){
    temp_lf <- unlist(strsplit(sort(unique(main_outer$lf_pair))[i], "-"))
    
    df_outer <- subset(main_outer, lf_pair == paste(temp_lf, collapse="-"))
    df_inner <- subset(main_inner, lf_pair == paste(temp_lf, collapse="-"))
    
    df_y <- y %>%
      select(assess_id, all_of(temp_lf), year)
    
    df_z <- z %>%
      select(assess_id, all_of(temp_lf))
    
    assess_ids <- intersect(unique(df_y$assess_id), unique(df_z$assess_id))
    
    piList <- data.frame()
    for(j in 1:length(assess_ids)){
      
      assess_id_temp <- assess_ids[j]
      
      temp_outer <- subset(df_outer, assess_id == assess_id_temp)
      temp_inner <- subset(df_inner, assess_id == assess_id_temp)
      
      temp_y <- df_y %>%
        filter(assess_id == assess_id_temp)
      
      yrList <- data.frame()
      for(yr in 1:length(unique(temp_y$year))){
        
        year_temp <- sort(unique(temp_y$year))[yr]
        
        temp_y_yr <- temp_y %>%
          filter(year == year_temp) %>%
          dplyr::select(-year)
        
        #arrange the envelope data back into a list
        envelopePts <- list("EnvOuter"=data.frame("outX" = temp_outer$outX,"outY"=temp_outer$outY),
                            "EnvInner"=data.frame("inX" = temp_inner$inX,"inY" = temp_inner$inY))
        
        compDat <- data.frame(y1 = as.vector(unlist(temp_y_yr[,2])),
                              y2 = as.vector(unlist(temp_y_yr[,3])))
        
        #add labelling variables to PI results dataframe
        df_refPoints <- df_z %>%
          group_by(assess_id) %>%
          dplyr::summarise(refPoints = n())
        
        #command to skip envelope fitting for data with no reference envelope
        abort <- ifelse(nrow(envelopePts[[1]]) == 0 & nrow(envelopePts[[2]]) == 0, TRUE, FALSE)
        
        if(abort==FALSE){
          piPts <- PIcalc(compDat, envelopePts, 0.9)
          piPts <- do.call(cbind.data.frame, piPts)
          piPts$assess_id <- assess_id_temp
          piPts$refPoints <- df_refPoints$refPoints[1]
          piPts$year <- year_temp
          
          yrList <- rbind(yrList, piPts)
        }
      }
      
      piList <- rbind(piList, yrList)
    }
    
    piList$lf_pair <- paste(temp_lf, collapse="-")
    main_output <- rbind(main_output, piList)
    
  }
  
  main_output <- dplyr::rename(main_output, binomialProbability = 'binomial probability')
  return(main_output)
}


#function for plotting the PI envelope
plot_env <- function(x, y, z, lf, pi, label, threshold){
  
  #rounding function for labelling
  specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
  
  refs <- y %>%
    group_by(assess_id) %>%
    dplyr::summarise(refStart=min(year),
                     refStop=max(year)) %>%
    ungroup()
  
  comps <- z %>%
    group_by(assess_id) %>%
    dplyr::summarise(compStart=min(year),
                     compStop=max(year)) %>%
    ungroup()
  
    df_lookup_main <- data.frame(assess_id = pi$assess_id, lf_pair=pi$lf_pair) %>%
     left_join(refs, by = "assess_id") %>%
     left_join(comps, by = "assess_id") %>%
      dplyr::mutate(years_label = paste0("Ref: " , refStart, "-", refStop, ",", " Comp: ", compStart, "-", compStop))
  
    #create labeller lookup table
    df_lookup_main$string <- paste0('PI: ', specify_decimal(pi$PI, 2), ' ',
                                  df_lookup_main$years_label,'\n',
                                  'ref points: ', pi$refPoints, ',', ' ',
                                  'comp points: ', pi$newPoints,',', ' ',
                                  'binom-p: ', specify_decimal(pi$binomialProbability, 4), ',', ' ',
                                  'chi-sq: ', specify_decimal(pi$chi.sq, 1))
  
    main_outer <- x[[1]]
    main_inner <- x[[2]]
  
    main_outer_data_id <- main_outer
    main_inner_data_id <- main_inner
    
    plot_list <- list()
    for(i in 1:length(unique(main_outer_data_id$lf_pair))){
      
      temp_lf <- unlist(strsplit(sort(unique(main_outer_data_id$lf_pair))[i], "-"))
      
      df_outer <- subset(main_outer_data_id, lf_pair == paste(temp_lf, collapse="-"))
      df_inner <- subset(main_inner_data_id, lf_pair == paste(temp_lf, collapse="-"))
      
      names(df_outer)[1:2] <- c("x", "y")
      names(df_inner)[1:2] <- c("x", "y")
      
      df_outer$subid <- 1L
      df_inner$subid <- 2L
      df_polys <- rbind(df_outer, df_inner)
      
      temp_ref <- y %>% dplyr::select(1:4, all_of(temp_lf)) %>%
        arrange(assess_id, year, month)
      names(temp_ref)[c(ncol(temp_ref)-1,ncol(temp_ref))] <- c("vx", "vy")
      
      temp_comp <- z %>% dplyr::select(1:4, all_of(temp_lf)) %>%
        arrange(assess_id, year, month)
      names(temp_comp)[c(ncol(temp_comp)-1,ncol(temp_comp))] <- c("vx", "vy")
      
      ids <- intersect(unique(temp_ref$assess_id),unique(temp_comp$assess_id))
      
      #grouping factor for colouring months
      temp_comp$month <- as.numeric(temp_comp$month)
      temp_comp$season <- ifelse(temp_comp$month %in% c(1, 2, 12), "12 1 2",
                                 ifelse(temp_comp$month %in% c(3, 4, 5), "3 4 5",
                                        ifelse(temp_comp$month %in% c(6, 7, 8), "6 7 8",
                                               ifelse(temp_comp$month %in% c(9, 10, 11), "9 10 11", "ERROR"))))
      
      #Create a custom color scale
      factor_levels <- unique(temp_comp$season)
      myColors <- c("blue", "green", "yellow", "red")
      names(myColors) <- levels(factor_levels)
      
      #subset lookup table to panel of relevance
      df_lookup_temp <- unique(subset(df_lookup_main, lf_pair == paste(temp_lf, collapse="-")))
      
      #create title reference string
      years_label <- paste0("Ref: " , df_lookup_temp$refStart[1], "-", df_lookup_temp$refStop[1], ",", 
                            " Comp: ", df_lookup_temp$compStart[1], "-", df_lookup_temp$compStop[1])
      df_lookup_temp <- setNames(df_lookup_temp$string, df_lookup_temp$assess_id)
      
      sub_plot_list <- list()
      for(j in 1:length(ids)){
        
        poly <- ids[j]
        
        gg_panel <- ggplot() +
          geom_polygon(data=subset(df_polys, assess_id == poly), aes(x, y, group = assess_id, subgroup = subid), fill="grey60", colour="black", size=0.25, alpha=0.5) +
          geom_path(data=subset(temp_comp, assess_id == poly), aes(x=vx, y=vy), colour="grey", linetype = 2, size=0.25) +
          geom_point(data=subset(temp_comp, assess_id == poly),aes(x=vx, y=vy, fill=season), shape=21) +
          geom_point(data=subset(temp_ref, assess_id == poly),aes(x=vx, y=vy), shape=21, fill=NA, colour="grey60", alpha=0.5) +
          scale_x_continuous(name=bquote(log[10]* "(" * .(temp_lf[1]) * ")")) +
          scale_y_continuous(expand=c(0.1,0), name=bquote(log[10]* "(" * .(temp_lf[2]) * ")")) +
          scale_fill_manual(values=myColors)+
          facet_wrap(~ assess_id, scales="free", labeller = labeller(assess_id=df_lookup_temp)) +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5),
                legend.position = c(.5,.05),
                legend.spacing.x = unit(0.1, 'cm'),
                legend.direction = "horizontal",
                legend.background = element_blank(),
                legend.key = element_rect(fill = NA),
                legend.text = element_text(margin = margin(t = 2)))+
          guides(fill = guide_legend(title = "Month",
                                   title.position = "left")) 
        
        sub_plot_list[[poly]] <- gg_panel 
      }
      plot_list[[paste(temp_lf, collapse="-")]] <- sub_plot_list
    }
  return(plot_list)
}


#function for modelling change in lifeforms over time with Kendall test
kendallAll <- function(x, seasonal=FALSE){
  
  #generate relevant dataset within year limits
  trajData <- x
  
  if(seasonal == FALSE){
    
    #model annual change in abundance of each group in each spatial unit
    df_fits_tot <- trajData %>%
      dplyr::select(assess_id, year, month, num_samples, lifeform, abundance) %>%
      group_by(assess_id, year, lifeform) %>%
      dplyr::summarise(abundance_mean = mean(abundance, na.rm=T),
                       .groups="drop") %>%
      filter(!is.nan(abundance_mean)) %>%
      group_by(assess_id, lifeform) %>%
      dplyr::mutate(count = n()) %>%
      filter(count >= 3) %>%
      dplyr::mutate(year=as.integer(year)) %>%
      nest() %>%
      mutate(fits = map(data, ~EnvStats::kendallTrendTest(abundance_mean ~ year, ci.slope=FALSE, data=.x)),
             fits2 = map(fits, ~structure(.x, class="htest")),
             fits3 = map(fits2, ~tidy(.x))) %>%
      dplyr::select(-c(data, fits, fits2)) %>%
      unnest_wider(fits3)
    
  } else {
    
    #model annual change in abundance of each group in each spatial unit
    df_fits_tot <- trajData %>%
      dplyr::select(assess_id, year, month, num_samples, lifeform, abundance) %>%
      group_by(assess_id, month, year, lifeform) %>%
      dplyr::summarise(abundance_mean = mean(abundance, na.rm=T),
                       .groups="drop") %>%
      filter(!is.nan(abundance_mean)) %>%
      group_by(assess_id, lifeform) %>%
      dplyr::mutate(count = n()) %>%
      filter(count >= 3) %>%
      dplyr::mutate(month=as.integer(month),
                    year=as.integer(year)) %>%
      nest() %>%
      mutate(fits = map(data, ~EnvStats::kendallSeasonalTrendTest(abundance_mean ~ month + year, ci.slope=FALSE, data=.x)),
             fits2 = map(fits, ~structure(.x, class="htest")),
             fits3 = map(fits2, ~tidy(.x))) %>%
      dplyr::select(-c(data, fits, fits2)) %>%
      unnest_wider(fits3) %>%
      unnest_wider(statistic) %>%
      do(janitor::clean_names(.)) %>%
      dplyr::rename("statistic"=z_trend,
                    "p.value"=p_value) %>%
      unnest_wider(p.value) %>%
      do(janitor::clean_names(.)) %>%
      dplyr::rename("p.value"=z_trend) %>%
      dplyr::select(assess_id, lifeform, statistic, p.value)
    
  }
  
  #column to code whether p-value is significant
  df_fits_tot$sig <- ifelse(df_fits_tot$p.value <= 0.05, TRUE, FALSE)
  
  #simplify the dataframe output
  df_fits_tot <- data.frame(assess_id = df_fits_tot$assess_id,
                            lifeform = df_fits_tot$lifeform,
                            statistic = df_fits_tot$statistic,
                            p = df_fits_tot$p.value,
                            sig = df_fits_tot$sig) %>%
    arrange(as.integer(assess_id), lifeform)
  
  
  
  return(df_fits_tot)
}


#function to prepare the data to be plotted as time-series
create_ts <- function(x, y){
  
  #generate relevant dataset within year limits
  trajData <- x
  
  #match model results to new plotting dataframe
  df_plot <- merge(trajData, y, by=c("assess_id", "lifeform"))
  
  #convert year and month variables to date format
  df_plot$date_mon <- as.Date(paste(df_plot$year, df_plot$month, 16), "%Y %m %d")
  df_plot$date_year <- as.Date(paste(df_plot$year, 07, 02), "%Y %m %d")
  
  #tally number of samples for each facet
  df_plot <- df_plot %>%
    dplyr::mutate(year=as.integer(year)) %>%
    filter(!is.na(abundance)) %>%
    group_by(lifeform, assess_id) %>%
    dplyr::mutate(sumSamples = sum(num_samples, na.rm=T),
                  prop_years = length(unique(year) %in% seq(min(year), max(year), 1)) / length(seq(min(year), max(year), 1))) %>%
    ungroup() #ensure that threshold number of samples are included in each dataset

  return(df_plot)
}


#function for plotting time-series
plot_ts <- function(x){
  
  #create labeller lookup table
  df_lookup_main <- data.frame(assess_id = x$assess_id,
                               lifeform = x$lifeform) %>%
    dplyr::mutate(string = paste0('Kendall: ', round(x$statistic, 2), '   ', 
                                  'p: ', ifelse(x$p <= 0.05, "<=0.05", round(x$p,3)), '  ',
                                  'n: ', x$sumSamples)) %>%
    distinct()
  
  
  #generate a temp dataframe
  x_temp <- x
  
  plot_list <- list()
  for (i in 1:length(unique(x_temp$lifeform))){
    
    lf_temp <- sort(unique(x_temp$lifeform))[i]
    
    #subset to polygon of interest
    df_lookup_temp <- unique(subset(df_lookup_main, lifeform == lf_temp))
    
    #filter to polygon of interest and plot results 
    temp <- x_temp %>%
      filter(lifeform == lf_temp)
    
    sub_plot_list <- list()
    for(j in 1:length(unique(temp$assess_id))){
      
      assess_id_temp <- as.character(sort(unique(temp$assess_id)))[j]
      
      #subset to polygon of interest
      df_lookup_temp_id <- unique(subset(df_lookup_temp, assess_id == assess_id_temp))
      df_lookup_temp_id <- setNames(df_lookup_temp_id$string, df_lookup_temp_id$assess_id)
      
      temp_params <- temp %>%
        filter(assess_id == assess_id_temp) %>%
        dplyr::select(year, abundance)
      
      round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}
      
      years <- c(min(temp_params$year),max(temp_params$year))
      years_brk <- ifelse(round_any(years[2],5,f=ceiling)-round_any(years[1],5,f=floor) <= 20, "2 years", "5 years")
      
      years <- seq.Date(from = as.Date(paste(round_any(years[1],5,f=floor),"01","01",sep="-")), 
                        to = as.Date(paste(round_any(years[2],5,f=ceiling)+1,"01","01",sep="-")), 
                        by = years_brk)
      
      y_bks <- if(max(temp_params$abundance)>=3){
        seq(0,10,1)
      }else if(max(temp_params$abundance)<3 & max(temp_params$abundance)>1){
        seq(0,10,0.5)  
      }else{
        seq(0,10,0.2)
      }
      
      gg_panel <- temp %>%
        filter(assess_id == assess_id_temp) %>%
        group_by(year) %>%
        dplyr::mutate(abundance_annual = mean(abundance, na.rm=T)) %>%
        ungroup() %>%
        dplyr::mutate(interp=ifelse(num_samples==0,TRUE,FALSE)) %>%
        arrange(year, month) %>%
        ggplot(.,aes(date_mon, abundance, colour=interp))+
        geom_path(aes(group=1),size=0.25)+
        geom_smooth(aes(date_year, abundance_annual), formula= y ~ x,
                    linetype="dashed", colour="black", 
                    method = 'lm', se = FALSE) +
        geom_line(aes(date_year, abundance_annual), colour="blue", size=1)+
        geom_point(aes(date_year, abundance_annual), shape=21, fill="blue")+
        facet_wrap(~assess_id, ncol=1, scales="free_y", labeller = labeller(assess_id=df_lookup_temp_id))+
        scale_x_date(breaks = years, date_labels = "%Y",
                     minor_breaks = NULL)+
        scale_y_continuous(name=bquote(log[10]* "(" * .(lf_temp) * ")"), minor_breaks = NULL, breaks=y_bks)+
        scale_colour_manual(values=c("TRUE"="grey","FALSE"="blue"))+
        guides(shape="none")+
        theme_bw()+
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust=1, vjust=1),
              axis.title.x = element_blank(),
              legend.position = "none")
      
      sub_plot_list[[assess_id_temp]] <- gg_panel
    }
    plot_list[[lf_temp]] <- sub_plot_list
  }
return(plot_list)
}


#function to select, combine and save the combined plots
combine_pi_plots <- function(x, y, maps, limits, path){

    data_id_plot <- x
    data_id_lf_plot <- y
    
    for(i in 1:length(data_id_plot)){
      
      lf1 <- unlist(strsplit(names(data_id_plot)[i], "-"))[1]
      lf2 <- unlist(strsplit(names(data_id_plot)[i], "-"))[2]
      
      lf_pair_temp <- paste0(lf1,"-", lf2) 
      lf_pair_plot <- data_id_plot[[lf_pair_temp]]
      
      lf1_plot <- data_id_lf_plot[[lf1]]
      lf2_plot <- data_id_lf_plot[[lf2]]
      
      assess_ids <- intersect(names(lf1_plot), names(lf2_plot))
      assess_ids <- sort(as.integer(intersect(assess_ids, names(lf_pair_plot))))
      
      for(j in 1:length(assess_ids)){
        
        assess_id_temp <- as.character(assess_ids[j])
        
        #print an output to provide the user with a sense of progress
        print(paste0("lifeform pair: ", paste0(lf1,"-", lf2), ", ", "assess_id: ", assess_id_temp))
        
        panels <- list(lf_pair_plot[[assess_id_temp]], lf1_plot[[assess_id_temp]], lf2_plot[[assess_id_temp]])

        if(all(!is.na(maps))){
          temp_plot <- maps[[assess_id_temp]] + panels[[1]] + (panels[[2]] / panels[[3]]) +  plot_layout(widths = c(NA, 1, 2.5))
          width <- 45
        } else {
          temp_plot <- panels[[1]] + (panels[[2]] / panels[[3]]) +  plot_layout(widths = c(1, 2.5))
          width <- 40
        }
        
        #create the filename
        filename_temp <- paste0(path, assess_id_temp, "_", paste0(lf1,"-", lf2,".png"))
        
        ggsave(temp_plot, file=filename_temp,
               height=15, width=width, units="cm", bg="white")
      }
  }
}
