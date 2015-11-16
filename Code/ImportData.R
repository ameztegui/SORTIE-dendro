# Clean all the files and load the required libraries
rm(list=ls())
library(dplyr)
library(data.table)

sortie_dendro <- function (year) {

  # Import all the files that contain tree maps and cores
    map_names <- list.files("D:/Activity/Ecopyr/Chechu_EcoPyr/Data/TreeMaps", pattern="*.txt", full.names=TRUE)
    map_list<- list()
    
    core_names <- list.files("D:/Activity/Ecopyr/Chechu_EcoPyr/Data/Cores", pattern="*.txt", full.names=TRUE)
    core_list<- list()
    raw_core_list <- list()
    merge_list <- list()
    sortie_list <- list()
    

  # Loop that calculates diameter based on height and allometry  ####
  
    for (i in 1:length(map_names)) {
        
        # Read data and create the variables "Diam" and "Height", as defined in SORTIE
          map <- read.delim(map_names[i])
          map$Diam<- as.numeric(map$DBH..cm.)
          map$Height<- as.numeric(map$H..m.)
          
        # Read core data, transpose and set the new column names
          raw_core <- read.delim(core_names[i])
          trans_core<- as.data.frame(t(raw_core[,-1]))
          colnames(trans_core)<- raw_core[,1]
          trans_core$Core <- row.names(trans_core)
          trans_core$Cores <- substr(trans_core$Core,1,6)
        
          trans_core<- trans_core[c(ncol(trans_core):1)]
          core <- aggregate(x = trans_core,by=list(trans_core$Cores),FUN=mean, na.rm=T)
          core$Cores <-core$Group.1
          
        
        # Define subset pieces based on tree characteristics ####
          abal_se<-which(is.na(map$Diam) & 
                           map$Type=="Seedling" &
                           map$Species =="ABAL" )
          abal_ad<-which(is.na(map$Diam) & 
                           (map$Type=="Sapling"  | map$Type=="Adult") &
                           map$Species =="ABAL" )
          
          pisy_se<-which(is.na(map$Diam) & 
                           map$Type=="Seedling" &
                           map$Species =="PISY" )
          pisy_ad<-which(is.na(map$Diam) & 
                           (map$Type=="Sapling"  | map$Type=="Adult") &
                           map$Species =="PISY" )
          
          piun_se<-which(is.na(map$Diam) & 
                           map$Type=="Seedling" &
                           map$Species =="PIUN" )
          piun_ad<-which(is.na(map$Diam) & 
                           (map$Type=="Sapling"  | map$Type=="Adult") &
                           map$Species =="PIUN" )
          
          bepe_se<-which(is.na(map$Diam) & 
                           map$Type=="Seedling" &
                           map$Species =="BEPE" )
          bepe_ad<-which(is.na(map$Diam) & 
                           (map$Type=="Sapling"  | map$Type=="Adult") &
                           map$Species =="BEPE" )
          
          coav_se<-which(is.na(map$Diam) & 
                           map$Type=="Seedling" &
                           map$Species =="COAV" )
          coav_ad<-which(is.na(map$Diam) & 
                           (map$Type=="Sapling"  | map$Type=="Adult") &
                           map$Species =="COAV" )
          
          fasy_se<-which(is.na(map$Diam) & 
                           map$Type=="Seedling" &
                           map$Species =="FASY" )
          fasy_ad<-which(is.na(map$Diam) & 
                           (map$Type=="Sapling"  | map$Type=="Adult") &
                           map$Species =="FASY" )
        
            
        # Calculate new diameter values for those missing, based on known allometry (Ameztegui et al. 2015; Evans et al. 2015)
        
              # Seedlings
                map[abal_se,"Diam"]<- log((map$Height[abal_se]-0.1-30)/-30)/-0.013 
                map[pisy_se,"Diam"]<- log((map$Height[pisy_se]-0.1-30)/-30)/-0.014
                map[piun_se,"Diam"]<- log((map$Height[piun_se]-0.1-30)/-30)/-0.015 
                map[bepe_se,"Diam"]<- exp(log(map$Height[bepe_se])-log(2.58))/0.42
                map[coav_se,"Diam"]<- exp(log(map$Height[coav_se])-log(2.77))/0.46
                map[fasy_se,"Diam"]<- exp(log(map$Height[fasy_se])-log(0.2))/0.75
        
              # Saplings and Adults
                map[abal_ad,"Diam"]<- log((map$Height[abal_ad]-1.35-27.65)/-27.65)/-0.020 
                map[pisy_ad,"Diam"]<- log((map$Height[pisy_ad]-1.35-21.25)/-21.25)/-0.034 
                map[piun_ad,"Diam"]<- log((map$Height[piun_ad]-1.35-19.05)/-19.05)/-0.030 
                map[bepe_ad,"Diam"]<- log((map$Height[bepe_ad]-1.35-22.5)/-22.5)/-0.043
                map[coav_ad,"Diam"]<- log((map$Height[coav_ad]-1.35-19.5)/-19.5)/-0.118
                map[fasy_ad,"Diam"]<- log((map$Height[fasy_ad]-1.35-39)/-39)/-0.007    
    
        
        # Correct values lower than 0.1 cm (which is the min D10 in SORTIE)
              map$Diam[map$Diam <0.1 ] <- 0.1
              
        
        ### Prepare file to export as tree map
                # Select only those required columns, and those species and tree life history stages to include
                    map_cols <- select(map,X,Y, Species, Type, Diam, Height)
                    map_final <- filter(map_cols, Type=="Seedling" | Type=="Sapling" | Type=="Adult" | Type=="Snag",
                                      Species=="ABAL" | Species=="PISY" | Species=="PIUN" |
                                      Species=="BEPE"| Species == "COAV" | Species =="FASY")
                  
                # Delete all observations that include a NA
                    map_final <- map_final[!(is.na(map_final$Diam) | (is.na(map_final$Height))),]
        
                # Save the generated data frame into a new file 
                    # write.table(data_final,file=paste0("D:/Aitor/Chechu EcoPyr/Data/TreeMaps/Final/",name,".txt"), row.names=F, sep="\t")
    
        
        ### Calculate diameter at different years
                # Merge both tables, so that we can have together dbh and growth values
        
                ### NOTE: core and merge_core will not have necessarily the same length, since 
                ### some trees were cored outside the plot. Thus, we need to get the nrows of merge_core but the
                ### ncol (core)
                    merge_core <- merge(map, core, by="Cores")
        
                    temp_names <- vector("character")
                    temp_names[1:5]<- c("X", "Y","Species", "Type","Diam")
                        for( k in 4:ncol(core)) {
                          temp_names[k+2]<-paste0(names(core[k]))
                        }      
  
                    temp_diam <- data.frame (matrix(NA, nrow=nrow(merge_core), ncol=ncol(core)+2))
                    names(temp_diam)<- temp_names
        
                    temp_diam["X"] <- merge_core$X
                    temp_diam["Y"] <- merge_core$Y
                    temp_diam["Species"] <-(merge_core$Species)
                    temp_diam["Type"] <- (merge_core$Type)
                    temp_diam["Diam"] <- merge_core$Diam
                    #temp_diam[,6] <- temp_diam$Diam - (merge_core[,20]*2/1000)
                    for ( l in 6:ncol(temp_diam)) {
                        temp_diam[,l] <- temp_diam[,l-1] - (merge_core[,l+14]*2/1000)
                    }
     
            sortie_map<-temp_diam[,c("X","Y", "Species", "Type",year)]
            names(sortie_map)<-c("X","Y", "Species", "Type","Diam")
            sortie_map$Diam[sortie_map$Diam <0]<-NA
            sortie_map$Height <-rep(0, nrow(sortie_map))
            sortie_map<- sortie_map[complete.cases(sortie_map),]
        
        
        # Load the generated dataframe in a list (map_list), so that we can access later
            name <- paste('P',i,sep='')
            map_list[[name]]<- map_final
            core_list[[name]]<- core
            raw_core_list[[name]]<- trans_core
            merge_list[[name]] <- temp_diam
            sortie_list[[name]] <- sortie_map

        #return(temp_diam)
        
        # Save the generated data frame into a new file 
         write.table(sortie_map,file=paste0("D:/Activity/Ecopyr/Chechu_EcoPyr/Data/TreeMaps/Final/",name,"_",year,".txt"), row.names=F, sep="\t", quote=F)
        
            
    }
  
  
      all_list<- list(merge_list= merge_list, 
                      sortie_list=sortie_list, 
                      core_list=core_list, 
                      raw_core_list=raw_core_list)  
      return (all_list)
  
}
  
  
out<-sortie_dendro(2010)


View(out$merge_list$P9)
