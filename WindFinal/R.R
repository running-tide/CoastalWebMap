library(sf)
data = st_read("Master_Windlease.geojson")
n = nrow(data)


for (i in 1:n) {
  #ID
  if(!is.na(data$prot_id[i])){  #block 1
    data$PROTRACT_1[i] = data$prot_id[i]
  }
  if(!is.na(data$PROTRACTIO[i])){  #block 2
    data$PROTRACT_1[i] = data$PROTRACTIO[i]
  }

  #leaseID
  if(!is.na(data$lease_id[i])){  #block 1
    data$LEASE_NU_1[i] = data$lease_id[i]
  }
  if(!is.na(data$BLOCK_NUMB[i])){  #block 2
    data$LEASE_NU_1[i] = data$BLOCK_NUMB[i]
  }
  
  #Name
  if(!is.na(data$lease_nm[i])){  #block 1
    data$COMPANY[i] = paste(data$lease_nm[i],data$aliquots[i])
  }
  if(!is.na(data$ADDITIONAL[i])){  #block 2
    data$COMPANY[i] = data$ADDITIONAL[i]
  }
  
  #URL
  if(!is.na(data$URL1[i])){  #block 1
    data$LEASE_DOCU[i] = data$URL1[i]
  }
  if(!is.na(data$URL2[i])){  #block 2
    data$LEASE_DO_1[i] = data$URL2[i]
  }
  
  #WP
  if(!is.na(data$PRIMARY_WP[i])){  #block 1
    data$RESOURCE[i] = data$PRIMARY_WP[i]
    }
    
  
    
}

sum(is.na(data$RESOURCE))
sum(is.na(data$LEASE_NUMB))


st_write(data, "CleanMaster_Windlease.geojson")



