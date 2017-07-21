# function script for grady lab database shinyR browser

# define the function to call the connection object
db.poolfun <- function(dbname,host,user,password,port){
  dbPool(dbConnect(dbDriver("PostgreSQL"),
                   dbname=dbname,
                   host=host,
                   user=user,
                   password=password,
                   port=port)
         )
}

# get patient ages at each colonoscopy
age = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  age = to_lt$year - from_lt$year
  ifelse(to_lt$mon < from_lt$mon | (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),age - 1, age)
}

# get a new query
queryselected <- function(dbtitle,
                          ageindex1,
                          ageindex2,
                          npolypsindex1,
                          npolypsindex2,
                          pathoptions,
                          queryoptionscheck,
                          queryoptions){

  cage <- c()
  for(i in 1:nrow(colonoscopies)){
    cage[i] <- as.numeric(age(patients[patients$patient_id==colonoscopies[i,]$patient_id,]$dob,
                              colonoscopies[i,]$encounter_date))
  }
  colonoscopies$age <- cage
  
  #=================
  # apply filters
  
  # index age range extrema
  min.index.age <- as.numeric(ageindex1)
  max.index.age <- as.numeric(ageindex2)
  
  c1pat <- colonoscopies[colonoscopies$index==TRUE
                  &  min.index.age <= colonoscopies$age
                  & colonoscopies$age <= max.index.age,]$patient_id
  c1 <- colonoscopies[colonoscopies$patient_id %in% c1pat,]
  
  # npolyps criteria
  min.indexpolyp <- as.numeric(npolypsindex1)
  max.indexpolyp <- as.numeric(npolypsindex2)
  
  c2pat <- c1[c1$index==TRUE 
              & min.indexpolyp <= c1$total_polyps_reported
              & max.indexpolyp >= c1$total_polyps_reported,]$patient_id
  c2 <- c1[c1$patient_id %in% c2pat,]
  
  # pathology index encounter filter
  pathselect <- pathoptions
  
  if(!pathselect=="no filter"){
    c3pat <- c2[c2$index==TRUE 
                & c2$pathology_encounter==pathselect,]$patient_id
    c3 <- c2[c2$patient_id %in% c3pat,]
  } else{
    c3 <- c2
  }
  
  qocheck <- queryoptionscheck
  
  # queryoptions check boxes for index only and polyp details
  if("1" %in% qocheck){
    c3 <- c3[c3$index==TRUE,]
  }
  if("2" %in% qocheck){
    psvar <- c()
    cpats <- c3$patient_id
    cdate <- c3$encounter_date
    
    for(i in 1:nrow(c3)){
      poli <- polyps[polyps$patient_id==cpats[i] 
                     & polyps$encounter_date==cdate[i],]
      psvar[i] <- paste(poli$polyp_size,collapse=";")
    }
    c3$polyp_sizes <- psvar
  }
  
  # query options == colonoscopies
  if(queryoptions=="colonoscopies"){
    if("7" %in% qocheck){
      patdob <- c()
      for(i in 1:nrow(c3)){
        patdob[i] <- as.Date(patients[patients$patient_id==c3$patient_id[i],]$dob,origin="1970-01-01")
      }
      c3$dob <- as.Date(patdob,origin="1970-01-01")
    }
    returntable <- c3
  }
  
  # query options == polyps
  if(queryoptions=="polyps"){
    
    if("6" %in% qocheck){
      patdage <- c()
      for(i in 1:nrow(polyps)){
        patdage[i] <- c3[which(paste0(c3$patient_id,"_",c3$encounter_date)==paste0(polyps[i,]$patient_id,"_",polyps[i,]$encounter_date)),]$age
      }
      polyps$age <- patdage
    }
    
    
    if("7" %in% qocheck){
      patdob <- c()
      for(i in 1:nrow(polyps)){
        patdob[i] <- as.Date(patients[patients$patient_id==polyps$patient_id[i],]$dob,origin="1970-01-01")
      }
      polyps$dob <- as.Date(patdob,origin="1970-01-01")
    }
    
    
    pol1 <- polyps[which(paste0(polyps$patient_id,"_",polyps$encounter_date) %in% 
                           paste0(c3$patient_id,"_",c3$encounter_date)),]
    returntable <- pol1
  }
  
  # query options == patients
  if(queryoptions=="patients"){
    p1 <- patients[patients$patient_id %in% c3$patient_id,]
    
    if("3" %in% qocheck){
      edvar <- c()
      ppats <- p1$patient_id
      for(i in 1:nrow(p1)){
        cpat <- c3[c3$patient_id==ppats[i],]
        edvar[i] <- paste(cpat$encounter_date,collapse=";")
      }
      
      p1$encounter_dates <- edvar
    }
    
    if("4" %in% qocheck){
      pathdvar <- c()
      ppats <- p1$patient_id
      for(i in 1:nrow(p1)){
        cpat <- c3[c3$patient_id==ppats[i],]
        pathdvar[i] <- paste(cpat$pathology_encounter,collapse=";")
      }
      
      p1$encounter_pathologies <- pathdvar
    }
    
    if("5" %in% qocheck){
      npoldvar <- c()
      ppats <- p1$patient_id
      for(i in 1:nrow(p1)){
        cpat <- c3[c3$patient_id==ppats[i],]
        npoldvar[i] <- paste(cpat$total_polyps_reported,collapse=";")
      }
      
      p1$encounter_total_polyps <- npoldvar
    }
    
    if("6" %in% qocheck){
      agedvar <- c()
      ppats <- p1$patient_id
      for(i in 1:nrow(p1)){
        cpat <- c3[c3$patient_id==ppats[i],]
        agedvar[i] <- paste(cpat$age,collapse=";")
      }
      
      p1$encounter_ages <- agedvar
    }
    
    returntable <- p1
  }
  
  return(returntable)
  
  }
