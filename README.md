# infection-and-ALS
#exposure 
#Ethnic background | Instance 0f <- metS(,c(2,30))
infection$ethnic <- ifelse(infection$Ethnic.background...Instance.0  == "white", 1,
                      ifelse(infection$Ethnic.background...Instance.0 == "British", 1,
                             ifelse(infection$Ethnic.background...Instance.0 == "Irish", 1,
                                    ifelse(infection$Ethnic.background...Instance.0 == "Any other white background", 1,
                                           ifelse(infection$Ethnic.background...Instance.0 == "Prefer not to answer", NA,
                                                  ifelse(infection$Ethnic.background...Instance.0== "NA", NA,
                                                         ifelse(infection$Ethnic.background...Instance.0 == "Do not know", NA, 0)))))))
                                                         
#sleep
infection$sleep <-  ifelse(infection$sleep_duration <6, 0,
                            ifelse(infection$sleep_duration <=8, 1,
                                   ifelse(infection$sleep_duration >8, 2,NA)))

#IB number Binary categorical
infection$IBnum <- apply(infection[, 2:110], 1, function(row) sum(row %in% c(1, 2)))
infection$IBtwo <- ifelse(infection$IBnum>0,1,0)
infection$IBtwo <-as.factor(infection$IBtwo)
#IBhospital number Binary categorical
infection$IBhospitalnum <- apply(infection[, 2:110], 1, function(row) sum(row %in% c(1)))
infection$IBnonhospitalnum <- apply(infection[, 2:110], 1, function(row) sum(row %in% c(2)))

infection$IBhospitaltwo <- ifelse(infection$IBhospitalnum>0,1,0)
infection$IBhospitaltwo <-as.factor(infection$IBhospitaltwo)

#IB Tripartite variable
infection$IBthree <- ifelse(infection$IBhospitaltwo == 1, 2,
                            ifelse(infection$IBnonhospitalnum > 0, 1, 0))
infection$IBthree  <-as.factor(infection$IBthree )

#infection  Viral 
infection$viralnum <- apply(infection[, c(8,28,29,30:54,57,64,68,69,70,72,74,78,79,95:110)], 1, function(row) sum(row %in% c(1, 2)))
infection$viraltwo <- ifelse(infection$viralnum>0,1,0)
infection$viraltwo  <-as.factor(infection$viraltwo )

#infection  bacterial
infection$bacterialnum <- apply(infection[, c(2:7,9:27,55,56,58,59,65,75:77,81:87,89,93,94)], 1, function(row) sum(row %in% c(1, 2)))
infection$bacterialtwo <- ifelse(infection$bacterialnum>0,1,0)
infection$bacterialtwo  <-as.factor(infection$bacterialtwo )

#infection cite CNS
infection$CNSnum <- apply(infection[, c(11,28:34,58:62,95:97)], 1, function(row) sum(row %in% c(1, 2)))
infection$CNStwo <- ifelse(infection$CNSnum>0,1,0)
infection$CNStwo <-as.factor(infection$CNStwo)

#infection cite gastrointestinal
infection$gastnum <- apply(infection[, c(2:8)], 1, function(row) sum(row %in% c(1, 2)))
infection$gasttwo <- ifelse(infection$gastnum>0,1,0)
infection$gasttwo <-as.factor(infection$gasttwo)

#infection cite liver
infection$livernum <- apply(infection[, c(44:48)], 1, function(row) sum(row %in% c(1, 2)))
infection$livertwo <- ifelse(infection$livernum>0,1,0)
infection$livertwo <-as.factor(infection$livertwo)

#infection cite respirtory
infection$respirnum <- apply(infection[, c(9,10,16,17,18,51,64:80,111:113)], 1, function(row) sum(row %in% c(1, 2)))
infection$respirtwo <- ifelse(infection$respirnum>0,1,0)
infection$respirtwo <-as.factor(infection$respirtwo)

#infection cite sepsis
infection$sepsisnum <- apply(infection[, c(20,21)], 1, function(row) sum(row %in% c(1, 2)))
infection$sepsistwo <- ifelse(infection$sepsisnum>0,1,0)
infection$sepsistwo <-as.factor(infection$sepsistwo)

#infection cite skin
infection$skinnum <- apply(infection[, c(25,35:43,81:86)], 1, function(row) sum(row %in% c(1, 2)))
infection$skintwo <- ifelse(infection$skinnum>0,1,0)
infection$skintwo <-as.factor(infection$skintwo)

#infection cite urogenital infection
infection$ureanum <- apply(infection[, c(89:94)], 1, function(row) sum(row %in% c(1, 2)))
infection$ureatwo <- ifelse(infection$ureanum>0,1,0)
infection$ureatwo <-as.factor(infection$ureatwo)
