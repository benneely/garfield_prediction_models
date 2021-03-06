################
# Function needed below, but doesn't need review
################

checkandconvert <- function(x) {
  if (betaType[names(unlist(x))]=="continuous") {return (as.numeric(x))} 
  else if (betaType[names(unlist(x))]=="categorical") {
    if (names(unlist(x))=="country") {
      result = c("Australia"=0,"Austria"=0,"Belgium"=0,"Brazil"=0,"Canada"=0,"Chile"=0,"Denmark"=0,"Spain"=0,"Finland"=0,"UK"=0,"Hungary"=0,"Italy"=0,
                 "Korea"=0,"Netherlands"=0,"Norway"=0,"Thailand"=0,"USA"=0)
      indx = which(x==names(result))
      result[indx]=1
      return(result)
    } else {
      if (x=="Yes") { 
        result = eval(parse(text=paste0('c("',names(unlist(x)),'"=1)')))
        return(result)
      } else { 
        result = eval(parse(text=paste0('c("',names(unlist(x)),'"=0)')))
        return(result)
      }
    } 
  }
} 



###########################
# Start Review Here
# Metadata Received
###########################

referencePoints <- c("country"=0,"age"=60,"sbp"=120,"hxhf"=0,"ckd"=0,"sbp"=120,"ap"=0)
betaType <- c("country"="categorical","age"="continuous","sbp"="continuous","hxhf"="categorical","ckd"="categorical","ap"="categorical")
betas <- c("age"=0.03752473,"Australia"=-0.218473958,"Austria"=0.159933164,"Belgium"=-0.418820175,"Brazil"=-0.489144157,"Canada"=-1.005531926,"Chile"=0.041576453,
           "Denmark"=-0.260201289,"Spain"=-0.560461262,"Finland"=0.410690944,"UK"=-0.525918093,"Hungary"=-0.564631916,"Italy"=-0.58668433,"Korea"=-0.234378602,
           "Netherlands"=0.174333537,"Norway"=0.42991423,"Thailand"=-0.497083762,"USA"=0,"hxhf"=0.8810468,"ckd"=0.658942324,"sbp"=0.00623828,"ap"=0.517418301)
upper = c("age"=105,"sbp"=270,"dpb"=180)
lower = c("age"=18,"sbp"=40,"dbp"=26)
time <- c(0,3,4,5,6,9,10,11,12,13,15,18,20,21,23,24,30,31,32,34,35,36,39,40,46,49,51,52,54,56,57,59,63,65,69,70,71,72,74,75,78,80,84,85,88,89,90,93,98,99,101,102,104,108,
          109,110,111,113,114,115,119,120,122,124,125,126,131,137,138,148,152,154,157,158,159,161,162,163,164,166,168,171,175,182,184,187,188,192,193,196,198,205,206,212,
          214,216,219,222,223,228,229,231,232,235,236,244,246,247,250,258,260,261,262,268,271,272,273,276,280,282,283,284,285,291,295,296,298,301,303,305,307,313,314,317,
          320,323,330,334,335,338,342,352,353,355,356,357,359,360,361)
baseHaz <- c(1,0.999956967,0.999913915,0.999870835,0.999827747,0.99978462,0.99965514,0.999611955,0.999525582,0.999395932,0.999352678,0.999309374,0.99926605,0.99917939,
             0.999092583,0.999049162,0.998962165,0.998918657,0.998875143,0.998831617,0.998788065,0.998744491,0.99870087,0.99865721,0.998569806,0.998526073,0.998482325,
             0.998394832,0.998351065,0.998307297,0.998263501,0.998219678,0.998175827,0.998088067,0.998044174,0.997956345,0.997868499,0.997824543,0.997780558,0.99773657,
             0.997648516,0.997560406,0.997516296,0.997428042,0.997383838,0.997339622,0.997295377,0.997251098,0.997162297,0.997073466,0.99698457,0.996940099,0.996895594,
             0.996850999,0.996806358,0.996761684,0.996716975,0.996672216,0.996627398,0.996582559,0.996537539,0.99644739,0.996356901,0.996311357,0.996265713,0.99622004,
             0.996174241,0.996128343,0.99608242,0.996036377,0.99599031,0.995944212,0.99585197,0.995805832,0.995759688,0.995667348,0.995621171,0.995482586,0.995436376,
             0.995343877,0.995297613,0.995251304,0.995204946,0.99515854,0.995112116,0.995065681,0.995019244,0.994972782,0.99487986,0.99483336,0.994786846,0.994740173,
             0.994646747,0.994599723,0.994505472,0.994458172,0.994363259,0.994315669,0.994268021,0.994220044,0.994123875,0.994075544,0.993978703,0.993929955,0.993881137,
             0.993831233,0.993780456,0.993729109,0.99367771,0.993574539,0.993522899,0.993471247,0.993419568,0.993367791,0.993315988,0.993212372,0.993160524,0.99310856,
             0.993056507,0.993004438,0.99295237,0.992900278,0.992848173,0.992796009,0.992691628,0.992587202,0.992534977,0.99248272,0.992430438,0.992378156,0.992325818,
             0.992273399,0.992220962,0.992168495,0.992116026,0.992063545,0.991957917,0.991904797,0.991851599,0.991798146,0.991744251,0.991689318,0.991634134,0.991523409,
             0.99146796,0.991356793,0.991300902,0.991244751,0.991188261)

##############
# Pretend Patient
##############
input <- list(age=70,hxhf="Yes",ap="No",sbp=130,country="USA",ckd="Yes")


##############
# Convert UI input to a data set
##############
QQ <- vector("list",length(betaType))
for(i in 1:length(betaType)){
  QQ[[ i ]] <- eval(parse(text = paste("input$", names(betaType)[i], sep="")))
  names(QQ[[ i ]]) <- names(betaType)[i]
}
cnames <- names(unlist(QQ))
indx <- which(sapply(QQ, is.character))
for (z in 1:length(indx)){
  QQ[[indx[z]]] <- unlist(lapply(QQ[indx[z]], checkandconvert ))
}
a <- unlist(QQ) 
b <- data.frame(as.list(a),stringsAsFactors=F)
#c is the final data set that should be matrix multiplie against betas
c                 <- b
print('user input data.frame as input into UI: ')
print(c)
#######################################################################
#Subtract the reference points from the user input
#######################################################################
same              <- intersect(names(c),names(betas))
same2             <- intersect(names(c),names(referencePoints))

# this calculation is done in accordance with the PMML Specification
r                 <- as.matrix(c[same])                 %*% (as.matrix(betas[same]))
s                 <- t(as.matrix(referencePoints[same2]))  %*% (as.matrix(betas[same2]))
S_t               <- baseHaz*exp(r-s)
#S_t               <- exp(-H_t)
C_t               <- 1-S_t
#Notice that C_t is not prob scale, something is amiss
print(C_t)
