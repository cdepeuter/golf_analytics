# # just dustin johnson
# flexmix(drive_dist_diff ~ net_wind + elevation_diff + rain_0_to_1_hrs_before + rain_1_to_2_hrs_before +rain_2_to_4_hrs_before +
#             rain_4_to_6_hrs_before + rain_6_to_8_hrs_before + rain_8_to_12_hrs_before +
#             rain_12_to_18_hrs_before + rain_18_to_24_hrs_before + rain_24_to_36_hrs_before + rain_36_to_48_hrs_before,  dj.drives, k=2)


# classes <-  flexmix(drive_dist_diff ~ net_wind + elevation_diff + rain_0_to_1_hrs_before + rain_1_to_2_hrs_before +rain_2_to_4_hrs_before +
#                         rain_4_to_6_hrs_before + rain_6_to_8_hrs_before + rain_8_to_12_hrs_before +
#                         rain_12_to_18_hrs_before + rain_18_to_24_hrs_before + rain_24_to_36_hrs_before + rain_36_to_48_hrs_before,  all.drives, k=2)
# 



## do collapse of rain cols
all.drives <- collapseRainCols(all.drives)
classes <-  flexmix(drive_dist_diff ~ net_wind + elevation_diff + rain_0_to_4_hrs_before + rain_4_to_12_hrs_before +rain_12_to_24_hrs_before +
                        rain_24_to_48_hrs_before ,  all.drives, k=2)


parameters(classes, component = 1)
parameters(classes, component = 2)

# get cluster assignments
flex_results <- clusters(classes)
post_probs <- posterior(classes)[,2]

all.drives$club_class <- flex_results
all.drives$club_prob <- post_probs



## investigate results
club_choice_by_player <- all.drives %>% group_by(long_hole, player) %>% summarise(club = mean(club_class == 2), obs = n())

# dustin johnson
club_choice_by_player[club_choice_by_player$player == 30925,]
# Bubba watson
club_choice_by_player[club_choice_by_player$player == 25804,]
#jim furyk
club_choice_by_player[club_choice_by_player$player == 10809,]
#henrik stenson
club_choice_by_player[club_choice_by_player$player == 21528,]
# mike weir
club_choice_by_player[club_choice_by_player$player == 10423,]



## interesting players


# load course hole information
course_hole_coords <- read.csv("data/pga-hole-coords.csv")


# 
# 
# regData <- all.drives[,c("drive_dist_diff", "elevation_diff", "net_wind", "rain_0_to_1_hrs_before")]
# dataRaw      <- mxData( observed=regData, type="raw" )
# # residual variances
# resVars      <- mxPath( from=c("elevation_diff", "net_wind", "rain_0_to_1_hrs_before", "rain_1_to_2_hrs_before", "rain_2_to_4_hrs_before", "rain_4_to_6_hrs_before"), arrows=2,
#                         free=TRUE, values=c(1,1,1,1,1,1),
#                         labels=c("e1","e2","e3","e4","e5","e6") ) 
# # latent variance
# latVar       <- mxPath( from="club", arrows=2,
#                         free=TRUE, values=1, labels ="varclub" )
# # factor loadings	
# facLoads     <- mxPath( from="F1", to=c("elevation_diff", "net_wind", "rain_0_to_1_hrs_before", "rain_1_to_2_hrs_before", "rain_2_to_4_hrs_before", "rain_4_to_6_hrs_before"), arrows=1,
#                         free=c(FALSE,TRUE,TRUE,TRUE,TRUE,TRUE), values=c(1,1,1,1,1,1),
#                         labels =c("l1","l2","l3","l4","l5","l6") )
# # means
# means        <- mxPath( from="drive_dist_diff", to=c("elevation_diff", "net_wind", "rain_0_to_1_hrs_before", "rain_1_to_2_hrs_before", "rain_2_to_4_hrs_before", "rain_4_to_6_hrs_before"), arrows=1,
#                         free=c(T,T,T,T,T,T,FALSE), values=c(1,1,1,1,1,1,0),
#                         labels =c("meanx1","meanx2","meanx3",
#                                   "meanx4","meanx5","meanx6",NA) ) 
# 
# oneFactorModel <- mxModel("Common Factor Model Path Specification", type="RAM",
#                           manifestVars=c("elevation_diff", "net_wind", "rain_0_to_1_hrs_before", "rain_1_to_2_hrs_before", "rain_2_to_4_hrs_before", "rain_4_to_6_hrs_before"), latentVars="club",
#                           dataRaw, resVars, latVar, facLoads, means)
# # Create an MxModel object
# # -----------------------------------------------------------------------------
# 
# oneFactorFit <- mxRun(oneFactorModel)      
# 
# summary(oneFactorFit)
# oneFactorFit$output$estimate
# 
# omxCheckCloseEnough(oneFactorFit$output$estimate[["l2"]], 0.999, 0.01)
# omxCheckCloseEnough(oneFactorFit$output$estimate[["l3"]], 0.959, 0.01)
# omxCheckCloseEnough(oneFactorFit$output$estimate[["l4"]], 1.028, 0.01)
# omxCheckCloseEnough(oneFactorFit$output$estimate[["l5"]], 1.008, 0.01)
# omxCheckCloseEnough(oneFactorFit$output$estimate[["l6"]], 1.021, 0.01)
# omxCheckCloseEnough(oneFactorFit$output$estimate[["varF1"]], 0.645, 0.01)
# omxCheckCloseEnough(oneFactorFit$output$estimate[["e1"]], 0.350, 0.01)
# omxCheckCloseEnough(oneFactorFit$output$estimate[["e2"]], 0.379, 0.01)
# omxCheckCloseEnough(oneFactorFit$output$estimate[["e3"]], 0.389, 0.01)
# omxCheckCloseEnough(oneFactorFit$output$estimate[["e4"]], 0.320, 0.01)
# omxCheckCloseEnough(oneFactorFit$output$estimate[["e5"]], 0.370, 0.01)
# omxCheckCloseEnough(oneFactorFit$output$estimate[["e6"]], 0.346, 0.01)
# omxCheckCloseEnough(oneFactorFit$output$estimate[["mean"]], 2.988, 0.01)
# omxCheckCloseEnough(oneFactorFit$output$estimate[["meanx2"]], 3.011, 0.01)
# omxCheckCloseEnough(oneFactorFit$output$estimate[["meanx3"]], 2.986, 0.01)
# omxCheckCloseEnough(oneFactorFit$output$estimate[["meanx4"]], 3.053, 0.01)
# omxCheckCloseEnough(oneFactorFit$output$estimate[["meanx5"]], 3.016, 0.01)
# omxCheckCloseEnough(oneFactorFit$output$estimate[["meanx6"]], 3.010, 0.01)
# # Compare OpenMx results to Mx results 