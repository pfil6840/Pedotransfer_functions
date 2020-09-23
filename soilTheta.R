## Function for estimation of bucket Size
## Bucket size is just a term for describing how much water a soil has when it is at field capacity.
## Field capacity is a soil science term to describe how much water is held in a soil when the potenital is potential water around -10 to -33 kPa
## This is when all readility drained water is removed 

## Inputs
# Inputs: (soil_data) - soil information; 
# This is a data frame where each row is a soil characteristic and each column is a depth interval. The first column is an id variable.
# Row names will indicate the soil characteristic of each row.
# Soil characteristics pertinent to the pedotransfer function are: Clay (clay), sand (sand) and bulk density (bd). It is
# possible other soil information will be in the table as well.
# Outputs of the associated ea_spline function would readily be used as soil information inputs for the soilTheta function.

#(DI) - depth intervals of soil information
# This is a vector that described explicitly the depth intervals that the soil data occurs

## Outputs
# a data frame with an estimate of the bucketsize for each depth interval and a total estimate (sum of all bucketsizes).

soilTheta<- function(soil_data){
  # Inputs: (soil_data) - soil information; (DI) - depth intervals of soil information
  
  # Saturated Theta
  theta_sat<- (0.1958 *tanh ((-0.0167* soil_data$clay - 0.0259 *soil_data$sand) + 0.5587 *soil_data$density
   + 1.86) - 0.4692 *tanh (((-0.0074* soil_data$clay - 0.0061* soil_data$sand) + 0.9869 *soil_data$density)
   - 1.47)) + 0.0063* tanh ((-0.0653 *soil_data$clay - 0.0063* soil_data$sand - 5.30 *soil_data$density)
   + 9.40) + 0.0495
  # Residual Theta
  theta_resid<- (0.3697 *tanh (-0.0167* soil_data$clay - 0.0259 *soil_data$sand + 0.5587 *soil_data$density + 1.86) - 
                 0.2543 *tanh (-0.0074* soil_data$clay - 0.0061* soil_data$sand + 0.9869 *soil_data$density-1.47) - 
                 0.2099* tanh (-0.0653 *soil_data$clay - 0.0063* soil_data$sand - 5.30 *soil_data$density + 9.40) - 
                  0.2032)^2
  
  # Field Capacity Theta
  theta_fc = 0.4795 - 3.873 * 10^-5 * soil_data$sand ^2 - 6.701 * 10^-7 * soil_data$clay ^2 * soil_data$sand
  # 
  # theta_fc33 <- 0.003320110612731 - 0.3396 * tanh(0.5 * (-0.9705 - 0.8529 * soil_data$density -
  #   0.00827 * soil_data$clay + 0.01994 * soil_data$sand)) +
  #   0.1629 * tanh(0.5 * (3.71 - 3.19 * soil_data$density + 0.01205 * soil_data$clay + 0.01617 * soil_data$sand))
  # - 0.1272 * tanh(0.5 * (-3.94 - 0.5067 * soil_data$density + 0.02158 * soil_data$clay + 0.04978 * soil_data$sand))
  # 
  # Permanent Wilting Point Theta
  theta_pwp<- -0.1554 - 0.7221 * tanh(0.5 * (-0.9705 - 0.8529 * soil_data$density - 0.00827 *
  soil_data$clay + 0.01994 * soil_data$sand))  + 0.1325 * tanh(0.5 * (3.71 - 3.19 * soil_data$density 
  + 0.01205 * soil_data$clay + 0.01617 * soil_data$sand)) + 0.1720 * tanh(0.5 * (-3.94 - 0.5067 * soil_data$density
  + 0.02158 * soil_data$clay + 0.04978 * soil_data$sand))
  
  k_sat = exp(2.41 - 8.12 * tanh(0.5 * (-3.96 + 2.86 * theta_fc + 1.90 * soil_data$density)) - 3.67 *
                tanh(0.5 * (-14.40 + 20.90 * theta_fc + 3.68 * soil_data$density)))

  return(soil.out<-cbind(soil_data,theta_sat,theta_resid,theta_fc,theta_pwp,k_sat))
  
}

###### END


##
#save(splineOuts, file = "C:/rdev/aqua/rPackage/pkg/data/splineOuts.rda")
