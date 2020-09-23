WB.Allbuckets <- function(Theta){
  profbuckets <- data.frame()
  bucket1 <- Theta$theta_fc[1]*50 
  bucket2 <- Theta$theta_fc[2]*100
  bucket3 <- Theta$theta_fc[3]*150
  bucket4 <- Theta$theta_fc[4]*300
  bucket5 <- Theta$theta_fc[5]*400
  rootzone_bucket <- sum(bucket1,bucket2,bucket3,bucket4,bucket5)
  profbuckets <- data.frame(bucket1,bucket2,bucket3,bucket4,bucket5,rootzone_bucket)
  profbuckets
}

WB.PWP <- function(Theta){
  profbuckets <- data.frame()
  bucket1 <- Theta$theta_pwp[1]*50 
  bucket2 <- Theta$theta_pwp[2]*100
  bucket3 <- Theta$theta_pwp[3]*150
  bucket4 <- Theta$theta_pwp[4]*300
  bucket5 <- Theta$theta_pwp[5]*400
  rootzone_bucket <- sum(bucket1,bucket2,bucket3,bucket4,bucket5)
  profbuckets <- data.frame(bucket1,bucket2,bucket3,bucket4,bucket5,rootzone_bucket)
  profbuckets
}

WB.RES <- function(theta){
  profbuckets <- data.frame()
  bucket1 <- Theta$theta_resid[1]*50 
  bucket2 <- Theta$theta_resid[2]*100
  bucket3 <- Theta$theta_resid[3]*150
  bucket4 <- Theta$theta_resid[4]*300
  bucket5 <- Theta$theta_resid[5]*400
  rootzone_bucket <- sum(bucket1,bucket2,bucket3,bucket4,bucket5)
  profbuckets <- data.frame(bucket1,bucket2,bucket3,bucket4,bucket5,rootzone_bucket)
  profbuckets
}