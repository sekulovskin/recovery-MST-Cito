#============================
#Secondary education placement function
#===========================
secondary.ed <- function(x){
  results <- c()
  for(i in 1:length(x)){
    if(x[i] <= 500){
      results[i] <-  "pro/bb"  #ask about this
    }
    else if (x[i] >= 501 & x[i] <= 504){
      results[i] <-"pro/bb"
    }
    
    else if (x[i] >= 505 & x[i] <= 524){
      results[i] <-"bb/kb"
    }
    
    else if (x[i] >= 525 & x[i] <= 532){
      results[i] <-"kb/gt"
    }
    
    else if (x[i] >= 533 & x[i] <= 539){
      results[i] <-"gt/havo"
    }
    
    else if (x[i] >= 540 & x[i] <= 544){
      results[i] <-"havo/vwo"
    }
    
    else if (x[i] >= 545 & x[i] <= 550){
      results[i] <- "vwo"
    }
  }
  return(results)
}

#==================================
#Reference Score transformation functions
#================================

transform.ref.score.lezen <- function(x){
  results <- c()
  for(i in 1:length(x)){
    
    if(x[i] <= -2.539){
      results[i] <-  0
    }
    else if (x[i] >= -2.539 & x[i] <= -1.837){
      results[i] <- 1
    }
    
    else if (x[i] >= -1.837 & x[i] <= -1.546){
      results[i] <- 2
    }
    
    else if (x[i] >= -1.546 & x[i] <= -1.365){
      results[i] <- 3
    }
    
    else if (x[i] >= -1.365 & x[i] <= -1.235){
      results[i] <- 4
    }
    
    else if (x[i] >= -1.235 & x[i] <= -1.135){
      results[i] <- 5
    }
    
    else if (x[i] >= -1.135 & x[i] <= -1.053){
      results[i] <- 6 
      
    }
    else if (x[i] >= -1.053 & x[i] <= -0.984){
      results[i] <- 7
      
    }
    else if (x[i] >= -0.984 & x[i] <= -0.924){
      results[i] <- 8 
    }
    
    else if (x[i] >= -0.924 & x[i] <= -0.871){
      results[i] <- 9
    }
    
    else if (x[i] >= -0.871 & x[i] <= -0.823){
      results[i] <- 10
    }
    
    else if (x[i] >=  -0.823 & x[i] <= -0.780){
      results[i] <- 11
    }
    
    else if (x[i] >=  -0.780 & x[i] <= -0.741){
      results[i] <- 12
    }
    
    else if (x[i] >=  -0.741 & x[i] <= -0.705){
      results[i] <- 13
    }
    
    else if (x[i] >=  -0.705 & x[i] <= -0.671){
      results[i] <- 14
    }
    
    else if (x[i] >=  -0.671 & x[i] <= -0.609){
      results[i] <- 15
    }
    
    else if (x[i] >=  -0.609 & x[i] <= -0.581){
      results[i] <-16
    }
    
    else if (x[i] >=  -0.581 & x[i] <= -0.554){
      results[i] <- 17
    }
    
    else if (x[i] >=  -0.554 & x[i] <= -0.529){
      results[i] <- 18
    }
    
    else if (x[i] >=  -0.529 & x[i] <= -0.504){
      results[i] <- 19
    }
    
    else if (x[i] >=  -0.504 & x[i] <= -0.481){
      results[i] <- 20
    }
    
    else if (x[i] >=  -0.481 & x[i] <= -0.458){
      results[i] <- 21
    }
    
    else if (x[i] >=  -0.458 & x[i] <= -0.436){
      results[i] <- 22
    }
    
    else if (x[i] >=  -0.436 & x[i] <= -0.415){
      results[i] <- 23
    }
    
    else if (x[i] >=  -0.415 & x[i] <= -0.394){
      results[i] <- 24
    }
    
    else if (x[i] >=  -0.394 & x[i] <= -0.374){
      results[i] <- 25
    }
    
    else if (x[i] >=  -0.374 & x[i] <= -0.354){
      results[i] <- 26
    }
    
    else if (x[i] >=  -0.354 & x[i] <= -0.335){
      results[i] <- 27
    }
    
    else if (x[i] >=  -0.335 & x[i] <= -0.316){
      results[i] <- 28
    }
    
    else if (x[i] >=  -0.316 & x[i] <= -0.298){
      results[i] <- 29
    }
 
    else if (x[i] >=  -0.298 & x[i] <= -0.280){
      results[i] <- 30
    }
    
    else if (x[i] >=  -0.280 & x[i] <= -0.262){
      results[i] <- 31
    }
    
    else if (x[i] >=  -0.262 & x[i] <= -0.245){
      results[i] <- 32
    }
    
    else if (x[i] >=   -0.245 & x[i] <= -0.227){
      results[i] <- 33
    }
    
    else if (x[i] >=  -0.227 & x[i] <= -0.210){
      results[i] <- 34
    }
    
    else if (x[i] >=  -0.210 & x[i] <= -0.193){
      results[i] <- 35
    }
    
    else if (x[i] >=  -0.193 & x[i] <= -0.176){
      results[i] <- 36
    }
    
    else if (x[i] >=  -0.176 & x[i] <= -0.158){
      results[i] <- 37
    }
    
    else if (x[i] >=  -0.158 & x[i] <= -0.141){
      results[i] <- 38
    }
    
    else if (x[i] >=  -0.141 & x[i] <= -0.124){
      results[i] <- 39
    }
    
    else if (x[i] >=  -0.124 & x[i] <= -0.107){
      results[i] <- 40
    }
    
    else if (x[i] >=  -0.107 & x[i] <= -0.090){
      results[i] <- 41
    }
    
     else if (x[i] >=  -0.090 & x[i] <= -0.073){
       results[i] <- 42
     }
  
     else if (x[i] >=  -0.073 & x[i] <= -0.056){
       results[i] <- 43
     }
    
     else if (x[i] >=  -0.056 & x[i] <= -0.039){
       results[i] <- 44
     }
    
     else if (x[i] >=  -0.039 & x[i] <= -0.021){
       results[i] <- 45
     }
    
     else if (x[i] >=  -0.021 & x[i] <= -0.003){
       results[i] <- 46
     }
    
     else if (x[i] >=  -0.003 & x[i] <= 0.015){
       results[i] <- 47
     }
    
     else if (x[i] >=  0.015 & x[i] <= 0.033){
       results[i] <- 48
     }
    
     else if (x[i] >=  0.033 & x[i] <= 0.051){
       results[i] <- 49
     }
    
     else if (x[i] >=  0.051 & x[i] <= 0.070){
       results[i] <- 50
      }
    
     else if (x[i] >=  0.070 & x[i] <= 0.089){
       results[i] <- 51
      }
    
     else if (x[i] >=  0.089 & x[i] <=  0.108){
       results[i] <- 52
      }
    
     else if (x[i] >=   0.108 & x[i] <= 0.128){
       results[i] <- 53
    }
    
      else if (x[i] >=  0.128 & x[i] <= 0.148){
        results[i] <- 54
    }
    
     else if (x[i] >=  0.148 & x[i] <= 0.168){
       results[i] <- 55
    }
    
     else if (x[i] >=  0.168 & x[i] <= 0.189){
       results[i] <- 56
    }
  
     else if (x[i] >=  0.189 & x[i] <= 0.211){
       results[i] <- 57
    }
    
    else if (x[i] >=  0.211 & x[i] <= 0.233){
      results[i] <- 58
    }
    
    else if (x[i] >=  0.233 & x[i] <= 0.255){
      results[i] <- 59
    }
    
    else if (x[i] >=  0.255 & x[i] <= 0.279){
      results[i] <- 60
    }
    
    else if (x[i] >=  0.279 & x[i] <= 0.303){
      results[i] <- 61
    }
    
    
    else if (x[i] >=  0.303 & x[i] <= 0.328){
      results[i] <- 62
    }
    
    
    else if (x[i] >=  0.328 & x[i] <= 0.354){
      results[i] <- 63
    }
    
    
    else if (x[i] >=  0.354 & x[i] <= 0.381){
      results[i] <- 64
    }
    
    
    else if (x[i] >=  0.381 & x[i] <= 0.410){
      results[i] <- 65
    }
    
    
    else if (x[i] >=  0.410 & x[i] <= 0.440){
      results[i] <- 66
    }
    
    else if (x[i] >=  0.440 & x[i] <= 0.471){
      results[i] <- 67
    }
    
    
    else if (x[i] >=  0.471 & x[i] <= 0.504){
      results[i] <- 68
    }
    
    
    else if (x[i] >=  0.504 & x[i] <= 0.539){
      results[i] <- 69
    }
    
    
    else if (x[i] >=  0.539 & x[i] <= 0.577){
      results[i] <- 70
    }
    
    
    
    else if (x[i] >=  0.577 & x[i] <= 0.617){
      results[i] <- 71
    }
    
    
    else if (x[i] >=  0.617 & x[i] <= 0.661){
      results[i] <- 72
    }
    
    else if (x[i] >= 0.661  & x[i] <= 0.708){
      results[i] <- 73
    }
    
    else if (x[i] >=  0.708 & x[i] <= 0.760){
      results[i] <- 74
    }
    
    else if (x[i] >=  0.760 & x[i] <= 0.819){
      results[i] <- 75
    }
    
    
    else if (x[i] >=  0.819 & x[i] <= 0.884){
      results[i] <- 76
    }
    
    
    else if (x[i] >=  0.884 & x[i] <= 0.960){
      results[i] <- 77
    }
    
    
    else if (x[i] >=  0.960 & x[i] <= 1.048){
      results[i] <- 78
    }
    
    
    
    else if (x[i] >=  1.048 & x[i] <= 1.155){
      results[i] <- 79
    }
    
    
    
    else if (x[i] >=  1.155 & x[i] <= 1.289){
      results[i] <- 80
    }
    
    
    
    else if (x[i] >=  1.289 & x[i] <= 1.469){
      results[i] <- 81
    }
    
    
    
    else if (x[i] >=  1.469 & x[i] <= 1.735){
      results[i] <- 82
    }
    
    
    
    else if (x[i] >=  1.735 & x[i] <= 2.210){
      results[i] <- 83
    }
    
    
    
    else if (x[i] >=  2.210 & x[i] <= 3.486){
      results[i] <- 84
    }
    
    
    else if (x[i] >=  3.486){
      results[i] <- 85
    }
    
 }
  return(results)
}


#==========================

transform.ref.score.rekenen <- function(x){
  results <- c()
  for(i in 1:length(x)){
    
    if(x[i] <= -1.976){
      results[i] <-  0
    }
    else if (x[i] >= -1.976 & x[i] <= -1.434){
      results[i] <- 1
    }
    
    else if (x[i] >= -1.434 & x[i] <= -1.177){
      results[i] <- 2
    }
    
    else if (x[i] >= -1.177 & x[i] <= -1.006){
      results[i] <- 3
    }
    
    else if (x[i] >= -1.006 & x[i] <= -.877){
      results[i] <- 4
    }
    
    else if (x[i] >= -.877 & x[i] <= -.772){
      results[i] <- 5
    }
    
    else if (x[i] >= -.772 & x[i] <= -.684){
      results[i] <- 6 
      
    }
    else if (x[i] >= -.684 & x[i] <= -.606){
      results[i] <- 7
      
    }
    else if (x[i] >= -.606 & x[i] <= -.538){
      results[i] <- 8 
    }
    
    else if (x[i] >= -.538 & x[i] <= -.476){
      results[i] <- 9
    }
    
    else if (x[i] >= -.476 & x[i] <= -.418){
      results[i] <- 10
    }
    
    else if (x[i] >=  -.418 & x[i] <= -.365){
      results[i] <- 11
    }
    
    else if (x[i] >=  -.365 & x[i] <= -.314){
      results[i] <- 12
    }
    
    else if (x[i] >=  -.314 & x[i] <= -.266){
      results[i] <- 13
    }
    
    else if (x[i] >=  -.266 & x[i] <= -.220){
      results[i] <- 14
    }
    
    else if (x[i] >=  -.220 & x[i] <= -.176){
      results[i] <- 15
    }
    
    else if (x[i] >=  -.176 & x[i] <= -.133){
      results[i] <-16
    }
    
    else if (x[i] >=  -.133 & x[i] <= -.091){
      results[i] <- 17
    }
    
    else if (x[i] >=  -.091 & x[i] <= -.049){
      results[i] <- 18
    }
    
    else if (x[i] >=  -.049 & x[i] <= -.009){
      results[i] <- 19
    }
    
    else if (x[i] >=  -.009 & x[i] <= .032){
      results[i] <- 20
    }
    
    else if (x[i] >=  .032 & x[i] <= .072){
      results[i] <- 21
    }
    
    else if (x[i] >=  .072 & x[i] <= .113){
      results[i] <- 22
    }
    
    else if (x[i] >=  .113 & x[i] <= .153){
      results[i] <- 23
    }
    
    else if (x[i] >=  .153 & x[i] <= .194){
      results[i] <- 24
    }
    
    else if (x[i] >=  .194 & x[i] <= .236){
      results[i] <- 25
    }
    
    else if (x[i] >=  .236 & x[i] <= .279){
      results[i] <- 26
    }
    
    else if (x[i] >=  .279 & x[i] <= .323){
      results[i] <- 27
    }
    
    else if (x[i] >=  .323 & x[i] <= .369){
      results[i] <- 28
    }
    
    else if (x[i] >=  .369 & x[i] <= .417){
      results[i] <- 29
    }
    
    else if (x[i] >=  .417 & x[i] <= .468){
      results[i] <- 30
    }
    
    else if (x[i] >=  .468 & x[i] <= .523){
      results[i] <- 31
    }
    
    else if (x[i] >=  .523 & x[i] <= .581){
      results[i] <- 32
    }
    
    else if (x[i] >=   .581 & x[i] <= .645){
      results[i] <- 33
    }
    
    else if (x[i] >=  .645 & x[i] <= .717){
      results[i] <- 34
    }
    
    else if (x[i] >=  .717 & x[i] <= .798){
      results[i] <- 35
    }
    
    else if (x[i] >=  .798 & x[i] <= .893){
      results[i] <- 36
    }
    
    else if (x[i] >=  .893 & x[i] <= 1.009){
      results[i] <- 37
    }
    
    else if (x[i] >=  1.009 & x[i] <= 1.160){
      results[i] <- 38
    }
    
    else if (x[i] >=  1.160 & x[i] <= 1.385){
      results[i] <- 39
    }
    
    else if (x[i] >=  1.385 & x[i] <= 1.859){
      results[i] <- 40
    }
  }
  return(results)
}


#==========================

transform.ref.score.taal <- function(x){
  results <- c()
  for(i in 1:length(x)){
    
    if(x[i] <= -1.742){
      results[i] <-  0
    }
    else if (x[i] >= -1.742 & x[i] <= -1.347){
      results[i] <- 1
    }
    
    else if (x[i] >= -1.347 & x[i] <= -1.155){
      results[i] <- 2
    }
    
    else if (x[i] >= -1.155 & x[i] <= -1.028){
      results[i] <- 3
    }
    
    else if (x[i] >= -1.028 & x[i] <= -0.933){
      results[i] <- 4
    }
    
    else if (x[i] >= -0.933 & x[i] <= -0.857){
      results[i] <- 5
    }
    
    else if (x[i] >= -0.857 & x[i] <= -0.794){
      results[i] <- 6 
      
    }
    else if (x[i] >= -0.794 & x[i] <= -0.739){
      results[i] <- 7
      
    }
    else if (x[i] >= -0.739 & x[i] <= -0.691){
      results[i] <- 8 
    }
    
    else if (x[i] >= -0.691 & x[i] <= -0.648){
      results[i] <- 9
    }
    
    else if (x[i] >= -0.648 & x[i] <= -0.608){
      results[i] <- 10
    }
    
    else if (x[i] >=  -0.608 & x[i] <= -0.573){
      results[i] <- 11
    }
    
    else if (x[i] >=  -0.573 & x[i] <= -0.540){
      results[i] <- 12
    }
    
    else if (x[i] >=  -0.540 & x[i] <= -0.509){
      results[i] <- 13
    }
    
    else if (x[i] >=  -0.509 & x[i] <= -0.480){
      results[i] <- 14
    }
    
    else if (x[i] >=  -0.480 & x[i] <= -0.453){
      results[i] <- 15
    }
    
    else if (x[i] >=  -0.453 & x[i] <= -0.427){
      results[i] <-16
    }
    
    else if (x[i] >=  -0.427 & x[i] <= -0.403){
      results[i] <- 17
    }
    
    else if (x[i] >=  -0.403 & x[i] <= -0.379){
      results[i] <- 18
    }
    
    else if (x[i] >=  -0.379 & x[i] <= -0.357){
      results[i] <- 19
    }
    
    else if (x[i] >=  -0.357 & x[i] <= -0.335){
      results[i] <- 20
    }
    
    else if (x[i] >=  -0.335 & x[i] <= -0.314){
      results[i] <- 21
    }
    
    else if (x[i] >=  -0.314 & x[i] <= -0.294){
      results[i] <- 22
    }
    
    else if (x[i] >=  -0.294 & x[i] <= -0.274){
      results[i] <- 23
    }
    
    else if (x[i] >=  -0.274 & x[i] <= -0.255){
      results[i] <- 24
    }
    
    else if (x[i] >=  -0.255 & x[i] <= -0.236){
      results[i] <- 25
    }
    
    else if (x[i] >=  -0.236 & x[i] <= -0.218){
      results[i] <- 26
    }
    
    else if (x[i] >=  -0.218 & x[i] <= -0.199){
      results[i] <- 27
    }
    
    else if (x[i] >=  -0.199 & x[i] <= -0.181){
      results[i] <- 28
    }
    
    else if (x[i] >=  -0.181 & x[i] <= -0.164){
      results[i] <- 29
    }
    
    else if (x[i] >=  -0.164 & x[i] <= -0.146){
      results[i] <- 30
    }
    
    else if (x[i] >=  -0.146 & x[i] <= -0.129){
      results[i] <- 31
    }
    
    else if (x[i] >=  -0.129 & x[i] <= -0.111){
      results[i] <- 32
    }
    
    else if (x[i] >=   -0.111 & x[i] <= -0.094){
      results[i] <- 33
    }
    
    else if (x[i] >=  -0.094 & x[i] <= -0.077){
      results[i] <- 34
    }
    
    else if (x[i] >=  -0.077 & x[i] <= -0.059){
      results[i] <- 35
    }
    
    else if (x[i] >=  -0.059 & x[i] <= -0.042){
      results[i] <- 36
    }
    
    else if (x[i] >=  -0.042 & x[i] <= -0.025){
      results[i] <- 37
    }
    
    else if (x[i] >=  -0.025 & x[i] <= -0.007){
      results[i] <- 38
    }
    
    else if (x[i] >=  -0.007 & x[i] <= 0.011){
      results[i] <- 39
    }
    
    else if (x[i] >=  0.011 & x[i] <= 0.029){
      results[i] <- 40
    }
    
    else if (x[i] >=  0.029 & x[i] <= 0.047){
      results[i] <- 41
    }
    
    else if (x[i] >=  0.047 & x[i] <= 0.065){
      results[i] <- 42
    }
    
    else if (x[i] >=  0.065 & x[i] <= 0.084){
      results[i] <- 43
    }
    
    else if (x[i] >=  0.084 & x[i] <= 0.103){
      results[i] <- 44
    }
    
    else if (x[i] >=  0.103 & x[i] <= 0.123){
      results[i] <- 45
    }
    
    else if (x[i] >=  0.123 & x[i] <= 0.143){
      results[i] <- 46
    }
    
    else if (x[i] >=  0.143 & x[i] <= 0.164){
      results[i] <- 47
    }
    
    else if (x[i] >=  0.164 & x[i] <= 0.185){
      results[i] <- 48
    }
    
    else if (x[i] >=  0.185 & x[i] <= 0.207){
      results[i] <- 49
    }
    
    else if (x[i] >=  0.207 & x[i] <= 0.230){
      results[i] <- 50
    }
    
    else if (x[i] >=  0.230 & x[i] <= 0.253){
      results[i] <- 51
    }
    
    else if (x[i] >=  0.253 & x[i] <=  0.278){
      results[i] <- 52
    }
    
    else if (x[i] >=   0.278 & x[i] <= 0.304){
      results[i] <- 53
    }
    
    else if (x[i] >=  0.304 & x[i] <= 0.331){
      results[i] <- 54
    }
    
    else if (x[i] >=  0.331 & x[i] <= 0.359){
      results[i] <- 55
    }
    
    else if (x[i] >=  0.359 & x[i] <= 0.389){
      results[i] <- 56
    }
    
    else if (x[i] >=  0.389 & x[i] <= 0.422){
      results[i] <- 57
    }
    
    else if (x[i] >=  0.422 & x[i] <= 0.456){
      results[i] <- 58
    }
    
    else if (x[i] >=  0.456 & x[i] <= 0.493){
      results[i] <- 59
    }
    
    else if (x[i] >=  0.493 & x[i] <= 0.534){
      results[i] <- 60
    }
    
    else if (x[i] >=  0.534 & x[i] <= 0.578){
      results[i] <- 61
    }
    
    
    else if (x[i] >= 0.578 & x[i] <= 0.628){
      results[i] <- 62
    }
    
    
    else if (x[i] >=  0.628 & x[i] <= 0.684){
      results[i] <- 63
    }
    
    
    else if (x[i] >=  0.684 & x[i] <= 0.748){
      results[i] <- 64
    }
    
    
    else if (x[i] >=  0.748 & x[i] <= 0.824){
      results[i] <- 65
    }
    
    
    else if (x[i] >=  0.824 & x[i] <= 0.917){
      results[i] <- 66
    }
    
    else if (x[i] >=  0.917 & x[i] <= 1.036){
      results[i] <- 67
    }
    
    
    else if (x[i] >=  1.036 & x[i] <= 1.203){
      results[i] <- 68
    }
    
    
    else if (x[i] >=  1.203 & x[i] <= 1.477){
      results[i] <- 69
    }
    
    
    else if (x[i] >=  1.477 & x[i] <= 2.126){
      results[i] <- 70
    }
  }
  return(results)
}


#==========================

transform.ref.score.schrijven <- function(x){
  results <- c()
  for(i in 1:length(x)){
    
    if(x[i] <= -1.663){
      results[i] <-  0
    }
    else if (x[i] >= -1.663 & x[i] <= -1.189){
      results[i] <- 1
    }
    
    else if (x[i] >= -1.189 & x[i] <= -0.970){
      results[i] <- 2
    }
    
    else if (x[i] >= -0.970 & x[i] <= -0.833){
      results[i] <- 3
    }
    
    else if (x[i] >= -0.833 & x[i] <= -0.659){
      results[i] <- 4
    }
    
    else if (x[i] >= -0.659 & x[i] <= -0.597){
      results[i] <- 5
    }
    
    else if (x[i] >= -0.597 & x[i] <= -0.544){
      results[i] <- 6 
      
    }
    else if (x[i] >= -0.544 & x[i] <= -0.497){
      results[i] <- 7
      
    }
    else if (x[i] >= -0.497 & x[i] <= -0.456){
      results[i] <- 8 
    }
    
    else if (x[i] >= -0.456 & x[i] <= -0.419){
      results[i] <- 9
    }
    
    else if (x[i] >= -0.419 & x[i] <= -0.386){
      results[i] <- 10
    }
    
    else if (x[i] >= -0.386 & x[i] <= -0.354){
      results[i] <- 11
    }
    
    else if (x[i] >=  -0.354 & x[i] <= -0.325){
      results[i] <- 12
    }
    
    else if (x[i] >=  -0.325 & x[i] <= -0.298){
      results[i] <- 13
    }
    
    else if (x[i] >=  -0.298 & x[i] <= -0.272){
      results[i] <- 14
    }
    
    else if (x[i] >=  -0.272 & x[i] <= -0.247){
      results[i] <- 15
    }
    
    else if (x[i] >=  -0.247 & x[i] <= -0.223){
      results[i] <-16
    }
    
    else if (x[i] >=  -0.223 & x[i] <= -0.200){
      results[i] <- 17
    }
    
    else if (x[i] >=  -0.200 & x[i] <= -0.178){
      results[i] <- 18
    }
    
    else if (x[i] >=  -0.178 & x[i] <= -0.157){
      results[i] <- 19
    }
    
    else if (x[i] >=  -0.157 & x[i] <= -0.136){
      results[i] <- 20
    }
    
    else if (x[i] >=  -0.136 & x[i] <= -0.115){
      results[i] <- 21
    }
    
    else if (x[i] >=  -0.115 & x[i] <= -0.095){
      results[i] <- 22
    }
    
    else if (x[i] >=  -0.095 & x[i] <= -0.075){
      results[i] <- 23
    }
    
    else if (x[i] >=  -0.075 & x[i] <= -0.055){
      results[i] <- 24
    }
    
    else if (x[i] >=  -0.055 & x[i] <= -0.036){
      results[i] <- 25
    }
    
    else if (x[i] >=  -0.036 & x[i] <= -0.016){
      results[i] <- 26
    }
    
    else if (x[i] >=  -0.016 & x[i] <= 0.003){
      results[i] <- 27
    }
    
    else if (x[i] >=  0.003 & x[i] <= 0.022){
      results[i] <- 28
    }
    
    else if (x[i] >=  0.022 & x[i] <= 0.041){
      results[i] <- 29
    }
    
    else if (x[i] >=  0.041 & x[i] <= 0.061){
      results[i] <- 30
    }
    
    else if (x[i] >=  0.061 & x[i] <= 0.080){
      results[i] <- 31
    }
    
    else if (x[i] >=  0.080 & x[i] <= 0.100){
      results[i] <- 32
    }
    
    else if (x[i] >=   0.100 & x[i] <= 0.119){
      results[i] <- 33
    }
    
    else if (x[i] >=  0.119 & x[i] <= 0.139){
      results[i] <- 34
    }
    
    else if (x[i] >=  0.139 & x[i] <= 0.160){
      results[i] <- 35
    }
    
    else if (x[i] >=  0.160 & x[i] <= 0.180){
      results[i] <- 36
    }
    
    else if (x[i] >=  0.180 & x[i] <= 0.201){
      results[i] <- 37
    }
    
    else if (x[i] >=  0.201 & x[i] <= 0.223){
      results[i] <- 38
    }
    
    else if (x[i] >=  0.223 & x[i] <= 0.245){
      results[i] <- 39
    }
    
    else if (x[i] >=  0.245 & x[i] <= 0.268){
      results[i] <- 40
    }
    
    else if (x[i] >=  0.268 & x[i] <= 0.291){
      results[i] <- 41
    }
    
    else if (x[i] >=  0.291 & x[i] <= 0.316){
      results[i] <- 42
    }
    
    else if (x[i] >=  0.316 & x[i] <= 0.341){
      results[i] <- 43
    }
    
    else if (x[i] >=  0.341 & x[i] <= 0.367){
      results[i] <- 44
    }
    
    else if (x[i] >=  0.367 & x[i] <= 0.395){
      results[i] <- 45
    }
    
    else if (x[i] >=  0.395 & x[i] <= 0.424){
      results[i] <- 46
    }
    
    else if (x[i] >=  0.424 & x[i] <= 0.456){
      results[i] <- 47
    }
    
    else if (x[i] >=  0.456 & x[i] <= 0.489){
      results[i] <- 48
    }
    
    else if (x[i] >=  0.489 & x[i] <= 0.525){
      results[i] <- 49
    }
    
    else if (x[i] >=  0.525 & x[i] <= 0.564){
      results[i] <- 50
    }
    
    else if (x[i] >=  0.564 & x[i] <= 0.608){
      results[i] <- 51
    }
    
    else if (x[i] >=  0.608 & x[i] <=  0.656){
      results[i] <- 52
    }
    
    else if (x[i] >=   0.656 & x[i] <= 0.711){
      results[i] <- 53
    }
    
    else if (x[i] >=  0.711 & x[i] <= 0.775){
      results[i] <- 54
    }
    
    else if (x[i] >=  0.775 & x[i] <= 0.853){
      results[i] <- 55
    }
    
    else if (x[i] >=  0.853 & x[i] <= 0.950){
      results[i] <- 56
    }
    
    else if (x[i] >=  0.950 & x[i] <= 1.083){
      results[i] <- 57
    }
    
    else if (x[i] >=  1.083 & x[i] <= 1.290){
      results[i] <- 58
    }
    
    else if (x[i] >=  1.290 & x[i] <= 1.711){
      results[i] <- 59
    }
    
    else if (x[i] >=  1.711){
      results[i] <- 60
    }
    
  }
  return(results)
}