## 2.1
fdiscunif_sample<-function(n=4){
    ceiling(n*runif(1))
}

## 2.2
frobot_path<-function(m=4){
  path_length<-0
  robot_pos<-c(1,1)
  while(!((robot_pos[1]==m) && (robot_pos[2]==m))){
    if ((robot_pos[1]==1) && (robot_pos[2]==1)){
      direction<-fdiscunif_sample(2)
      if (direction==1){
        robot_pos<-c(1,2)
      }else{
        robot_pos<-c(2,1)
      }
    }else if((robot_pos[1]==1) && (robot_pos[2]==m)){
      direction<-fdiscunif_sample(2);if (direction==1){robot_pos<-c(2,m)}else{robot_pos<-c(1,m-1)}
    }else if ((robot_pos[1]==m) && (robot_pos[2]==1)){
      direction<-fdiscunif_sample(2);if (direction==1){robot_pos<-c(m,2)}else{robot_pos<-c(m-1,1)}
    }else if ((robot_pos[1]==m) && (robot_pos[2]==m)){print("Warning: we have reached a place in the loop that should not ever be reached!")
    }else if ((robot_pos[1]==1) && (robot_pos[2]!=m)&&(robot_pos[2]!=1)){
      direction<-fdiscunif_sample(3);if (direction==1){robot_pos<-c(1,robot_pos[2]+1)};if (direction==2){robot_pos<-c(2,robot_pos[2])};if (direction==3){robot_pos<-c(1,robot_pos[2]-1)}
    }else if ((robot_pos[1]==m) && (robot_pos[2]!=m)&&(robot_pos[2]!=1)){
      direction<-fdiscunif_sample(3);if (direction==1){robot_pos<-c(m,robot_pos[2]+1)};if (direction==2){robot_pos<-c(m-1,robot_pos[2])};if (direction==3){robot_pos<-c(m,robot_pos[2]-1)}
    }else if ((robot_pos[1]!=1)&&(robot_pos[1]!=m) && (robot_pos[2]==1)){
      direction<-fdiscunif_sample(3);if (direction==1){robot_pos<-c(robot_pos[1]+1,1)};if (direction==2){robot_pos<-c(robot_pos[1],2)};if (direction==3){robot_pos<-c(robot_pos[1]-1,1)}
    }else if ((robot_pos[1]!=1)&&(robot_pos[1]!=m) && (robot_pos[2]==m)){
      direction<-fdiscunif_sample(3);if (direction==1){robot_pos<-c(robot_pos[1]+1,m)};if (direction==2){robot_pos<-c(robot_pos[1],m-1)};if (direction==3){robot_pos<-c(robot_pos[1]-1,m)}
    }else if ((robot_pos[1]!=1)&&(robot_pos[1]!=m) && (robot_pos[2]!=m) && (robot_pos[2]!=1)){
      direction<-fdiscunif_sample(4);if (direction==1){robot_pos<-c(robot_pos[1]+1,robot_pos[2])};if (direction==2){robot_pos<-c(robot_pos[1]-1,robot_pos[2])};if (direction==3){robot_pos<-c(robot_pos[1],robot_pos[2]+1)};if (direction==4){robot_pos<-c(robot_pos[1],robot_pos[2]-1)}
    }
    path_length<-path_length+1
  }
  path_length
}

N<-1000

vTm4<-sapply(1:N,function(i,m){frobot_path(m)},m=4,simplify=TRUE)
vTm8<-sapply(1:N,function(i,m){frobot_path(m)},m=8,simplify=TRUE)

pT4<-sum(vTm4==4*4)/N
pT8<-sum(vTm8==4*8)/N

## Q 2.3
library(boot)
get_boot_mean<-function(x,vi){mean(x[vi],na.rm=TRUE)}

ET4<-mean(vTm4)
ET8<-mean(vTm8)
ciET4<-boot.ci(boot(vTm4,get_boot_mean,R=1000))
ciET8<-boot.ci(boot(vTm8,get_boot_mean,R=1000))

