# Tateyama-OKC-Analyst-Technical-Assessment
Aric Tateyama's Technical Assessment for the OKC Thunder Analyst Internship position. 

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(readr)
library(dplyr)
```

# Data Science & Solutions Technical Assessment

```{r shots_data}
# Import shots_data.csv
shots_data <- read.csv("~/Documents/OKC Thunder/shots_data.csv")

# Calculate Shot Distance From Hoop
shots_data$dist <- sqrt((shots_data$x)^2 + (shots_data$y)^2)
dist <- shots_data$dist

# Differentiate Shot Zones (Corner 3, Not Corner 3, and 2 Pointer)
shots_data$zone <- with(shots_data, ifelse(x > 22.0 & y <= 7.8 | x < -22.0 & y <= 7.8, "C3", 
                      ifelse(y > 7.8 & dist > 23.75, "NC3", "2PT"))) 

# Filter Team A and Team B
teamA <- filter(shots_data, team == "Team A")
teamB <- filter(shots_data, team == "Team B")

# Percentage of Shots Attempted Within a Zone For Team A
PSA_A <- prop.table(table(teamA$zone))
paste("Team A's Shot Distribution Percentage for 2-Pointers is: ", round(PSA_A[1] * 100, 3), "%")
paste("Team A's Shot Distribution Percentage for Corner 3's is: ", round(PSA_A[2] * 100, 3), "%")
paste("Team A's Shot Distribution Percentage for Non-Corner 3's is: ", round(PSA_A[3] * 100, 3), "%")

# Percentage of Shots Attempted Within a Zone For Team B
PSA_B <- prop.table(table(teamB$zone))
paste("Team B's Shot Distribution Percentage for 2-Pointers is: ", round(PSA_B[1] * 100, 3), "%")
paste("Team B's Shot Distribution Percentage for Corner 3's is: ", round(PSA_B[2] * 100, 3), "%")
paste("Team B's Shot Distribution Percentage for Non-Corner 3's is: ", round(PSA_B[3] * 100, 3), "%")

# Function For Effective Field Goal Percentage
eFG <- function(FGM, M3PT, FGA)  {
  eFGP <- ((FGM) + (0.5 * M3PT)) / (FGA)
  return(eFGP)
}

# Effective Field Goal Percentage for Team A
A_2PTM <- nrow(filter(teamA, zone == "2PT", fgmade == 1)) # 2-Pointers Made
A_2PTA <- nrow(filter(teamA, zone == "2PT")) # 2-Pointers Attempted
paste("Team A's Effective FG% for 2-Pointers is: ", round(eFG(A_2PTM, 0, A_2PTA) * 100, 3), "%") # eFG% for 2-Pointers

A_C3M <- nrow(filter(teamA, zone == "C3", fgmade == 1)) # Corner 3's Made
A_C3A <- nrow(filter(teamA, zone == "C3")) # Corner 3's Attempted
paste("Team A's Effective FG% for Corner 3's is: ", round(eFG(A_C3M, A_C3M, A_C3A) * 100, 3), "%") # eFG% for Corner 3's

A_NC3M <- nrow(filter(teamA, zone == "NC3", fgmade == 1)) # Non-Corner 3's Made
A_NC3A <- nrow(filter(teamA, zone == "NC3")) # Non-Corner 3's Attempted
paste("Team A's Effective FG% for Non-Corner 3's is: ", round(eFG(A_NC3M, A_NC3M, A_NC3A) * 100, 3), "%") # eFG% for Non-Corner 3's

# Effective Field Goal Percentage for Team B
B_2PTM <- nrow(filter(teamB, zone == "2PT", fgmade == 1)) # 2-Pointers Made
B_2PTA <- nrow(filter(teamB, zone == "2PT")) # 2-Pointers Attempted
paste("Team B's Effective FG% for 2-Pointers is: ", round(eFG(B_2PTM, 0, B_2PTA) * 100, 3), "%") # eFG% for 2-Pointers

B_C3M <- nrow(filter(teamB, zone == "C3", fgmade == 1)) # Corner 3's Made
B_C3A <- nrow(filter(teamB, zone == "C3")) # Corner 3's Attempted
paste("Team B's Effective FG% for Corner 3's is: ", round(eFG(B_C3M, B_C3M, B_C3A) * 100, 3), "%") # eFG% for Corner 3's

B_NC3M <- nrow(filter(teamB, zone == "NC3", fgmade == 1)) # Non-Corner 3's Made
B_NC3A <- nrow(filter(teamB, zone == "NC3")) # Non-Corner 3's Attempted
paste("Team B's Effective FG% for Non-Corner 3's is: ", round(eFG(B_NC3M, B_NC3M, B_NC3A) * 100, 3), "%") # eFG% for Non-Corner 3's
```

# Solutions 

`Shot Distribution for Team A =>` **2 Pointers: 69.286%, Corner 3's: 6.429%, and Non-Corner 3's: 24.286%** 

`Shot Distribution for Team B =>` **2 Pointers: 67.411%, Corner 3's: 4.911%, and Non-Corner 3's: 27.679%**

`Effective Field Goal % for Team A =>` **2 Pointers: 48.969%, Corner 3's: 75.0%, and Non-Corner 3's: 46.324%**

`Effective Field Goal % for Team B` => **2 Pointers: 44.371%, Corner 3's: 54.545%, and Non-Corner 3's: 50.806%**
