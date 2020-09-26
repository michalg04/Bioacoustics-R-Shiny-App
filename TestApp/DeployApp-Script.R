library(rsconnect)
rsconnect::setAccountInfo(name='mschroth', token='4F51B3CD98746361436ED6672F590CBE', secret='fBV7tag7rOWNBSmjG6XUJOQLRyw/LWsgI7Ic0W5u')



setwd("F:/Dropbox/FlashDrive-64GB/CalPoly-prof/Research/AnnBowles/TestApp/")
# setwd("C:/Users/hglanz/Dropbox/FlashDrive-64GB/CalPoly-prof/Research/AnnBowles/TestApp/")
deployApp(account = "mschroth")

