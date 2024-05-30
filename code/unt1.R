eth1 <- cleandat$Q59.a
eth2 <- cleandat$Ethnicity
eth3 <- cleandata_factor$EthnicityCleaned
eth <- cbind(eth1, eth2, eth3)
View(eth)
write.csv(eth, "eth.csv")
