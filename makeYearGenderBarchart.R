
# Below is code from @meridithblevins used in qa-checks-r
# ## CREATE A BARPLOT GRAPHIC
# dob_year <- as.numeric(substr(basic$birth_d,1,4))
# ## for simplicity, remove birthdates that have any flag
# plotindex1 <- ifelse(exists("birth_d_a",basic),is.na(basic$birth_d_a)|basic$birth_d_a %in% c("D","M","Y"),TRUE)
# plotdob <- data.frame(table(dob_year[plotindex1],basic$gender[plotindex1]))
# 
# 
# png("output/year_gender_barchart.png",res=100,width=640,height=480, bg="transparent")
# par(mgp=c(2,1,0),mar=c(3,3,1,1))
# plotdob[,1] <- as.numeric(as.character(plotdob[,1]))
# plot(plotdob[,1],plotdob[,3],col=0,xlab="Year of birth",ylab="Number of patients",xlim=quantile(plotdob[,1],p=c(0.05,0.95)))
# rect(xleft=plotdob[plotdob$Var2==1,1]-.35,ybottom=0,xright=plotdob[plotdob$Var2==1,1],ytop=plotdob[plotdob$Var2==1,3],border=TRUE,col="blue",lwd=0.5)
# rect(xleft=plotdob[plotdob$Var2==2,1],ybottom=0,xright=plotdob[plotdob$Var2==2,1]+.35,ytop=plotdob[plotdob$Var2==2,3],border=TRUE,col="red",lwd=0.5)
# legend("topleft", c("Male", "Female"), pch=15,
#        col=c("blue","red"), border="black",
#        bty="n")
# dev.off()