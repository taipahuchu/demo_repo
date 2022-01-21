install.packages("survival")
library(survival)

setwd ("C:/The Way/Work/IFoA Examiner/Exam Marking/2021 - April/3. Exam Papers and Marking Schedules/CS2B")

mortalitydata = read.csv(file="CS2B_Apr_21_Qu_3_Data.csv",head=TRUE)

#part (i)
KMfit = survfit(Surv(mortalitydata$Time, mortalitydata$Status)~1, conf.int = 0.995)
plot(KMfit, xlab = "Time", ylab = "Survival Function, S(t)", main = "Kaplan-Meier Estimate, with its two-sided 99.5% confidence interval, for all patients")

#part (ii)
summary(KMfit, time = 365)$surv

#OR

KMfit$surv[365]

#OR

min(KMfit$surv)

#part (iv)
KMfit = survfit(Surv(mortalitydata$Time, mortalitydata$Status)~Drug+Gender, data = mortalitydata)

#OR
KMfit = survfit(Surv(mortalitydata$Time, mortalitydata$Status)~mortalitydata$Drug+mortalitydata$Gender)

print(KMfit)

plot(KMfit, xlab = "Time", ylab = "Survival Function, S(t)", main = "Kaplan-Meier Estimate for the four possible patient groups", col = c("blue", "red", "black", "green"))
legend("topright", legend = c("Male - No Drug", "Female - No Drug", "Male - Drug", "Female - Drug"), col = c("blue", "red", "black", "green"), pch = 7)

#part (vi)
coxph(Surv(mortalitydata$Time, mortalitydata$Status)~Drug+Gender, data = mortalitydata, ties = "breslow")

#OR
coxph(Surv(mortalitydata$Time, mortalitydata$Status)~mortalitydata$Drug+mortalitydata$Gender, ties = "breslow")

#part (viii)
coxph(Surv(mortalitydata$Time, mortalitydata$Status)~Drug*Gender, data = mortalitydata, ties = "breslow")

#OR
coxph(Surv(mortalitydata$Time, mortalitydata$Status)~mortalitydata$Drug*mortalitydata$Gender, ties = "breslow")

#part (ix)
m1 = coxph(Surv(mortalitydata$Time, mortalitydata$Status)~Drug+Gender, data = mortalitydata, ties = "breslow")
m2 = coxph(Surv(mortalitydata$Time, mortalitydata$Status)~Drug*Gender, data = mortalitydata, ties = "breslow")

L1 = m1$loglik[2]
L2 = m2$loglik[2]

-2 *(L1 - L2)










