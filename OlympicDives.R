# Olympic diving scores analysis

#####################################################################
############################### PART 1 ##############################
#####################################################################
setwd("Desktop/OlympicDives")
x <- read.csv("http://www.stat.yale.edu/~jay/diving/Diving2000.csv",
              as.is=TRUE)
dim(x)
names(x)

# Most of the variables are obvious. Country is the country of the diver,
# JCountry is the country of the judge. There are seven judges for each
# dive. Rank and DiveNo are used only within rounds of events and should be
# ignored (other than perhaps to observe who eventually won medals).

plot(jitter(JScore) ~ jitter(Difficulty), data=x,
     col=factor(x$Event),
     xlab="Degree of Difficulty",
     ylab="Judge's Scores")
legend("bottomleft", legend=levels(factor(x$Event)),
       pch=1, col=1:nlevels(factor(x$Event)))

# The legend is clunky but it's fine for basic explorations. 

# There is an odd left/right division in the plot above. There is a 
# lot of overplotting, which the jittering partly addresses. The 
# coloring is a bit of a mess, not clearly explaining the left/right 
# split. Let's dig into this more ...

tapply(x$Difficulty, x$Round, mean)

boxplot(x$Difficulty ~ x$Round, data=x, main="Differing Score by Round",     
        xlab="Round of Dive",
        ylab="Judge's Scores")

table(x$Round, x$Difficulty)

# The gap seems to be explained by semi dives on left, final & prelims
# on the right of the original scatterplot code provided. No semi dive
# was more than 2.1 difficulty. Now I'll look at some summary stats:

## What is the average score in the competition?
mean(x$JScore)
# 6.832576

## What is the average score for Chinese divers (a single average)?
mean(x$JScore [x$Country=="CHN"])
# 8.158986

## What is the average score for American divers (a single average)?
mean(x$JScore [x$Country=="USA"])
# 7.477191

## Average score for all countries
tapply(x$JScore, x$Country, mean)
# Confirms our previous averages are correct

## What about just the semi-final round average?
tapply(x$JScore, x$Round, mean)
# "Semi" mean = 7.793747

# The average level of difficulty is much lower in the semi round
# (~1.90) compared with the prelims (2.98) or the finals (3.06). 
# Easier dives might imply higher scores because of fewer mistakes, or
# lower scores in the prelims (weaker divers) while divers seeking medals
# try difficult dives in the Finals with more room for error.

# And here is one more graph given what we know now. Now we can clearly 
# see the differences by round. Interesting that the lower scores of 
# semi dives are relatively high compared to the more difficult dives
# performed in other rounds, but from a strategy standpoint this makes sense.

plot(jitter(JScore) ~ jitter(Difficulty), data=x,
     col=factor(x$Round),
     xlab="Degree of Difficulty",
     ylab="Judge's Scores")
legend("bottomleft", legend=levels(factor(x$Round)),
       pch=1, col=1:nlevels(factor(x$Round)))

#####################################################################
############################### PART 2 ##############################
#####################################################################

# Now let's see if we can't find some judge biases. 
# Will use same dataset as above so no need to re-read into R.
# I want to see if there are statistically significant differences
# in the scores a judge gives to divers from their own country
# compared with divers from other countries, so I calculate the 
# discrepancies. This exercise mirrors research conducted by Yale's
# Jay Emerson, who was also my professor for this class. [Highly 
# recommend.] His short report  will be in my GitHub repo, 
# labeled "MathHorizons.pdf."

x$match <- x$Country == x$JCountry

x$divenum <- rep(1:1541, each=7)
temp <- tapply(x$JScore, x$divenum, mean)
x$avg <- rep(temp, each=7)
x$discrepancy <- x$JScore - x$avg

# Next I choose a judge to explore. Mexico's Jesus Mena sounds like 
# a winner.

thisjudge <- "MENA Jesus"
y <- x[x$Judge==thisjudge,] 

# Now I want to create a graphic with two histograms comparing 
# the distributions of the discrepancies for the matching and 
# non-matching "groups". I've included vertical lines at the group 
# means for added clarity.

par(mfrow=c(2,1))

hist(y$discrepancy[y$match],
     xlim=c(-2.5,2.5), breaks=pretty,
     xlab = "Discrepency of Matches",
     main= thisjudge)
abline(v=mean(y$discrepancy[y$match]), lwd=3, col="green")

hist(y$discrepancy[!y$match], breaks=pretty,
     xlim=c(-2.5,2.5),
     main=thisjudge,
     xlab="Discrency of Non-Matches")
abline(v=mean(y$discrepancy[!y$match]), lwd=3, col="green")

# Visually interesting, but to be thorough I'll conduct a one-sided 
# 2-sample t-test for this judge's bias in favor of divers from the same 
# country [if one exists]. 
# The null hypothesis is one of no bias, obviously. We were asked to also provide
# a 99% confidence interval.

a <- t.test(y$discrepancy[y$match],y$discrepancy[!y$match], alternative = "greater")
b <- t.test(y$discrepancy[y$match],y$discrepancy[!y$match], conf.level = .99)

names(a)
print(a["estimate"]) 
0.24744898 - -0.05719462 # = 0.3046436

print(a["p.value"])

names(b)
print(b["conf.int"])

# a) estimated size of bias: 0.3046436
# b) one-sided test p-value: 0.0001313695
# c) 99% confidence interval: 0.1025203 0.5067669
# Discrepencies are significant!

# But before we get mad at Judge Jesus Mena, let's see if some of his fellow judges
# show any bias toward their respective compatriots. Here I also look at judges
# from Russia, Germany and the U.S.

# I originally made the resulting graphs a .pdf, but for the sake of the website I'll
# keep them as R plots so that they're visible online. Just uncomment the next
# line and the dev.off() at the end of the loop if you want your own .pdf. Example
# will be posted in my repo.

# pdf("OlympicDives_all.pdf")
for (thisjudge in c("ALT Walter", "ZAITSEV Oleg", "McFARLAND Steve")) {
  y <- x[x$Judge==thisjudge,]
  par(mfrow=c(2,1))
  
  hist(y$discrepancy[y$match],
       xlim=c(-2.5,2.5), breaks=pretty,
       xlab="Discrepency of Matches",
       main=thisjudge)
  abline(v=mean(y$discrepancy[y$match]), lwd=3, col="green")
  
  hist(y$discrepancy[!y$match], breaks=pretty,
       xlim=c(-2.5,2.5),
       main=thisjudge,
       xlab="Discrepency of Non-Matches")
  abline(v=mean(y$discrepancy[!y$match]), lwd=3, col="green")  
  
}
#dev.off()

# Visually we see these judges follow the same pattern: countrymen get a bump
# in score compared with non-countrymen. The statistics bear this out as well.
# But it is important to keep in mind: this may not be intentional bias; perhaps
# each judge above has implicit desires for their fellow citizens to do well.
# Regardless, we now have data that supports a nationalistic bias in Olympic
# diving judges, at least at the 2000 games.