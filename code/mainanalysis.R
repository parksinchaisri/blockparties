# Code for "Community Vibrancy and Its Relationship
# Authors: Wichinpong Park Sinchaisri and Shane T Jensen
# Last updated: 20210915
# Main Analysis

# LOAD DATA
## BLOCK PARTIES
blockparties <- readRDS("../data/blockparties.Rds") # actual block parties
blockparties[, duration := event_duration/60] # in hours
curblocks <- readRDS("../data/curblocks.Rds") 
load("../data/phillyblockgroup") # Philly block group data
pbg <- data.table(fortify(phillyblockgroup))
blk <- merge(blockparties, curblocks[, .(block, street_address, lon, lat, blockid = blockid2, tractgroup = tractgroup2, censustract = censustract2, blockcount = blockcount2, blockgroup = blockgroup2, blockgroupID)], by = "block", all.x = T)
blk <- blk[!is.na(blockid),] # only keep blocks with block ID
blk[, mn := month(start_date)] # month of the start date

blocks <- readRDS("../data/blocks-2020.Rds") # each row is each block group
blocks <- blocks[total > 30,] # only keep neighborhoods with > 30 people
blocks[, n_trad := n_permits - n_spont] # number of traditional events

dt.permits <- readRDS("../data/dt-permits.Rds")
dt.bg <- readRDS("../data/dt-bg.Rds")
dt.bgd <- readRDS("../data/dt-bgd.Rds")
dt.bgmn <- readRDS("../data/dt-bgmn.Rds")
dt.bgyr <- readRDS("../data/dt-bgyr.Rds")

## CRIME DATA
crimelong <- readRDS("../data/final_crimelong.Rds")
pbg3 <- readRDS("../data/pbg3.Rds") # block-level census tract, block group, area
pbg3[, tractgroup := paste(censustract, blockgroup, sep="-")]
crimelong <- merge(crimelong, unique(pbg3[, .(tractgroup, id = as.character(blockid2))]), by = "tractgroup", all.x=T)

# Summary statistics
summary(blocks[, .(n_permits, spont, income, poverty_metric, total, black, prop_black, hispanic, prop_hispanic, logcrime = log(crime_total), area_com, area_res, vacant_proportion)])

# Generate figures for the paper
# Figure 1
# Yearly Trends
tmp <- blk[Year < 2016, .("n_permits" = length(unique(permit_number)), "n_types" = length(unique(event_type_description)), "nt" = length(unique(event_type)), "n_spont" = sum(spont), "first_date" = min(start_date), "last_date" = max(end_date)), by = .(Year)]
tmp[, spont := ifelse(n_permits > 0, n_spont/n_permits, 0)]

tmpb <- blk[Year < 2016, .("n_permits" = length(unique(permit_number)), "n_types" = length(unique(event_type_description)), "nt" = length(unique(event_type)), "n_spont" = sum(spont), "first_date" = min(start_date), "last_date" = max(end_date)), by = .(Year, blockgroupID)]
tmpb[, spont := ifelse(n_permits > 0, n_spont/n_permits, 0)]

savepdf("fig_yearly_npermits")
plotmeans(n_permits ~ Year, data = tmp, mean.labels=F, col="black", connect=TRUE, n.label=FALSE, main="Total Number of Events by Year", xlab="Year", ylab="Number of Events", pch=20, lwd=2, barwidth = 2, cex.lab = 1.5, cex.axis = 1.5, cex=1.5, cex.main = 1.75)
dev.off()

savepdf("fig_yearly_npermits_block")
plotmeans(n_permits ~ Year, data = tmpb, mean.labels=F, col="black", connect=TRUE, n.label=FALSE, main="Total Number of Events by Year", xlab="Year", ylab="Number of Events", pch=20, lwd=2, barwidth = 2, cex.lab = 1.5, cex.axis = 1.5, cex=1.5, cex.main = 1.75)
dev.off()

savepdf("fig_yearly_spont")
plotmeans(spont ~ Year, data = tmp, mean.labels=FALSE, col="black", connect=TRUE, n.label=FALSE, main="Spontaneous Proportion by Year", xlab="Year", ylab="Spontaneous Proportion", pch=20, lwd=2, barwidth = 2, cex.lab = 1.5, cex.axis = 1.5, cex=1.5, cex.main = 1.75)
dev.off()

savepdf("fig_yearly_spont_block")
plotmeans(spont ~ Year, data = tmpb, mean.labels=FALSE, col="black", connect=TRUE, n.label=FALSE, main="Spontaneous Proportion by Year", xlab="Year", ylab="Spontaneous Proportion", pch=20, lwd=2, barwidth = 2, cex.lab = 1.5, cex.axis = 1.5, cex=1.5, cex.main = 1.75)
dev.off()

# Crime trends
savepdf("fig_violent_block")
plotmeans(crime.violent ~ year, data = crimelong, mean.labels=F, col="black", connect=TRUE, n.label=FALSE, main="Violent Crimes by Year", xlab="Year", ylab="Number of Violent Crimes", pch=20, lwd=2, barwidth = 2, cex.lab = 1.5, cex.axis = 1.5, cex=1.5, cex.main = 1.75)
dev.off()

savepdf("fig_nonviolent_block")
plotmeans(crime.nonviolent ~ year, data = crimelong, mean.labels=F, col="black", connect=TRUE, n.label=FALSE, main="Non-violent Crimes by Year", xlab="Year", ylab="Number of Non-violent Crimes", pch=20, lwd=2, barwidth = 2, cex.lab = 1.5, cex.axis = 1.5, cex=1.5, cex.main = 1.75)
dev.off()

tmp <- crimelong[, .("crime.violent" = sum(crime.violent), "crime.nonviolent" = sum(crime.nonviolent)), by = .(year)]

savepdf("fig_violent")
plotmeans(crime.violent ~ year, data = tmp, mean.labels=F, col="black", connect=TRUE, n.label=FALSE, main="Violent Crimes by Year", xlab="Year", ylab="Number of Violent Crimes", pch=20, lwd=2, barwidth = 2, cex.lab = 1.5, cex.axis = 1.5, cex=1.5, cex.main = 1.75)
dev.off()

savepdf("fig_nonviolent")
plotmeans(crime.nonviolent ~ year, data = tmp, mean.labels=F, col="black", connect=TRUE, n.label=FALSE, main="Non-violent Crimes by Year", xlab="Year", ylab="Number of Non-violent Crimes", pch=20, lwd=2, barwidth = 2, cex.lab = 1.5, cex.axis = 1.5, cex=1.5, cex.main = 1.75)
dev.off()

# Figure 2
summary(blocks$n_permits) # median = 42.5
blocks[, vibrancy := ifelse(n_permits <= 42.5, "Low", "High")]

savepdf3("fig_box_income")
boxplot(income ~ vibrancy, data = blocks, outline=F, xlab="Community Measure", ylab="", main="Median Household Income", cex.lab = 2.25, cex.axis = 2.25, cex=2.25, cex.main = 2.5)
dev.off()

savepdf3("fig_box_poverty")
boxplot(poverty_metric ~ vibrancy, data = blocks, outline=F, xlab="Community Measure", ylab="", main="Poverty Metric", cex.lab = 2.25, cex.axis = 2.25, cex=2.25, cex.main = 2.5)
dev.off()

savepdf3("fig_box_black")
boxplot(prop_black ~ vibrancy, data = blocks, outline=F, xlab="Community Measure", ylab="", main="Proportion of Black Population", cex.lab = 2.25, cex.axis = 2.25, cex=2.25, cex.main = 2.5)
dev.off()

# CORRELATION PLOT
M <- cor(blocks[!is.na(income) & !is.na(poverty_metric), .(n_permits, spont, income, poverty_metric, total, prop_black, prop_hispanic, area, area_com, area_res, vacant_proportion, log_crime = log(crime_total), log_violent_crime = log(crime_violent))])
colnames(M) <- c("# total events", "spontaneous ratio", "income", "poverty", "total population", "black proportion", "hispanic proportion", "area", "commercial proportion", "residential proportion", "vacant proportion", "log(total crime)", "log(violent crime)")
rownames(M) <- c("# total events", "spontaneous ratio", "income", "poverty", "total population", "black proportion", "hispanic proportion", "area", "commercial proportion", "residential proportion", "vacant proportion", "log(total crime)", "log(violent crime)")
corrplot(M, type="upper", order="original", tl.col="black")


# MAIN ANALYSES


## TABLE S1
rego1 <- lm(log(crime_total) ~ n_permits + log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic, data = blocks)

rego2 <- lm(log(crime_total) ~ spont + log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic, data = blocks)

nbrego3 <- glm.nb(crime_total ~ n_permits + log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic, data = blocks)

nbrego4 <- glm.nb(crime_total ~ spont + log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic, data = blocks)

stargazer(rego1, rego2, nbrego3, nbrego4, single.row =T, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), type="text")
stargazer(rego1, rego2, nbrego3, nbrego4, single.row =T, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), type="latex")

rss1 <- c(crossprod(rego1$residuals)) # Residual sum of squares
mse1 <- rss1 / length(rego1$residuals) # Mean squared error
rmse1 <- sqrt(mse1) # Root MSE
sig1 <- rss1 / rego1$df.residual # Pearson estimated residual variance (as returned by summary.lm)

rss2 <- c(crossprod(rego2$residuals)) # Residual sum of squares
mse2 <- rss2 / length(rego2$residuals) # Mean squared error
rmse2 <- sqrt(mse2) # Root MSE
sig2 <- rss2 / rego2$df.residual # Pearson estimated residual variance (as returned by summary.lm)

rss3 <- c(crossprod(nbrego3$residuals)) # Residual sum of squares
mse3 <- rss3 / length(nbrego3$residuals) # Mean squared error
rmse3 <- sqrt(mse3) # Root MSE
sig3 <- rss3 / nbrego3$df.residual # Pearson estimated residual variance (as returned by summary.lm)

rss4 <- c(crossprod(nbrego4$residuals)) # Residual sum of squares
mse4 <- rss4 / length(nbrego4$residuals) # Mean squared error
rmse4 <- sqrt(mse4) # Root MSE
sig4 <- rss4 / nbrego4$df.residual # Pearson estimated residual variance (as returned by summary.lm)

print(paste("RMSE:", round_any(rmse1, 0.000001), round_any(rmse2, 0.000001), round_any(rmse3, 0.000001), round_any(rmse4, 0.000001)))

## TABLE S2
t2010 <- readRDS("../data/t2010.Rds")

# Re-do trends
t2010[, permits_sigpos := ifelse(slope > 0 & sigs == 1, 1, 0)]
t2010[, permits_signeg := ifelse(slope < 0 & sigs == 1, 1, 0)]
t2010[, crimes_sigpos := ifelse(cslope > 0 & csigs == 1, 1, 0)]
t2010[, crimes_signeg := ifelse(cslope < 0 & csigs == 1, 1, 0)]
t2010[, spont_sigpos := ifelse(sslope > 0 & ssigs == 1, 1, 0)]
t2010[, spont_signeg := ifelse(sslope < 0 & ssigs == 1, 1, 0)]

table(t2010$crimes_sigpos) # 1234, 38
table(t2010$crimes_signeg) # 683, 589
table(t2010$permits_sigpos) # 1178, 94
table(t2010$permits_signeg) # 1088, 184
table(t2010$spont_sigpos) # 797, 313
table(t2010$spont_signeg) # 1083 27

# Significant slopes
reg1 <- glm(permits_sigpos ~ log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic + crimes_sigpos + crimes_signeg, data = t2010)
reg2 <- glm(permits_signeg ~  log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic + crimes_sigpos + crimes_signeg, data = t2010)
reg3 <- glm(crimes_sigpos ~  log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic + permits_sigpos + permits_signeg, data = t2010)
reg4 <- glm(crimes_signeg ~  log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic + permits_sigpos + permits_signeg, data = t2010)

stargazer(reg1, reg2, reg3, reg4, single.row =T, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), type="text")
stargazer(reg1, reg2, reg3, reg4, single.row =T, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), type="latex")

# + or - slope
reg1 <- glm(I(slope > 0) ~ log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic + crimes_sigpos + crimes_signeg, data = t2010)
reg2 <- glm(I(slope < 0) ~  log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic + crimes_sigpos + crimes_signeg, data = t2010)
reg3 <- glm(I(cslope > 0) ~  log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic + permits_sigpos + permits_signeg, data = t2010)
reg4 <- glm(I(cslope < 0) ~  log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic + permits_sigpos + permits_signeg, data = t2010)

stargazer(reg1, reg2, reg3, reg4, single.row =T, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), type="text")
stargazer(reg1, reg2, reg3, reg4, single.row =T, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), type="latex")

# Spont
reg1 <- glm(spont_sigpos ~ log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic + crimes_sigpos + crimes_signeg, data = t2010)
reg2 <- glm(spont_signeg ~ log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic + crimes_sigpos + crimes_signeg, data = t2010)
reg3 <- glm(crimes_sigpos ~ log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic + spont_sigpos + spont_signeg, data = t2010)
reg4 <- glm(crimes_signeg ~ log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic + spont_sigpos + spont_signeg, data = t2010)

stargazer(reg1, reg2, reg3, reg4, single.row =T, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), type="text")
stargazer(reg1, reg2, reg3, reg4, single.row =T, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), type="latex")

# Tables S4 = Violent
reg41 <- lm(log(crime_violent) ~ n_permits + log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic, data = blocks)

reg42 <- lm(log(crime_violent) ~ spont + log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic, data = blocks)

nbreg43 <- glm.nb(crime_violent ~ n_permits + log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic, data = blocks)

nbreg44 <- glm.nb(crime_violent ~ spont + log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic, data = blocks)

stargazer(reg41, reg42, nbreg43, nbreg44, single.row =T, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), type="text")

stargazer(reg41, reg42, nbreg43, nbreg44, single.row =T, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), type="latex")

rss1 <- c(crossprod(reg41$residuals)) # Residual sum of squares
mse1 <- rss1 / length(reg41$residuals) # Mean squared error
rmse1 <- sqrt(mse1) # Root MSE
sig1 <- rss1 / reg41$df.residual # Pearson estimated residual variance (as returned by summary.lm)

rss2 <- c(crossprod(reg42$residuals)) # Residual sum of squares
mse2 <- rss2 / length(reg42$residuals) # Mean squared error
rmse2 <- sqrt(mse2) # Root MSE
sig2 <- rss2 / reg42$df.residual # Pearson estimated residual variance (as returned by summary.lm)

rss3 <- c(crossprod(nbreg43$residuals)) # Residual sum of squares
mse3 <- rss3 / length(nbreg43$residuals) # Mean squared error
rmse3 <- sqrt(mse3) # Root MSE
sig3 <- rss3 / nbreg43$df.residual # Pearson estimated residual variance (as returned by summary.lm)

rss4 <- c(crossprod(nbreg44$residuals)) # Residual sum of squares
mse4 <- rss4 / length(nbreg44$residuals) # Mean squared error
rmse4 <- sqrt(mse4) # Root MSE
sig4 <- rss4 / nbreg44$df.residual # Pearson estimated residual variance (as returned by summary.lm)

print(paste("RMSE:", round_any(rmse1, 0.000001), round_any(rmse2, 0.000001), round_any(rmse3, 0.000001), round_any(rmse4, 0.000001)))

# S5 Non-Violent
reg51 <- lm(log(crime_nonviolent) ~ n_permits + log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic, data = blocks)

reg52 <- lm(log(crime_nonviolent) ~ spont + log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic, data = blocks)

nbreg53 <- glm.nb(crime_nonviolent ~ n_permits + log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic, data = blocks)

nbreg54 <- glm.nb(crime_nonviolent ~ spont + log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic, data = blocks)

stargazer(reg51, reg52, nbreg53, nbreg54, single.row =T, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), type="text")

stargazer(reg51, reg52, nbreg53, nbreg54, single.row =T, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), type="latex")

rss1 <- c(crossprod(reg51$residuals)) # Residual sum of squares
mse1 <- rss1 / length(reg51$residuals) # Mean squared error
rmse1 <- sqrt(mse1) # Root MSE
sig1 <- rss1 / reg51$df.residual # Pearson estimated residual variance (as returned by summary.lm)

rss2 <- c(crossprod(reg52$residuals)) # Residual sum of squares
mse2 <- rss2 / length(reg52$residuals) # Mean squared error
rmse2 <- sqrt(mse2) # Root MSE
sig2 <- rss2 / reg52$df.residual # Pearson estimated residual variance (as returned by summary.lm)

rss3 <- c(crossprod(nbreg53$residuals)) # Residual sum of squares
mse3 <- rss3 / length(nbreg53$residuals) # Mean squared error
rmse3 <- sqrt(mse3) # Root MSE
sig3 <- rss3 / nbreg53$df.residual # Pearson estimated residual variance (as returned by summary.lm)

rss4 <- c(crossprod(nbreg54$residuals)) # Residual sum of squares
mse4 <- rss4 / length(nbreg54$residuals) # Mean squared error
rmse4 <- sqrt(mse4) # Root MSE
sig4 <- rss4 / nbreg54$df.residual # Pearson estimated residual variance (as returned by summary.lm)

print(paste("RMSE:", round_any(rmse1, 0.000001), round_any(rmse2, 0.000001), round_any(rmse3, 0.000001), round_any(rmse4, 0.000001)))

# S6 - Vice Crime
reg61 <- lm(log(crime_vice + 0.001) ~ n_permits + log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic, data = blocks)

reg62 <- lm(log(crime_vice + 0.001) ~ spont + log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic, data = blocks)

nbreg63 <- glm.nb(crime_vice ~ n_permits + log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic, data = blocks)

nbreg64 <- glm.nb(crime_vice ~ spont + log(income) + poverty_metric + log_pop + prop_black + prop_hispanic + I(area/1000000) + prop_com + prop_res + prop_vac + prop_trans + prop_ind + prop_park + prop_civic, data = blocks)

stargazer(reg61, reg62, nbreg63, nbreg64, single.row =T, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), type="text")

stargazer(reg61, reg62, nbreg63, nbreg64, single.row =T, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), type="latex")

rss1 <- c(crossprod(reg61$residuals)) # Residual sum of squares
mse1 <- rss1 / length(reg61$residuals) # Mean squared error
rmse1 <- sqrt(mse1) # Root MSE
sig1 <- rss1 / reg61$df.residual # Pearson estimated residual variance (as returned by summary.lm)

rss2 <- c(crossprod(reg62$residuals)) # Residual sum of squares
mse2 <- rss2 / length(reg62$residuals) # Mean squared error
rmse2 <- sqrt(mse2) # Root MSE
sig2 <- rss2 / reg62$df.residual # Pearson estimated residual variance (as returned by summary.lm)

rss3 <- c(crossprod(nbreg63$residuals)) # Residual sum of squares
mse3 <- rss3 / length(nbreg63$residuals) # Mean squared error
rmse3 <- sqrt(mse3) # Root MSE
sig3 <- rss3 / nbreg63$df.residual # Pearson estimated residual variance (as returned by summary.lm)

rss4 <- c(crossprod(nbreg64$residuals)) # Residual sum of squares
mse4 <- rss4 / length(nbreg64$residuals) # Mean squared error
rmse4 <- sqrt(mse4) # Root MSE
sig4 <- rss4 / nbreg64$df.residual # Pearson estimated residual variance (as returned by summary.lm)

print(paste("RMSE:", round_any(rmse1, 0.000001), round_any(rmse2, 0.000001), round_any(rmse3, 0.000001), round_any(rmse4, 0.000001)))

# ======================================================================
# Matched Pairs Experiments
# ======================================================================

## Treatment = having high number of permits or spont ratio
## Predict = actual crime number

blocks <- readRDS("Data/blocks-2020.Rds")
br <- blocks[, .(blockid, n_permits, spont, income, poverty_metric, total, area, vacant_proportion, area_com, area_res, crime_total, crime_vice, crime_violent, crime_nonviolent, prop_black, prop_hispanic)]
br <- br[complete.cases(br),] # keep only observations with complete data

# Treatment = Number of Permits or Spontaneous Ratio
# ======================================================================

# Spont
summary(br$spont) # median = 0.962
medianSpont <- median(br$spont)
br[, spHigh := ifelse(spont <= medianSpont, 0, 1)]

# Number of events
summary(br$n_permits) # median = 43
medianPermits <- median(br$n_permits)
br[, npHigh := ifelse(n_permits <= medianPermits, 0, 1)]

# Prepare data
# ======================================================================

d <- br
d[, pop := total/100]
d[, logincome := log(income)]
d[, areas := as.numeric(area)/1000000]
d[, area_com := as.numeric(area_com)]
d[, area_res := as.numeric(area_res)]

# Choose with spHigh or npHigh as the dependent variable
propscore.model = glm(npHigh ~ logincome + poverty_metric + pop + prop_black + prop_hispanic + areas + area_com + area_res + vacant_proportion, data = d, family=binomial, x=TRUE, y=TRUE)
summary(propscore.model)

# =====================================================================================

d$treated <- propscore.model$y
d$treatment <- d$treated
treated <- d$treated
d$logit.ps <- predict(propscore.model)
table(d$treated, d$logit.ps >= 0.5)
maxControl <- max(d[treated == 0, logit.ps])
minTreated <- min(d[treated == 1, logit.ps])
logit.propscore <- predict(propscore.model)
pooled.sd.logit.propscore <- sqrt(var(logit.propscore[d$treatment==1])/2+var(logit.propscore[d$treatment==0])/2)

table(d$logit.ps >= minTreated - 0.5*pooled.sd.logit.propscore & d$logit.ps <= maxControl + 0.5*pooled.sd.logit.propscore, d$treated) # we'll remove all in the FALSE row

d[, kept := ifelse(logit.ps >= minTreated & logit.ps <= maxControl, TRUE, FALSE)]
kept.index <- d$kept
d <- d[logit.ps >= minTreated & logit.ps <= maxControl,] # only
treated <- d$treated

sum(kept.index) # number of observations that we kept
table(d$treated) # the minimum of the two will be my maximum pairs

Xmat <- propscore.model$x[kept.index,-1] # Matrix of covariates, excluding intercept

# Matrix of covariates to include in the Mahalanobis distance <- should affect crime (outcome variable) the most
# Xmatmahal <- subset(d, select=c(poverty_metric, pop, area_com, area_res))
Xmatmahal <- d[, .(poverty_metric, pop, areas, vacant_proportion, area_com, area_res, prop_black, prop_hispanic, logincome)]

# Rank based Mahalanobis distance
distmat <- smahal(d$treated, as.matrix(Xmatmahal))
# distmat is a CONTROL x TREAT matrix.
summary(as.numeric(distmat))

# Add caliper
distmat2 <- addcaliper(distmat, d$treated, d$logit.ps, calipersd = 0.5, penalty = 1000)
summary(as.numeric(distmat2))

### Name the rows and columns of distance matrix by the subject numbers in treated
# Label the rows and columns of the distance matrix by the rownames in d
rownames(distmat2) <- rownames(d)[d$treated==1]
colnames(distmat2) <- rownames(d)[d$treated==0]

# Matching
# ==================================================================
table(d$treated)
nocontrols.per.match <- 1

matchvec <- pairmatch(distmat2, controls = nocontrols.per.match, data = d, remove.unmatchables = T)
print(summary(matchvec))
print(matchvec, grouped = T)

d$matchvec <- matchvec

## Create a matrix saying which control units each treated unit is matched to
## Create vectors of the subject indices of the treatment units ordered by
## their matched set and corresponding control unit
treated.subject.index = rep(0,sum(treated==1))

matched.control.subject.index.mat = matrix(rep(0,nocontrols.per.match*length(treated.subject.index)),ncol=nocontrols.per.match)

matchedset.index = substr(matchvec,start=3,stop=10)
matchedset.index.numeric = as.numeric(matchedset.index)

for(i in 1:length(treated.subject.index)){
  print(i)
  matched.set.temp = which(matchedset.index.numeric==i)
  print(matched.set.temp)
  #if(length(matched.set.temp) > 1) {
  treated.temp.index = which(d$treated[matched.set.temp]==1)
  treated.subject.index[i] = matched.set.temp[treated.temp.index]
  matched.control.subject.index.mat[i,] = matched.set.temp[-treated.temp.index]
  #}
}

matched.control.subject.index = matched.control.subject.index.mat

### Check balance
# Calculate standardized differences 
# Covariates used in propensity score model
Xmat = propscore.model$x[kept.index,];

# Which variables are missing [Will explain this more later]
missing.mat=matrix(rep(0,ncol(Xmat)*nrow(Xmat)),ncol=ncol(Xmat))
summary(Xmat)

# Put in NAs for all X variables which are missing and for which mean value has been imputed
# [I will discuss issue of missing variables more later]
Xmat.without.missing = Xmat
for(i in 1:ncol(Xmat)){
  Xmat.without.missing[missing.mat[,i]==1,i]=NA
}

treatedmat = Xmat.without.missing[treated==1,];

controlmat.before=Xmat.without.missing[treated==0,];
controlmean.before=apply(controlmat.before,2,mean,na.rm=TRUE);
controlvar=apply(controlmat.before,2,var,na.rm=TRUE);
treatmean=apply(treatedmat,2,mean,na.rm=TRUE);
treatvar=apply(treatedmat,2,var,na.rm=TRUE);
controlmat.after=Xmat[matched.control.subject.index,];
controlmean.after=apply(controlmat.after,2,mean);

# Standardized differences after matching
stand.diff.before=(treatmean-controlmean.before)/sqrt((treatvar+controlvar)/2);
stand.diff.after=(treatmean-controlmean.after)/sqrt((treatvar+controlvar)/2);
cbind(stand.diff.before[-1],stand.diff.after[-1])

# Regression analysis
# ============================================================

{
  print("All crimes")
  reg.formula = update(propscore.model$formula, I(log(crime_total)) ~ treated + matchvec + .)
  matched.reg.model = lm(reg.formula, data = d[!is.na(matchvec),])
  # Point estimate of treatment effect
  print(coef(matched.reg.model)[2])
  print(confint(matched.reg.model)[2,])
  
  reg.formula = update(propscore.model$formula, crime_total ~ treated + matchvec + .)
  matched.reg.model = lm(reg.formula, data = d[!is.na(matchvec),])
  # Point estimate of treatment effect
  print(coef(matched.reg.model)[2])
  print(confint(matched.reg.model)[2,])
  
  print("Violent crimes")
  reg.formula = update(propscore.model$formula, I(log(crime_violent)) ~ treated + matchvec + .)
  matched.reg.model = lm(reg.formula, data = d[!is.na(matchvec),])
  # Point estimate of treatment effect
  print(coef(matched.reg.model)[2])
  print(confint(matched.reg.model)[2,])
  
  reg.formula = update(propscore.model$formula, crime_violent ~ treated + matchvec + .)
  matched.reg.model = lm(reg.formula, data = d[!is.na(matchvec),])
  # Point estimate of treatment effect
  print(coef(matched.reg.model)[2])
  print(confint(matched.reg.model)[2,])
  
  print("Nonviolent crimes")
  
  reg.formula = update(propscore.model$formula, I(log(crime_nonviolent)) ~ treated + matchvec + .)
  matched.reg.model = lm(reg.formula, data = d[!is.na(matchvec),])
  # Point estimate of treatment effect
  print(coef(matched.reg.model)[2])
  print(confint(matched.reg.model)[2,])
  
  reg.formula = update(propscore.model$formula, crime_nonviolent ~ treated + matchvec + .)
  matched.reg.model = lm(reg.formula, data = d[!is.na(matchvec),])
  # Point estimate of treatment effect
  print(coef(matched.reg.model)[2])
  print(confint(matched.reg.model)[2,])
  
  
  print("Vice crimes")
  reg.formula = update(propscore.model$formula, I(log(crime_vice + 0.0001)) ~ treated + matchvec + .)
  matched.reg.model = lm(reg.formula, data = d[!is.na(matchvec),])
  # Point estimate of treatment effect
  print(coef(matched.reg.model)[2])
  print(confint(matched.reg.model)[2,])
  
  reg.formula = update(propscore.model$formula, crime_vice ~ treated + matchvec + .)
  matched.reg.model = lm(reg.formula, data = d[!is.na(matchvec),])
  # Point estimate of treatment effect
  print(coef(matched.reg.model)[2])
  print(confint(matched.reg.model)[2,])
  
  # T-test
  controls <- d[!is.na(matchvec) & treated == 0,]
  treats <- d[!is.na(matchvec) & treated == 1,]
  controls <- controls[order(matchvec),]
  treats <- treats[order(matchvec),]
  
  print("T-Test")
  print(t.test(treats$crime_total, controls$crime_total, paired=T))
  print(t.test(log(treats$crime_total), log(controls$crime_total), paired=T))
  print(t.test(treats$crime_violent, controls$crime_violent, paired=T))
  print(t.test(log(treats$crime_violent), log(controls$crime_violent), paired=T))
  print(t.test(treats$crime_nonviolent, controls$crime_nonviolent, paired=T))
  print(t.test(log(treats$crime_nonviolent), log(controls$crime_nonviolent), paired=T))
  print(t.test(treats$crime_vice, controls$crime_vice, paired=T))
  print(t.test(log(treats$crime_vice + 0.0001), log(controls$crime_vice + 0.0001), paired=T))
  
  print("Wilcoxon")
  print(wilcox.test(treats$crime_total, controls$crime_total, paired=T))
  print(wilcox.test(treats$crime_violent, controls$crime_violent, paired=T))
  print(wilcox.test(treats$crime_nonviolent, controls$crime_nonviolent, paired=T))
  print(wilcox.test(treats$crime_vice, controls$crime_vice, paired=T))
}

# FULL MATCHING
# ========================================================================

{
  
  diff.propensity.score.mat <- outer(d$logit.ps[d$treated==1],d$logit.ps[d$treated==0],"-")
  distmat.propensity <- abs(diff.propensity.score.mat)
  
  # Label the rows and columns of the distance matrix by the rownames in d
  rownames(distmat.propensity)=rownames(d)[d$treated==1]
  colnames(distmat.propensity)=rownames(d)[d$treated==0]
  
  matchvec=fullmatch(distmat.propensity, data = d) # full-matching based on propensity score
  d$matchvec <- matchvec
  d$matchID <- matchvec
  
  print(sort(stratumStructure(matchvec), decreasing = T))
  print(effectiveSampleSize(matchvec))
  print(matchvec, grouped=T)
  # d[c(7, 1094), ]
  d[, matchsize := length(unique(blockid)), by = matchID]
  d <- d[order(matchID, treated),]
  head(d[matchsize == 2, .(match = matchID, treated, block = blockid, logincome, poverty = poverty_metric, pop, black = prop_black, hispanic = prop_hispanic, areas, area_com, area_res, area_vacant = vacant_proportion, logit.ps)], 10)
  
  # Number the strata
  matchedset.index=substr(matchvec,start=3,stop=10)
  matchedset.index.numeric=as.numeric(matchedset.index)
  
  # Calculate standardized difference before and after a full match
  
  # Drop observations with missing values from the calculations
  # stratum.myindex should contain strata for each subject, 0 means a unit was not 
  # matched
  standardized.diff.func=function(x,treatment,stratum.myindex,missing=rep(0,length(x))){
    xtreated=x[treatment==1 & missing==0];
    xcontrol=x[treatment==0 & missing==0];
    var.xtreated=var(xtreated);
    var.xcontrol=var(xcontrol);
    combinedsd=sqrt(.5*(var.xtreated+var.xcontrol));
    std.diff.before.matching=(mean(xtreated)-mean(xcontrol))/combinedsd;
    nostratum=length(unique(stratum.myindex))-1*max(stratum.myindex==0);
    if(max(stratum.myindex==0)==0){
      stratumlist=sort(unique(stratum.myindex))
    }
    if(max(stratum.myindex==0)==1){
      templist=sort(unique(stratum.myindex))
      stratumlist=templist[-1]
    }
    diff.in.stratum=rep(0,nostratum);
    number.in.stratum=rep(0,nostratum);
    for(i in 1:nostratum){
      if(sum(stratum.myindex==stratumlist[i] & treatment==1 & missing==0)==0 | sum(stratum.myindex==stratumlist[i] & treatment==0 & missing==0)==0){
        number.in.stratum[i]=0
      }
      if(sum(stratum.myindex==stratumlist[i] & treatment==1 & missing==0)>0 & sum(stratum.myindex==stratumlist[i] & treatment==0 & missing==0)>0){
        diff.in.stratum[i]=mean(x[stratum.myindex==stratumlist[i] & treatment==1 & missing==0])-mean(x[stratum.myindex==stratumlist[i] & treatment==0 & missing==0]);
        number.in.stratum[i]=sum(stratum.myindex==stratumlist[i])
      }
    }
    std.diff.after.matching=(sum(number.in.stratum*diff.in.stratum)/sum(number.in.stratum))/combinedsd;
    list(std.diff.before.matching=std.diff.before.matching,std.diff.after.matching=std.diff.after.matching);
  }
  
  # Covariates used in propensity score model
  Xmat=propscore.model$x[kept.index,];
  
  missing.mat=matrix(rep(0,ncol(Xmat)*nrow(Xmat)),ncol=ncol(Xmat))
  summary(Xmat)
  
  # Put in NAs for all X variables which are missing and for which mean value has been imputed
  # [I will discuss issue of missing variables more later]
  Xmat.without.missing = Xmat
  # for(i in 1:ncol(Xmat)){
  #   Xmat.without.missing[missing.mat[,i]==1,i]=NA
  # }
  
  # Calculate the standardized differences
  std.diff.before=rep(0,ncol(Xmat.without.missing));
  std.diff.after=rep(0,ncol(Xmat.without.missing));
  names(std.diff.before)=names(Xmat[1,]);
  names(std.diff.after)=names(Xmat[1,]);
  for(i in 1:ncol(Xmat.without.missing)){
    missing.temp=is.na(Xmat.without.missing[,i])
    temp.stand.diff=standardized.diff.func(Xmat.without.missing[,i],d$treated,matchedset.index.numeric,missing.temp);
    std.diff.before[i]=temp.stand.diff$std.diff.before.matching;
    std.diff.after[i]=temp.stand.diff$std.diff.after.matching;
  }
  
  # Rename std.diff.before and std.diff.after to shorter names sd.bf and sd.af
  # and use digits option to be able to columns of std.diff.before and 
  # std.diff.after in one row
  sd.bf=std.diff.before
  sd.af=std.diff.after
  print(cbind(sd.bf,sd.af))
  
  covariates=names(sd.bf[-1])
  
  # Love plots
  plot.dataframe=data.frame(stand.diff=c(sd.bf[-1], sd.af[-1]),covariates=rep(covariates,2),type=c(rep("Unmatched",length(covariates)),rep("Matched",length(covariates)))) 
  plot.dataframe$covariates <- c("Log(income)", "Poverty", "Population", "Black", "Hispanic", "Area", "Commercial", "Residential", "Vacant", "Log(income)", "Poverty", "Population", "Black", "Hispanic", "Area", "Commercial", "Residential", "Vacant")
  
  ggplot(plot.dataframe,aes(x=stand.diff,y=covariates))+geom_point(size=5,aes(shape=factor(type)))+scale_shape_manual(values=c(4,1)) + geom_vline(xintercept=c(-.2,.2),col="grey",lty=2)  + labs(shape="", x="Standard Differences", y="Covariates") + geom_vline(xintercept=0,lty=1, col="red") 
  
  {
    print("All crimes")
    
    # using full matching pairs
    reg.formula = update(propscore.model$formula, I(log(crime_total)) ~ treated + matchvec + .)
    matched.reg.model = lm(reg.formula, data = d[!is.na(matchvec),])
    # Point estimate of treatment effect
    print(coef(matched.reg.model)[2])
    print(confint(matched.reg.model)[2,])
    
    reg.formula = update(propscore.model$formula, crime_total ~ treated + matchvec + .)
    matched.reg.model = lm(reg.formula, data = d[!is.na(matchvec),])
    # Point estimate of treatment effect
    print(coef(matched.reg.model)[2])
    print(confint(matched.reg.model)[2,])
    
    # using matchID <- only 88 one-to-one pairs from full matching
    reg.formula = update(propscore.model$formula, I(log(crime_total)) ~ treated + matchID + .)
    matched.reg.model = lm(reg.formula, data = d[matchsize == 2,])
    # Point estimate of treatment effect
    print(coef(matched.reg.model)[2])
    print(confint(matched.reg.model)[2,])
    
    reg.formula = update(propscore.model$formula, crime_total ~ treated + matchID + .)
    matched.reg.model = lm(reg.formula, data = d[matchsize == 2,])
    # Point estimate of treatment effect
    print(coef(matched.reg.model)[2])
    print(confint(matched.reg.model)[2,])
    
    print("Violent crimes")
    reg.formula = update(propscore.model$formula, I(log(crime_violent)) ~ treated + matchvec + .)
    matched.reg.model = lm(reg.formula, data = d[!is.na(matchvec),])
    # Point estimate of treatment effect
    print(coef(matched.reg.model)[2])
    print(confint(matched.reg.model)[2,])
    
    reg.formula = update(propscore.model$formula, crime_violent ~ treated + matchvec + .)
    matched.reg.model = lm(reg.formula, data = d[!is.na(matchvec),])
    # Point estimate of treatment effect
    print(coef(matched.reg.model)[2])
    print(confint(matched.reg.model)[2,])
    
    print("Nonviolent crimes")
    
    reg.formula = update(propscore.model$formula, I(log(crime_nonviolent)) ~ treated + matchvec + .)
    matched.reg.model = lm(reg.formula, data = d[!is.na(matchvec),])
    # Point estimate of treatment effect
    print(coef(matched.reg.model)[2])
    print(confint(matched.reg.model)[2,])
    
    reg.formula = update(propscore.model$formula, crime_nonviolent ~ treated + matchvec + .)
    matched.reg.model = lm(reg.formula, data = d[!is.na(matchvec),])
    # Point estimate of treatment effect
    print(coef(matched.reg.model)[2])
    print(confint(matched.reg.model)[2,])
    
    print("Vice crimes")
    reg.formula = update(propscore.model$formula, I(log(crime_vice + 0.0001)) ~ treated + matchvec + .)
    matched.reg.model = lm(reg.formula, data = d[!is.na(matchvec),])
    # Point estimate of treatment effect
    print(coef(matched.reg.model)[2])
    print(confint(matched.reg.model)[2,])
    
    reg.formula = update(propscore.model$formula, crime_vice ~ treated + matchvec + .)
    matched.reg.model = lm(reg.formula, data = d[!is.na(matchvec),])
    # Point estimate of treatment effect
    print(coef(matched.reg.model)[2])
    print(confint(matched.reg.model)[2,])
  }
}
