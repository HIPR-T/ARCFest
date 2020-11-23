# Can you do me a favour - put the attached excel thing into a shiny app that people can access online.Basically its for a workshop, 
# - people have 2 CCGs 
# CCG1 has double the amount of depression than CCG1, but half the waiting time
# - participants then have 1 million to spend on new depression services and they need to change the share that goes to each CCG. 
# - spending more - reduces the prevalence of depression and reduces waiting times.
# 
# So what I want is a display with those two charts - with a slider determining the share that goes to each CCG and then 
# as you change that,  the charts showing the prevalence and waiting times changes.


library(data.table)
options(scipen = 999)

eqdf <- fread(normalizePath("raw_data/access_equity_trade.csv"))
eqdf


allocdf <- data.table( "After Funding CCG Charactersitics" = c("CCG1", "CCG2"),
                      "Prevalence of depression %" = NA, 
                      "Waiting times - months" = NA)

allocdf$`Prevalence of depression %` <- eqdf$preval_depress_pc * (eqdf$preval_reduction ^ ((eqdf$total_budget * eqdf$share)/100000))
allocdf$`Waiting times - months` <- eqdf$waiting_months * (eqdf$waiting_reduction ^ ((eqdf$total_budget * eqdf$share)/100000))

