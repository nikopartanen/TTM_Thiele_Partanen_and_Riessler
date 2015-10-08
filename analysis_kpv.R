# CC-BY Hanna Thiele & Niko Partanen 2015
# R package FRelan can be installed with the following commands

# We load all the packages in the beginning. FRelan is not available in CRAN,
# so one has to install it with devtools. The package is used to parse ELAN
# tiers into an R dataframe.

# https://github.com/langdoc/FRelan 

# library(devtools)
# install_github(repo = "FRelan", username = "langdoc")

library(FRelan)
library(plyr)
library(dplyr)
library(readr)
library(zoo)
library(ggplot2)


read_graid <- function(file){
        
        # Here we parse the tiers we want
        
        graid_funct <- read_tier(eaf_file = file, 
                                 tier = "GRAIDfunctT") %>% 
                rename(graid_funct = Token) %>% 
                tbl_df %>%
                select(graid_funct)
        
        graid_token <- read_tier(eaf_file = file,
                                 tier = "GRAIDwordT") %>% 
                tbl_df %>%
                select(Session_name, Token)
        
        graid_form <- read_tier(eaf_file = file,
                                tier = "GRAIDformT") %>% 
                rename(graid_form = Token) %>% 
                tbl_df %>%
                select(graid_form)
        
        # Those tiers are merged into their own data frame
        
        graid <- cbind(graid_token, graid_funct, graid_form) %>% tbl_df
        
        # We copy the sentence boundary marking and numerate the sentences
        
        graid[graid$Token == "#",]$graid_funct <- "#"

        graid[grep(".+#(.+)?", graid$Token),]$graid_funct <- "#"
        
        graid$nr <- NA
        graid$nr[1] <- 0
        graid[graid$Token == "#",]$nr <- 1:length(graid[graid$Token == "#",]$nr)
        
        library(zoo)
        graid$nr <- na.locf(graid$nr)
        
        
        # This is determining a token's position relative to the predicate within the same clause
        graid$position <- "unknown"
        graid[graid$graid_funct == "#",]$position <- "#"
        graid[graid$graid_funct == "pred",]$position <- "pred"
        graid[which(grepl("pred", graid$position)) - c(1),]$position <- ifelse(graid[which(grepl("pred", graid$position)) - c(1),]$position == "#", "#", "prepred")
        
        i <- 1
        while(i < 20)
        {
                graid[which(grepl("prepred", graid$position)) - c(1),]$position <- ifelse(graid[which(grepl("prepred", graid$position)) - c(1),]$position == "#", "#", "prepred")
                i <- i + 1
        }
        graid[which(graid$position == "pred") + c(1),]$position <- ifelse(graid[which(graid$position == "pred") + c(1),]$position == "#", "#", "postpred")
        i <- 1
        while(i < 20)
        {
                graid[which(grepl("postpred", graid$position)) + c(1),]$position <- ifelse(graid[which(grepl("postpred", graid$position)) + c(1),]$position == "#", "#", "postpred")
                i <- i + 1
        } 
        
        ##### Weight part
        
        ### This is determining a token's phrase weight - heads of NPs get weight according do the number of "ln" items that precede them directly ("rn" items have not been implemented yet, since they don't seem to occur in Kildin Saami), Pronouns and adverbs get weight 1. 
        
        ### (Note for Niko: Hanna has a version that takes "rn" into account, add that!)
        
        graid$phraseWeight <- 0
        graid[grepl("^np", graid$graid_form),]$phraseWeight <- 1
        i <- 1
        while(i < 6)
        {
                graid[which(graid$phraseWeight > 0) - c(1),]$phraseWeight <- ifelse(grepl("ln", graid[which(graid$phraseWeight > 0) - c(1),]$graid_form), 1, graid[which(graid$phraseWeight > 0) - c(1),]$phraseWeight)
                i <- i + 1
        }
        i <- 1
        while(i < 6)
        {
                graid[which(graid$phraseWeight > 0) + c(1),]$phraseWeight <- ifelse(grepl("^np",graid[which(graid$phraseWeight > 0),]$graid_form), graid[which(graid$phraseWeight > 0) + c(1),]$phraseWeight,
                                                                                    ifelse(graid[which(graid$phraseWeight > 0) + c(1),]$phraseWeight > 0, graid[which(graid$phraseWeight > 0) + c(1),]$phraseWeight + 1, 0))
                graid[graid$phraseWeight == i,]$phraseWeight <- ifelse(grepl("ln", graid[graid$phraseWeight == i,]$graid_form), 0, i)
                i <- i + 1
        }
        graid[grepl("pro", graid$graid_form),]$phraseWeight <- 1
        graid[grepl("adv", graid$graid_form),]$phraseWeight <- 1 
        
        ###
        
        graid
}

# This parses the ELAN files using the function defined above
# We select the parts which are thoroughly annotated and also checked over
# The number there also is a convenient work-in-progress way to keep some track

vpr <- read_graid(file = "./kpv/kpv_izva20140325-3sledges-b.eaf")
vpr <- vpr[1:1161,]
mvf <- read_graid("./kpv/kpv_izva20140330-1FilippovaMV-b.eaf")
mvf <- mvf[1:1719,]
iib <- read_graid("./kpv/kpv_lit20080000belyx-190.eaf")
iib <- iib[1:1415,]
vtc <- read_graid("./kpv/kpv_izva20130000VKn10Chuprov.eaf")
vtc <- vtc[1:1286,]

# Let's ad some metadata in this point, with more files it would make sense to store is separatedly, but with just four it doesn't really make any difference

vpr$mode <- "spoken"
mvf$mode <- "spoken"
iib$mode <- "written"
vtc$mode <- "written"

graid <- rbind(vpr, mvf, iib, vtc)

# This sets again the phraseweight factor levels

graid$phraseWeight <- as.factor(graid$phraseWeight)
graid$phraseWeight <- gsub("(3|4|5|6)", "3+", graid$phraseWeight)

# This marks the accusative types — not relevant now, but still interesting

graid$p_marking <- gsub("(.+(сӧ|сэ|тӧ|тэ|эс|ес|ӧс)$|менэ|менӧ|тэнэ|тэнӧ|сіе|сійӧ)", "accusative", graid$Token)
graid$p_marking <- gsub("^[^accusative]*$", "nominative", graid$p_marking)
graid$p_marking <- gsub("accusativeс", "accusative", graid$p_marking)

# This sets the factor levels again

graid$position <- as.factor(graid$position)
graid$position <- factor(graid$position, levels = c("prepred", "postpred", "pred", "unknown", "#"))

# The plots are made here.

p_g_no_mode <- ggplot(data = graid %>% 
                              filter(grepl("^(p|g|g_vic)$", graid_funct)) %>%
                              filter(! graid_form == "unknown") %>%
                              filter(! position == "unknown") %>%
                              filter(! phraseWeight == 0),
                      aes(x = position)) +
        geom_histogram() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(text = element_text(size=16))

ggsave(filename = "./plots/1-kpv-goals_and_objects_no_mode.png", plot = p_g_no_mode)


p_g_all <- ggplot(data = graid %>% 
                          filter(grepl("^(p|g|g_vic)$", graid_funct)) %>%
                          filter(! graid_form == "unknown") %>%
                          filter(! position == "unknown") %>%
                          filter(! phraseWeight == 0),
                  aes(x = position)) +
        geom_histogram() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        facet_grid(. ~ phraseWeight) +
        theme(text = element_text(size=16))

ggsave(filename = "./plots/2-kpv-goals_and_objects_all.png", plot = p_g_all)



p_g <- ggplot(data = graid %>% 
                      filter(grepl("^(p|g|g_vic)$", graid_funct)) %>%
                      filter(! graid_form == "unknown") %>%
                      filter(! position == "unknown") %>%
                      filter(! phraseWeight == 0),
              aes(x = position)) +
        geom_histogram() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        facet_grid(mode ~ .) +
        theme(text = element_text(size=16))

ggsave(filename = "./plots/3-kpv-goals_and_objects.png", plot = p_g)



p_g_pw <- ggplot(data = graid %>% 
                         filter(grepl("^(p|g|g_vic)$", graid_funct)) %>%
                         filter(! graid_form == "unknown") %>%
                         filter(! position == "unknown") %>%
                         filter(! phraseWeight == 0),
                 aes(x = position)) +
        geom_histogram() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        facet_grid(mode ~ phraseWeight) +
        theme(text = element_text(size=16))

ggsave(filename = "./plots/4-kpv-goals_and_objects_with_pw.png", plot = p_g_pw)

