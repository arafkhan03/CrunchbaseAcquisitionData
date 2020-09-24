colMeans(is.na(acquisition))

# Acquisition Data Descriptive Stats

# 1.0 Data Preparation and Library Loading
library(skimr)
library(dplyr)
library(plyr)
library(lubridate)
library(ggplot2)

## Summary
dim(acquisition)
skim(acquisition)

## Only Keep Columns that have NAs Less than 75%
acquisition_NA <- as.data.frame(
        round(colMeans(is.na(acquisition)), 2)
)
sum(acquisition_NA$`round(colMeans(is.na(acquisition)), 2)` < 0.75)
acquisition_WNA <- acquisition[, acquisition_NA$`round(colMeans(is.na(acquisition)), 2)` < 0.75]

## Summary
skim(acquisition_WNA)





# 2.0 Number of Acquirer by Number of Acquisition

acquirer_count_acquisition <- acquisition_WNA %>%
        dplyr::group_by(`Acquirer Name`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Acquirer = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Arrange DF
acquirer_count_acquisition <- arrange(acquirer_count_acquisition, 
                                      desc(acquirer_count_acquisition$Count_by_Acquirer))

        ## Add Percentage
acquirer_count_acquisition$Percentage <- as.numeric(format(acquirer_count_acquisition$Count_by_Acquirer/
                                                                   sum(acquirer_count_acquisition$Count_by_Acquirer), 
                                                           scientific = FALSE, digits = 2))

        ## Take Top 30
top30_acquirer_count_acquisition <- acquirer_count_acquisition[1:30,]
sum(top30_acquirer_count_acquisition$Percentage)

        ## Plot Number of Investors by Regions
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)

                ### Generate the layout. This function return a dataframe with one line per bubble. 
                ### It gives its center (x and y) and its radius, proportional of the value
acquirer_packing_acquisition <- circleProgressiveLayout(top30_acquirer_count_acquisition$Count_by_Acquirer, 
                                                     sizetype='area')

                ### We can add these packing information to the initial data frame
data_acquisition_acquirer_circlepack <- cbind(top30_acquirer_count_acquisition, acquirer_packing_acquisition)

                ### The next step is to go from one center + a radius to the coordinates of a circle that
                ### is drawn by a multitude of straight lines.
dat.gg_acquirer_acquisition_circle <- circleLayoutVertices(acquirer_packing_acquisition, 
                                                        npoints=50)

        ## Make the plot
Acquirer_Acquisition_Circleplot <- ggplot() + 
        
                ### Make the bubbles
        geom_polygon(data = dat.gg_acquirer_acquisition_circle, aes(x, y, group = id, 
                                                                 fill=as.factor(id)), 
                     colour = "black", alpha = 0.6) +
        
                ### Add text in the center of each bubble + control its size
        geom_text(data = data_acquisition_acquirer_circlepack, aes(x, y, size=Count_by_Acquirer, 
                                                                label = `Acquirer Name`)) +
        ggtitle("Distribution of Top 30 Acquirers by Acquisition") +
        
                ### General theme:
        scale_size_continuous(range = c(1,4)) +
        theme_void() + 
        theme(legend.position="none") +
        coord_equal()

Acquirer_Acquisition_Circleplot






# 2.0 Count of Acquisition by Announced Year

        ## Add Year Variable
acquisition_WNA$Year <- lubridate::year(acquisition_WNA$`Announced Date`)

        ## Cut Year Variable into Factor
acquisition_WNA$year_factor <- cut(acquisition_WNA$Year, breaks = 10, dig.lab = 4)

        ## Summarize No. of Acquisition by Announced Year
year_count_acquistion <- acquisition_WNA %>%
        dplyr::group_by(year_factor, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Year = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Add Percentage
year_count_acquistion$Percentage <- as.numeric(format(year_count_acquistion$Count_by_Year/
                                                              sum(year_count_acquistion$Count_by_Year), 
                                                      scientific = FALSE, digits = 2))

        ## Plot Number of Acquisition by Announced Year
ylim_year_acquisition <- c(0, 1.1*max(year_count_acquistion$Count_by_Year))
xx_year_acquisition <- barplot(year_count_acquistion$Count_by_Year, xaxt = 'n', xlab = '', width = 0.85,
                   ylim = ylim_year_acquisition, main = "Number of Acquisitions by Year of Announcement", 
                   ylab = "Frequency")
text(x = xx_year_acquisition, y = year_count_acquistion$Count_by_Year, 
     label = year_count_acquistion$Count_by_Year, pos = 3, cex = 0.8, col = "black")
axis(1, at=xx_year_acquisition, labels=year_count_acquistion$year_factor, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)






# 3.0 Number of Acquisitions by Top 10 Industries of Acquirers

        ## Summarize Number of Acquisitions by Top 10 Industries of Acquirers
acquirer_industry_count_acquistion <- acquisition_WNA %>%
        dplyr::group_by(`Acquirer Industries`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Acquirer_Industry = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Add Percentage
acquirer_industry_count_acquistion$Percentage <- as.numeric(format(acquirer_industry_count_acquistion$Count_by_Acquirer_Industry/
                                                                           sum(acquirer_industry_count_acquistion$Count_by_Acquirer_Industry), 
                                                                   scientific = FALSE, digits = 2))

        ## Arrange DF
acquirer_industry_count_acquistion <- arrange(acquirer_industry_count_acquistion, 
                                              desc(acquirer_industry_count_acquistion$Count_by_Acquirer_Industry))

        ## Take top 10
acquirer_industry_count_acquistion <- acquirer_industry_count_acquistion[1:10,]
sum(acquirer_industry_count_acquistion$Percentage)

        ## Plot Number of Acquisitions by Top 10 Industries of Acquirers
par(mar = c(10,4,4,4))
ylim_acquirer_industry_acquisition <- c(0, 1.1*max(acquirer_industry_count_acquistion$Count_by_Acquirer_Industry))
xx_acquirer_industry_acquisition <- barplot(acquirer_industry_count_acquistion$Count_by_Acquirer_Industry, xaxt = 'n', xlab = '', width = 0.85,
                               ylim = ylim_acquirer_industry_acquisition, main = "Number of Acquisitions by Top 10 Industries of Acquirers", 
                               ylab = "Frequency")
text(x = xx_acquirer_industry_acquisition, y = acquirer_industry_count_acquistion$Count_by_Acquirer_Industry, 
     label = acquirer_industry_count_acquistion$Count_by_Acquirer_Industry, pos = 3, cex = 0.5, col = "black")
axis(1, at=xx_acquirer_industry_acquisition, labels = acquirer_industry_count_acquistion$`Acquirer Industries`, 
     tick=FALSE, las=2, line=-0.5, cex.axis=0.5)






# 4.0 Number of Acquisitions by Top 10 Industries of Acquirees

        ## Summarize Number of Acquisitions by Top 10 Industries of Acquirees
acquiree_industry_count_acquistion <- acquisition_WNA %>%
        dplyr::group_by(`Acquiree Industries`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Acquiree_Industry = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Add Percentage
acquiree_industry_count_acquistion$Percentage <- as.numeric(format(acquiree_industry_count_acquistion$Count_by_Acquiree_Industry/
                                                                           sum(acquiree_industry_count_acquistion$Count_by_Acquiree_Industry), 
                                                                   scientific = FALSE, digits = 2))

        ## Arrange DF
acquiree_industry_count_acquistion <- arrange(acquiree_industry_count_acquistion, 
                                              desc(acquiree_industry_count_acquistion$Count_by_Acquiree_Industry))

        ## Take top 10
acquiree_industry_count_acquistion <- acquiree_industry_count_acquistion[1:10,]
sum(acquiree_industry_count_acquistion$Percentage)

        ## Plot Number of Acquisitions by Top 10 Industries of Acquirees
ylim_acquiree_industry_acquisition <- c(0, 1.1*max(acquiree_industry_count_acquistion$Count_by_Acquiree_Industry))
xx_acquiree_industry_acquisition <- barplot(acquiree_industry_count_acquistion$Count_by_Acquiree_Industry, xaxt = 'n', xlab = '', width = 0.85,
                                            ylim = ylim_acquiree_industry_acquisition, main = "Number of Acquisitions by Top 10 Industries of Acquirees", 
                                            ylab = "Frequency")
text(x = xx_acquiree_industry_acquisition, y = acquiree_industry_count_acquistion$Count_by_Acquiree_Industry, 
     label = acquiree_industry_count_acquistion$Count_by_Acquiree_Industry, pos = 3, cex = 0.5, col = "black")
axis(1, at=xx_acquiree_industry_acquisition, labels = acquiree_industry_count_acquistion$`Acquiree Industries`, 
     tick=FALSE, las=2, line=-0.5, cex.axis=0.5)






# 5.0 No. of Acquisition by Acquirer Est Rev Range

        ## Summarize No. of Acquisition by Acquirer Est Rev Range
acquirer_revrange_count_acquistion <- acquisition_WNA %>%
        dplyr::group_by(`Acquirer's Estimated Revenue Range`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Acquirer_Revrange = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Add Percentage
acquirer_revrange_count_acquistion$Percentage <- as.numeric(format(acquirer_revrange_count_acquistion$Count_by_Acquirer_Revrange/
                                                                           sum(acquirer_revrange_count_acquistion$Count_by_Acquirer_Revrange), 
                                                                   scientific = FALSE, digits = 2))
        
        ## Assign Index
acquirer_revrange_count_acquistion$index <- c(5, 8, 3, 7, 2, 6, 4, 1)

        ## Sort DF by Index
acquirer_revrange_count_acquistion <-  arrange(acquirer_revrange_count_acquistion, 
                                               acquirer_revrange_count_acquistion$index)

        ## Plot No. of Acquisition by Acquirer Est Rev Range
ylim_acquirer_revrange_acquisition <- c(0, 1.1*max(acquirer_revrange_count_acquistion$Count_by_Acquirer_Revrange))
xx_acquirer_revrange_acquisition <- barplot(acquirer_revrange_count_acquistion$Count_by_Acquirer_Revrange, xaxt = 'n', xlab = '', width = 0.85,
                                            ylim = ylim_acquirer_revrange_acquisition, main = "Number of Acquisitions by Acquirer's Estimated Revenue Range", 
                                            ylab = "Frequency")
text(x = xx_acquirer_revrange_acquisition, y = acquirer_revrange_count_acquistion$Count_by_Acquirer_Revrange, 
     label = acquirer_revrange_count_acquistion$Count_by_Acquirer_Revrange, pos = 3, cex = 0.8, col = "black")
axis(1, at=xx_acquirer_revrange_acquisition, labels = acquirer_revrange_count_acquistion$`Acquirer's Estimated Revenue Range`, 
     tick=FALSE, las=2, line=-0.5, cex.axis=0.5)






# 6.0 No. of Acquisition by Acquisition Type

        ## Summarize No. of Acquisition Type
acquisition_type_count_acquistion <- acquisition_WNA %>%
        dplyr::group_by(`Acquisition Type`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Acquisition_Type = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Drop Mistake (Acquihire)
acquisition_type_count_acquistion <- acquisition_type_count_acquistion[2:5,]

        ## Add Percentage
acquisition_type_count_acquistion$Percentage <- as.numeric(format(acquisition_type_count_acquistion$Count_by_Acquisition_Type/
                                                                           sum(acquisition_type_count_acquistion$Count_by_Acquisition_Type), 
                                                                   scientific = FALSE, digits = 2))

        ## Plot No. of Acquisition by Acquisition Type
ylim_acquisition_type_acquisition <- c(0, 1.1*max(acquisition_type_count_acquistion$Count_by_Acquisition_Type))
xx_acquisition_type_acquisition <- barplot(acquisition_type_count_acquistion$Count_by_Acquisition_Type, xaxt = 'n', xlab = '', width = 0.85,
                                            ylim = ylim_acquisition_type_acquisition, main = "Number of Acquisitions by Acquisition Type", 
                                            ylab = "Frequency")
text(x = xx_acquisition_type_acquisition, y = acquisition_type_count_acquistion$Count_by_Acquisition_Type, 
     label = acquisition_type_count_acquistion$Count_by_Acquisition_Type, pos = 3, cex = 0.8, col = "black")
axis(1, at=xx_acquisition_type_acquisition, labels = acquisition_type_count_acquistion$`Acquisition Type`, 
     tick=FALSE, las=2, line=-0.5, cex.axis=0.5)






# 7.0 No. of Acquisition by Acquirer Funding Status

        ## Summarize No. of Acquisition by Acquirer Funding Status
acquirer_fundstat_acquistion <- acquisition_WNA %>%
        dplyr::group_by(`Acquirer Funding Status`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Acquirer_Funding_Status = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Add Percentage
acquirer_fundstat_acquistion$Percentage <- as.numeric(format(acquirer_fundstat_acquistion$Count_by_Acquirer_Funding_Status/
                                                                          sum(acquirer_fundstat_acquistion$Count_by_Acquirer_Funding_Status), 
                                                                  scientific = FALSE, digits = 2))

        ## Plot No. of Acquisition by Acquirer Funding Status
ylim_acquirer_fundstat_acquisition <- c(0, 1.1*max(acquirer_fundstat_acquistion$Count_by_Acquirer_Funding_Status))
xx_acquirer_fundstat_acquisition <- barplot(acquirer_fundstat_acquistion$Count_by_Acquirer_Funding_Status, xaxt = 'n', xlab = '', width = 0.85,
                                           ylim = ylim_acquirer_fundstat_acquisition, main = "Number of Acquisitions by Acquirer's Funding Status", 
                                           ylab = "Frequency")
text(x = xx_acquirer_fundstat_acquisition, y = acquirer_fundstat_acquistion$Count_by_Acquirer_Funding_Status, 
     label = acquirer_fundstat_acquistion$Count_by_Acquirer_Funding_Status, pos = 3, cex = 0.8, col = "black")
axis(1, at=xx_acquirer_fundstat_acquisition, labels = acquirer_fundstat_acquistion$`Acquirer Funding Status`, 
     tick=FALSE, las=2, line=-0.5, cex.axis=0.5)

