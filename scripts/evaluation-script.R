library(tidyverse)

csv_data <- read.csv(file="../responses/Presidential Election 2020 (Responses) - Hewainna.csv", header=TRUE, sep=",")
colnames(csv_data) <- c("vote?", "first_vote", "second_vote", "third_vote", "reason", "vote_in_2015", "vote_in_2018", "voting_members_count", "follow_opinion?", "income_level", "ethnicity", "religion")
head(csv_data)
summary(csv_data$firstVote)

draw_categorical_pie_chart <- function(df, title_name) {
  names(df)[1] <- "categoricalVar"
  df$n <- df$n * 100 / sum(df$n)   # convert count to percentage
  
  # Create a basic bar
  pie = ggplot(df, aes(x="", y=n, fill=categoricalVar)) + geom_bar(stat="identity", color='white')
  
  # Convert to pie (polar coordinates) and add labels
  pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(n), "%")), position = position_stack(vjust = 0.5))
  
  # Remove labels and add title
  pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = title_name)
  
  # Tidy up the theme
  pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                      axis.text = element_blank(),
                                      axis.ticks = element_blank(),
                                      plot.title = element_text(hjust = 0.5))
  print(pie)
}

# Increase the figure size
options(repr.plot.width=8, repr.plot.height=8)

# draw_categorical_pie_chart(count(csv_data, first_vote), 'First vote of participants')
# draw_categorical_pie_chart(count(csv_data, second_vote), 'Second vote of participants')
# draw_categorical_pie_chart(count(csv_data, income_level), 'Income level distribution')
# draw_categorical_pie_chart(count(csv_data, ethnicity), 'Ethnicity distribution')
# draw_categorical_pie_chart(count(csv_data, religion), 'Religion distribution')


