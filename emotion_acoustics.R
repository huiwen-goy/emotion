
# Code and required libraries for generating each figure

# FIGURE: How emotions vary on each characteristic
library(ggplot2)
library(gridExtra)

# Plot emotions as overlapping distributions; one plot per measure
g1 <- (ggplot(data = acoustic, aes(Speech_Duration, fill = Emotion)) + 
  geom_density(alpha=0.25) + 
  scale_fill_manual(values = c("red","green","skyblue","orange","grey80","purple","black")) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(colour = "black", size = 14)) +
  theme(legend.position = "none") +   
  xlab('Sentence duration (s)') )

g2 <- (ggplot(data = acoustic, aes(F0_mean, fill = Emotion)) + 
  geom_density(alpha=0.25) + 
  scale_fill_manual(values = c("red","green","skyblue","orange","grey80","purple","black")) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(colour = "black", size = 14)) +
  theme(legend.position = "none") + 
  xlab('F0 mean (Hz)') )

g3 <- (ggplot(data = acoustic, aes(F0_sd, fill = Emotion)) + 
  geom_density(alpha=0.25) + 
  scale_fill_manual(values = c("red","green","skyblue","orange","grey80","purple","black")) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(colour = "black", size = 14)) +
  theme(legend.position = "none") + 
  xlab('F0 SD (Hz)') )

g4 <- (ggplot(data = acoustic, aes(F0_slope, fill = Emotion)) + 
  geom_density(alpha=0.25) + 
  scale_fill_manual(values = c("red","green","skyblue","orange","grey80","purple","black")) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(colour = "black", size = 14)) +
  theme(legend.position = "none") + 
  xlab('F0 slope') )

g5 <- (ggplot(data = acoustic, aes(Spectral_COG, fill = Emotion)) + 
  geom_density(alpha=0.25) + 
  scale_fill_manual(values = c("red","green","skyblue","orange","grey80","purple","black")) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 12), axis.text = element_text(colour = "black", size = 12)) +
  theme(legend.position = "none") + 
  xlab('Spectral center-of-gravity (Hz)') )

g6 <- (ggplot(data = acoustic, aes(Int_mean, fill = Emotion)) + 
  geom_density(alpha=0.25) + 
  scale_fill_manual(values = c("red","green","skyblue","orange","grey80","purple","black")) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(colour = "black", size = 14)) +
  theme(legend.position = "none") + 
  xlab('Intensity mean (dB)') )

g7 <- (ggplot(data = acoustic, aes(Int_sd, fill = Emotion)) + 
  geom_density(alpha=0.25) + 
  scale_fill_manual(values = c("red","green","skyblue","orange","grey80","purple","black")) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(colour = "black", size = 14)) +
  theme(legend.position = "none") +
  xlab('Intensity SD (dB)') )

# Plot one version of g2 that contains a legend; this legend will be extracted
tempg2 <- (ggplot(data = acoustic, aes(F0_mean, fill = Emotion)) + 
  geom_density(alpha=0.25) + 
  scale_fill_manual(values = c("red","green","skyblue","orange","grey80","purple","black")) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20), 
    axis.text = element_text(colour = "black", size = 16), 
    text = element_text(size = 16)) +
  xlab('F0 mean (Hz)') )

# Function to extract legend
# Source: https://stackoverflow.com/questions/11883844/inserting-a-table-under-the-legend-in-a-ggplot2-histogram
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# Extract legend
legend <- g_legend(tempg2)

# Compile into a panel with 7 plots and 1 common legend for all plots
grid.arrange(g1, g2, g3, g4, g5, g6, g7, legend, ncol=4, nrow=2)

# FIGURE: How do characteristics correlate?
library(corrplot)

corrplot(cor(acoustic[, c(4:10)]), 
         method = "color", 
         col = colorRampPalette(c("blue","white","red"))(200),
         outline = TRUE, 
         type = "lower", 
         order = "original", 
         addCoef.col = "black", 
         tl.col="black", 
         tl.srt = 0,
         diag = FALSE 
)

# Example LDA
library(MASS)
library(caret)

# Create LDA model with two predictors
mLDA <- lda(data = traindata, Emotion ~ F0_mean + F0_sd, method = "moment")

# Get predicted values from model for training data
predtrain <- predict(mLDA, newdata = traindata)

# Evaluate model performance
confusionMatrix(data = predtrain$class, reference = traindata$Emotion)

# Get predicted values from model for test data
predtest <- predict(mLDA, newdata = testdata)

# Evaluate model performance
confusionMatrix(data = predtest$class, reference = testdata$Emotion)

# Confusion matrix only
table(predtest$class, testdata$Emotion, dnn = c("Predicted", "Actual"))

# Create dataframe with model accuracy info
test.acc <- data.frame(numpred = c(2:7), 
                       accuracy = c(0.8503, 0.9456, 0.9592, 0.9592, 0.9592, 0.9762), 
                       lowerCI = c(0.8043, 0.9131, 0.9298, 0.9298, 0.9298, 0.9516), 
                       upperCI = c(0.8891, 0.9686, 0.9787, 0.9787, 0.9787, 0.9904) )

# FIGURE: Test set accuracy by number of predictors
ggplot(data = test.acc, aes(x = numpred, y = accuracy)) + 
  geom_point(size = 2) + 
  geom_line(size = 0.75) + 
  geom_ribbon(aes(ymin = lowerCI, ymax = upperCI), fill = 'lightskyblue', alpha=0.2) + 
  scale_x_continuous(name = "Number of predictors", limits = c(2, 7)) + 
  scale_y_continuous(name = "Test set accuracy", limits = c(0.8, 1.0), expand = c(0, 0)) + 
  theme_bw() + 
  theme(plot.margin = unit(c(0.5, 0.25, 0.25, 0.25), "cm")) + 
  theme(panel.background = element_rect(colour = 'black', size = 1)) + 
  theme(axis.title = element_text(colour = "black", size = 20), 
        axis.text = element_text(colour = "black", size = 16), 
        text = element_text(colour = "black", size = 16)) +
  theme(axis.title.y = element_text(vjust = +1.5)) +
  annotate("text", x=2.3, y=0.84, label = "F0 mean + F0 SD", colour = 'dodgerblue3', size = 5) +
  annotate("text", x=3, y=0.955, label = "+ Spectral CoG", colour = 'dodgerblue3', size = 5) +
  annotate("text", x=4, y=0.97, label = "+ Int mean", colour = 'dodgerblue3', size = 5) +
  annotate("text", x=5, y=0.97, label = "+ Duration", colour = 'dodgerblue3', size = 5) +
  annotate("text", x=6, y=0.97, label = "+ Int SD", colour = 'dodgerblue3', size = 5) +
  annotate("text", x=6.85, y=0.985, label = "+ F0 slope", colour = 'dodgerblue3', size = 5)
