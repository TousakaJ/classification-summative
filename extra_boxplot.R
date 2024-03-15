datax = c(0.904, 0.9852, 0.9862, 0.9872, 0.9578, 0.9818, 0.9882)
namex = c(
    "Baseline", "Cart", "Cart-Pruned",
    "Random Forest", "Logistic", "Boost", "Final Model"
)
plot1 = barplot(datax, main = "Model Performance Comparation", beside = TRUE, legend = TRUE, col = c("#6767fa", "#ffe224", "#24ffed", "#24ff50", "#1317fa", "#a7ff24", "#ff5e24"), ylim = c(0.85, 1))

legend("bottomright", legend = namex, fill = c("#6767fa", "#ffe224", "#24ffed", "#24ff50", "#1317fa", "#a7ff24", "#ff5e24"))
text(plot1, y = 0.99, format(datax), col="black", cex=2)