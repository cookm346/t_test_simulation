a <- expand.grid(n2 = seq(10, 100, 10), 
                 sd2 = c(1/10, 1/4, 1/2, 1, 2, 4, 10))


monte_carlo <- function(params, n = 100, student = TRUE){
    r <- apply(a, 1, function(x){
        mean(replicate(n, t.test(rnorm(10, 0, 1), rnorm(x[1], 0, x[2]), var.equal = student)$p.value) < 0.05)
    })
    return(data.frame(a, 
                      t_test = ifelse(student, "Student", "Welch"), 
                      type_one_error = r))
}

d <- rbind(monte_carlo(a, n = 1000, student = TRUE),
           monte_carlo(a, n = 1000, student = FALSE))


library(ggplot2)

png(filename = "heatmaps.png", width = 13, height = 7, units = "in", res = 900)

ggplot(d, aes(as.factor(n2), as.factor(sd2), fill=type_one_error)) + geom_tile() + 
    ylab("SD of group 2") +
    xlab("N of group 2") +
    coord_flip() +
    geom_text(aes(label = round(type_one_error, 2))) +
    scale_fill_gradient(name = "Type 1 error", low = "white", high = "red") +
    facet_wrap(~t_test)

dev.off()
