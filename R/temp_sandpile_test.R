# A temporary file used in improving sandpile()

# test <- data.frame(
#     n = c(1:5, 50, 100, 500, 1000, 5000, 10000, 20000, 30000),
#     time_old = 0,
#     time_new = 0,
#     identical = NA
# )
#
# for (i in 1:nrow(test)) {
#     test$time_old[i] <- system.time({
#         old <- sandpile_old(test$n[i], NULL)
#     })["elapsed"]
#     test$time_new[i] <- system.time({
#         new <- sandpile(test$n[i], NULL)
#     })["elapsed"]
#     test$identical[i] <- identical(old, new)
# }
#
# test$time_diff <- test$time_new - test$time_old
# test

#
# sandpile(10000)
#
# # Die Zeilen mit setdiff() und intersect() sind zu langsam.
#
#
# x <- 1:100
# y <- 90:100
# upper_bound <- min(y)
#
# system.time({
#     for (i in 1:10000) {
#         foo <- setdiff(x, y)
#     }
# })
#
# system.time({
#     for (i in 1:10000) {
#         foo <- x[x < upper_bound]
#     }
# })
#
# system.time({
#     for (i in 1:10000) {
#         x[!x %in% y]
#     }
# })
