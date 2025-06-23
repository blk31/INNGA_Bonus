# heuristic solution
# weight_per_value <- weights / values
# volume_per_value <- volume / values
# X <- cbind(values, weights, weight_per_value, volume, volume_per_value)
# colnames(X) <- c("value", "weight", "weight/value", "volume", "volume/value")
# print(X)
# X_weight_volume <- X[order(X[, "weight/value"], X[, "volume/value"]), ]
# print(X_weight_volume)

# weight_packed <- 0
# volume_packed <- 0
# value_packed <- 0
# while ((weight_packed < weight_limit) & (volume_packed < volume_limit)) {
#     value_packed <- value_packed + X_weight_volume[1,1]
#     weight_packed <- weight_packed + X_weight_volume[1, 2]
#     volume_packed <- volume_packed + X_weight_volume[1, 4]
#     X_weight_volume <- X_weight_volume[-1, ]
# }
# cat("Value: ", value_packed, "Volume: ", volume_packed, "Weight: ", weight_packed)