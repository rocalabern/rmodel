#' @title r.clusters.rearrage
#' @export
r.clusters.rearrage <- function (real, model) {
  values = unique(real)
  n = length(values)
  for (i in 2:n) {
    for (j in 1:(i-1)) {
      scoreAct = sum(as.integer(real==values[i])*as.integer(model==values[i]))
      scoreNew = sum(as.integer(real==values[i])*as.integer(model==values[j]))
      if (scoreNew > scoreAct) {
        indi = model==values[i]
        indj = model==values[j]
        model[indi] = values[j]
        model[indj] = values[i]
      }
    }
  }
}

#' @title r.clusters.kmeans
#' @export
r.clusters.kmeans <- function (data, k) {
  result = kmeans(data, k)
  return (list(result=result, centers=result$centers, clusters=result$cluster))
}

#' @title r.clusters.pam
#' @export
r.clusters.pam <- function (data, k) {
  result = cluster::pam(data, k)
  return (list(result=result, centers=result$medoids, clusters=result$clustering))
}

#' @title r.clusters.silhouette
#' @export
r.clusters.silhouette <- function (data, clusters, dist = dist(data, "euclidean")) {
  result = cluster::silhouette(data, dist)
  return (result)
}