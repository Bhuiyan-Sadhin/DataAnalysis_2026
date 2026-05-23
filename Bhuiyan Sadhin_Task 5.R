install.packages("vegan")
library(vegan)
# Read dataset
data <- read.table("/Users/apple/Downloads/INRTU courses/2nd semester/Data Analysis/Homework 5/data.txt", header = TRUE, sep = "\t", check.names = FALSE)
data
rownames(data) <- data[, 1]
data <- data[, -1]

# STEP 1: NMDS with Bray-Curtis distance

set.seed(42)
ord <- metaMDS(data, distance = "bray", trymax = 100)

cat("NMDS stress:", round(ord$stress, 4), "\n")


# STEP 2: Fit significant species vectors (envfit, p <= 0.05)

set.seed(42)
fit_sp <- envfit(ord, data, perm = 999)

# All species results (r2 and p-value)
print(fit_sp)
print(fit_sp, p.max = 0.05)

# STEP 3: UPGMA hierarchical clustering with Bray-Curtis

d_bray <- vegdist(data, method = "bray")
fit_clust <- hclust(d_bray, method = "average")  # UPGMA

# Plot dendrogram
plot(fit_clust, hang = -1,
     main = "UPGMA Dendrogram (Bray-Curtis)",
     xlab = "Sites", sub = "", ylab = "Bray-Curtis dissimilarity")

# STEP 4: Cut dendrogram into 2-3 clusters
k <- 3
clusters <- cutree(fit_clust, k = k)

# Safety fallback: reduce to k = 2 if any cluster has only 1 site
if (any(table(clusters) < 2)) {
  k <- 2
  clusters <- cutree(fit_clust, k = k)
  message("Reduced to k = 2: one cluster had only 1 site.")
}

print(clusters)
print(table(clusters))

# STEP 5: NMDS plot
palette_cols <- c("#E41A1C", "#377EB8", "#4DAF4A")  # red, blue, green
site_cols    <- palette_cols[clusters]

plot(ord, type = "n",
     main = "NMDS ordination (Bray-Curtis) with UPGMA clusters")

# 5a. Shaded confidence ellipses (1 SD) per cluster
ordiellipse(ord, groups = clusters, kind = "sd", lwd = 2,
            col    = palette_cols[1:k],
            draw   = "polygon",
            alpha  = 40,
            border = palette_cols[1:k])

# 5b. Site points coloured by cluster
points(ord, display = "sites", pch = 21, cex = 2.5, lwd = 2,
       bg  = adjustcolor(site_cols, alpha.f = 0.8),
       col = site_cols)

# 5c. Significant species arrows only (p <= 0.05)
plot(fit_sp, p.max = 0.05, col = "darkgreen", cex = 0.85, add = TRUE)

# 5d. Non-overlapping site labels
orditorp(ord, display = "sites", cex = 0.85, col = "black",
         priority = rowSums(data), air = 1.2)

# Legend
legend("topright",
       legend = paste("Cluster", 1:k),
       pch    = 21,
       pt.bg  = palette_cols[1:k],
       col    = palette_cols[1:k],
       pt.cex = 1.8, bty = "n")

# STEP 6: PERMANOVA — test differences between clusters
site_df <- data.frame(cluster = factor(clusters))

set.seed(42)
perm_result <- adonis2(data ~ cluster,
                       data         = site_df,
                       method       = "bray",
                       permutations = 999)

print(perm_result)

# Extract key statistics
R2 <- round(perm_result$R2[1], 4)
pv <- perm_result$`Pr(>F)`[1]
