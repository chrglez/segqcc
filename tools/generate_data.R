# Generate segqcc package datasets (run once, committed as .rda).
# Deterministic (seeded); explicit vector construction, no mapply.

# ---- X-bar variable process -----------------------------------------------
set.seed(20260925)
n_per  <- 8
seg_len <- c(120, 120, 160)              # samples per segment (total 400)
means   <- c(100.0, 101.5, 99.5)
sdv     <- rep(2.0, 3)
seg_of_j <- rep(1:3, seg_len)            # which segment each sample j belongs to
K <- length(seg_of_j)                     # 400
# One label per OBSERVATION: sample j is repeated n_per times so
# qcc.groups() aggregates the right observations per sample. Using seq_len(K)
# here would be recycled across the K*n_per rows and scramble the labels.
xbar_sample <- rep(seq_len(K), each = n_per)
xbar_value  <- as.numeric(mapply(function(j) rnorm(n_per, means[seg_of_j[j]], sdv[seg_of_j[j]]),
                                 seq_len(K)))
segxbar <- data.frame(value = xbar_value, sample = xbar_sample)
stopifnot(nrow(segxbar) == K * n_per, length(unique(xbar_sample)) == K, min(xbar_sample) == 1)

# ---- p / np attribute process ---------------------------------------------
set.seed(777)
Kp       <- 300
sizes    <- rep(50, Kp)
p_true   <- c(rep(0.05, 150), rep(0.09, 150))    # defect-rate shift at 150
defectives <- rbinom(Kp, sizes, p_true)   # one Binomial(n=50, p) count per sample
segp <- data.frame(value = defectives, sizes = sizes)
stopifnot(nrow(segp) == Kp)

# ---- c-chart Poisson defect process ---------------------------------------
set.seed(4242)
c_true <- c(rep(3.0, 150), rep(5.0, 150))          # defect-rate shift at 150
segc <- data.frame(value = rpois(300, c_true))
stopifnot(nrow(segc) == 300)

# ---- individuals process (one observation per sample) ----------------------
# Matches the shape of the original motivating example: 400 samples of size 1,
# where the within-sample sigma of an xbar chart does not exist and the chart
# has to be the individuals (xbar.one) one.
set.seed(31415)
ind_seg_len <- c(150, 100, 150)                  # total 400
ind_means   <- c(50.0, 53.0, 51.0)
segind <- data.frame(
  value = rnorm(sum(ind_seg_len), rep(ind_means, ind_seg_len), 1.5)
)
stopifnot(nrow(segind) == 400)

dir.create("data", showWarnings = FALSE)
save(segxbar, file = "data/segxbar.rda", version = 2)
save(segp,    file = "data/segp.rda",     version = 2)
save(segc,    file = "data/segc.rda",     version = 2)
save(segind,  file = "data/segind.rda",   version = 2)
cat("written:", list.files("data"), "\n")
cat("segind:", nrow(segind), "rows\n")
cat("segxbar:", nrow(segxbar), "rows, ", length(unique(segxbar$sample)), "samples\n")
cat("segp:", nrow(segp), "rows\n")
cat("segc:", nrow(segc), "rows\n")
