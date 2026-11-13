# bench_audit.R - Benchmark audit of dplyr critical operations
library(dplyr)
library(bench)

cat("=== Gerando dados para Benchmark ===\n")
set.seed(42)
N <- 2e6 # 2 milhões de linhas para benchmark rápido e estável
K <- 50000 # 50 mil grupos

cat(sprintf("N = %d linhas, K = %d grupos\n", N, K))

df_main <- tibble(
  id = sample(sprintf("grp_%06d", 1:K), N, replace = TRUE),
  val1 = rnorm(N),
  val2 = rnorm(N),
  cat_var = sample(letters[1:10], N, replace = TRUE)
)

df_join_right <- tibble(
  id = sample(sprintf("grp_%06d", 1:K), K * 2, replace = TRUE),
  aux = rnorm(K * 2)
)

cat("=== 1. group_by() + summarise() com multiplas funcoes e across() ===\n")
bm_summarise <- bench::mark(
  iterations = 3,
  check = FALSE,
  filter_gc = FALSE,
  across = {
    df_main |>
      group_by(id) |>
      summarise(
        across(c(val1, val2), list(mean = mean, sum = sum)),
        .groups = "drop"
      )
  },
  direct = {
    df_main |>
      group_by(id) |>
      summarise(
        val1_mean = mean(val1),
        val2_sum = sum(val2),
        .groups = "drop"
      )
  }
)
print(bm_summarise[, c("expression", "min", "median", "itr/sec", "mem_alloc", "n_gc")])

cat("\n=== 2. left_join() e inner_join() com chaves de caracteres e many-to-many ===\n")
bm_joins <- bench::mark(
  iterations = 3,
  check = FALSE,
  filter_gc = FALSE,
  left_join = {
    left_join(df_main[1:500000, ], df_join_right[1:100000, ], by = "id", relationship = "many-to-many")
  },
  inner_join = {
    inner_join(df_main[1:500000, ], df_join_right[1:100000, ], by = "id", relationship = "many-to-many")
  }
)
print(bm_joins[, c("expression", "min", "median", "itr/sec", "mem_alloc", "n_gc")])

cat("\n=== 3. mutate() com vetores de tamanho dinamico e avaliacao tidy eval (rlang) ===\n")
col_sym1 <- rlang::sym("val1")
col_sym2 <- rlang::sym("val2")

bm_mutate <- bench::mark(
  iterations = 3,
  check = FALSE,
  filter_gc = FALSE,
  mutate_tidy_eval = {
    df_main |>
      mutate(
        out1 = !!col_sym1 * 2,
        out2 = !!col_sym1 + !!col_sym2,
        out3 = if_else(val1 > 0, "pos", "neg")
      )
  },
  mutate_grouped = {
    df_main[1:500000, ] |>
      group_by(cat_var) |>
      mutate(
        out1 = val1 - mean(val1),
        out2 = cumsum(val2)
      ) |>
      ungroup()
  }
)
print(bm_mutate[, c("expression", "min", "median", "itr/sec", "mem_alloc", "n_gc")])

cat("\n=== Benchmark Concluido ===\n")
