
#' Einkommenssteuer berechnen
#'
#'
#' \deqn{
#' Einkommensteuer(zve)=
#'\begin{cases}
#'0 & zve\leq gfb\\
#'pg1s(zve-gfb)+\frac{pg2s-pg1s}{2(pg1e-gfb)}(zve-gfb)^2 & gfb<zve\leq pg1e\\
#'pg2s(zve-pg1e)+\frac{pp1s-pg2s}{2(pg2e-pg1e)}(zve-pg1e)^2 +
#'  pg1s(pg1e-gfb)+\frac{pg2s-pg1s}{2(pg1e-gfb)}(pg1e-gfb)^2 & pg1e<zve\leq pg2e\\
#'pp1s(zve-pg2e)+pg2s(zve-pg1e)+\frac{pp1s-pg2s}{2(pg2e-pg1e)}(zve-pg1e)^2 + pg1s(pg1e-gfb)+\frac{pg2s-pg1s}{2(pg1e-gfb)}(pg1e-gfb)^2 & pg1e<zve\leq pg2e\\
#'pp2s(zve-pp1e)+pp1s(pp1e-pg2e)+pg2s(zve-pg1e)+\frac{pp1s-pg2s}{2(pg2e-pg1e)}(zve-pg1e)^2 + pg1s(pg1e-gfb)+\frac{pg2s-pg1s}{2(pg1e-gfb)}(pg1e-gfb)^2 & pg1e<zve\leq pg2e\\
#'\end{cases}}
#'
#'
#' @param zve Zu versteuerndes Einkommen
#' @param gfb Grundfreibetrags
#' @param pg1s Startsatz erste Progressionszone
#' @param pg1e Ende erste Progressionszone
#' @param pg2s Startsatz zweite Progressionszone
#' @param pg2e Ende zweite Progressionszone
#' @param pp1s Satz erste Proportionalzone, Spitzensteuersatz
#' @param pp1e Ende erste Proportionalzone
#' @param pp2s Seite zweite Proportionalzone, Reichensteuersatz
#'
#' @returns value
#' @export
#'
#' @examples
#' compute_Einkommensteuer(30000)
compute_Einkommensteuer <- function(zve, gfb = 11604, pg1s = 14, pg1e = 17005, pg2s = 23.97,
                             pg2e = 66760, pp1s = 42, pp1e = 277826, pp2s = 45) {
  # Precompute constants
  C1 <- pg1s * (pg1e - gfb) + ((pg2s - pg1s) / (2 * (pg1e - gfb))) * (pg1e - gfb)^2
  C2 <- C1 + pg2s * (pg2e - pg1e) + ((pp1s - pg2s) / (2 * (pg2e - pg1e))) * (pg2e - pg1e)^2
  C3 <- C2 + pp1s * (pp1e - pg2e)

  # Initialize tax vector
  tax <- numeric(length(zve))

  # Case 1: zve <= gfb
  idx1 <- zve <= gfb
  tax[idx1] <- 0

  # Case 2: gfb < zve <= pg1e
  idx2 <- zve > gfb & zve <= pg1e
  tax[idx2] <- pg1s * (zve[idx2] - gfb) +
    ((pg2s - pg1s) / (2 * (pg1e - gfb))) * (zve[idx2] - gfb)^2

  # Case 3: pg1e < zve <= pg2e
  idx3 <- zve > pg1e & zve <= pg2e
  tax[idx3] <- C1 + pg2s * (zve[idx3] - pg1e) +
    ((pp1s - pg2s) / (2 * (pg2e - pg1e))) * (zve[idx3] - pg1e)^2

  # Case 4: pg2e < zve <= pp1e
  idx4 <- zve > pg2e & zve <= pp1e
  tax[idx4] <- C2 + pp1s * (zve[idx4] - pg2e)

  # Case 5: zve > pp1e
  idx5 <- zve > pp1e
  tax[idx5] <- C3 + pp2s * (zve[idx5] - pp1e)

  return(tax / 100)
}
