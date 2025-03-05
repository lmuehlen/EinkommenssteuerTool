
#' Grenzsteuersatz berechnen
#'
#' *compute_Grenzsteuersatz* berechnet den Grenzsteuersatz bei einem zu versteuernden Einkommen von *zve* basierend auf den gegebenen Eckwerten der Grenzsteuer.
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
#' @returns number
#' @export
#'
#' @examples compute_Grenzsteuersatz(30000)
compute_Grenzsteuersatz <- function(zve, gfb=11604, pg1s=14, pg1e=17005, pg2s=23.97,
                            pg2e=66760, pp1s=42, pp1e=277826, pp2s=45) {
  mtr <- numeric(length(zve))

  mtr[zve <= gfb] <- 0
  idx <- zve > gfb & zve <= pg1e
  mtr[idx] <- pg1s + ((pg2s - pg1s) / (pg1e - gfb)) * (zve[idx] - gfb)

  idx <- zve > pg1e & zve <= pg2e
  mtr[idx] <- pg2s + ((pp1s - pg2s) / (pg2e - pg1e)) * (zve[idx] - pg1e)

  mtr[zve > pg2e & zve <= pp1e] <- pp1s
  mtr[zve > pp1e] <- pp2s

  return(mtr)
}
