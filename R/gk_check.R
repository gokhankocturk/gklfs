#' Veri Setinin Kontrolu
#'
#' @param data Check edilecek veri seti
#'
#' @return Kontrol sonrasi TRUE veya FALSE ciktisini verir

#' @examples
#' control <- gk_check(mtcars);
#' if(gk_check(mtcars)) {x <- 999};
#' @export

gk_check <- function(data) {
  aa <- bb <- cc <- dd <- ee <- FALSE
  if(any(data$duzfakto <= 0)) {
    warning("HATA --- DUZFAKTO <= 0 olan degerler var...")
  }
  else {
    message("OK --- DUZFAKTO'larin hepsi 0'dan buyuk...")
    aa <- TRUE
  }

  if(any(is.na(data$fk_cinsi))){
    warning("HATA --- CINSIYET degiskeni bos olan gozler var...")
  }
  else {
    message("OK --- CINSIYET sutununun hepsi dolu...")
    bb <- TRUE
  }

  if(any(is.na(data$ILKAYITNO))){
    warning("HATA --- ILKAYITNO degiskeni bos olan gozler var...")
  }
  else {
    message("OK --- ILKAYITNO sutununun hepsi dolu...")
    cc <- TRUE
  }

  if(any(is.na(data$nur))){
    warning("HATA --- NUTS2 - KIRKENT (nur) degiskeni bos olan gozler var...")
  }
  else {
    message("OK --- NUTS2 - KIRKENT (nur) sutununun hepsi dolu...")
    dd <- TRUE
  }

  if(any(is.na(data$hhb_kirkent))){
    warning("HATA --- HHB - KIRKENT (hhb_kirkent) degiskeni bos olan gozler var...")
  }
  else {
    message("OK --- HHB_KIRKENT (hhb_kirkent) sutununun hepsi dolu...")
    ee <- TRUE
  }

  if(aa && bb && cc && dd && ee) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }

}
