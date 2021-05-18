#' Title
#'
#' @param data "gk_caltrim" fonksiyonu sonrasinda elde edilen ozet veri setinin kullanilmasi gerekmektedir.
#' @param yas Yas degiskeni
#' @param cinsiyet Cinsiyet degiskeni
#' @param faktor Kalibrasyon ve trimming sonrasi elde edilen nihai agirliklar.
#' @param duzfakto Cevapsizlik duzeltmeleri sonrasinda kalibrasyona girecek agirliklar.
#' @param ibbs2  IBBS2 degiskeni
#' @param kirkent Kirkent degiskeni
#' @param stratum Bloklarin (2 veya 3 blok) birlestirilmesiyle olusturulmus yeni tabakalar
#' @param ilkayitno Ilkayitno degiskeni
#' @param calmodel Kalibrasyon ve trimmingde kullandigimiz model. Ornegin "KIR_KENT:IBBS_2 + YASGR:CINSIYET + HHB - 1"
#' @param varyans Toplama iliskin varyans hesabi icin "Total", ortalamaya iliskin varyans hesabi icin "Mean" degerini alir.
#'
#' @return Varyans hesaplamalari sonucunda IBBS2 bazinda elde edilen sonuclari verir.
#' @export
#'
#' @examples
gk_variance <- function(data,
                        yas = "fk_yas",
                        cinsiyet = "fk_cinsi",
                        faktor = "w",
                        duzfakto = "duzfakto",
                        ibbs2 = "IBBS_2",
                        kirkent = "KIR_KENT",
                        stratum = "BLOKNO",  # Burasi normalde STRATUM olacak, simdilik veri setinde yok.
                        ilkayitno = "ILKAYITNO",
                        calmodel = "KIR_KENT:IBBS_2 - 1",
                        varyans = "Total"
) {
  veri <- data
  veri$iss_yeni <- NA
  veri$iss_yeni <- as.numeric(veri$iss_yeni)
  veri <- veri[DURUM == 2, iss_yeni := 1]
  veri <- veri[DURUM == 1, iss_yeni := 0]
  veri <- veri[DURUM == 3, iss_yeni := 0]

  veri <- veri %>% filter(!is.na(iss_yeni)) %>% filter(!!sym(yas) >= 15 & !!sym(yas) <= 74)

  veri[[faktor]] <- as.numeric(veri[[faktor]])
  veri[[duzfakto]] <- as.numeric(veri[[duzfakto]])
  veri[[ibbs2]] <- as.factor(veri[[ibbs2]])
  veri[[kirkent]] <- as.factor(veri[[kirkent]])
  veri[[stratum]] <- as.factor(veri[[stratum]])
  veri[[cinsiyet]] <- as.factor(veri[[cinsiyet]])
  veri[[ilkayitno]] <- as.factor(veri[[ilkayitno]])

  contrasts.off()

  # Asagidaki fonksiyonda "strata" argumanina veri setinde yer alacak "STRATUM" sutununun girilmesi gerekiyor.
  # Bana gonderilen veride bu sutun olmadigi icin programin calisip calismadigina bakmak icin rasgele ILKAYITNO
  # sutununu kullandim.

  # Ayrica "ids" argumanina "BLOKNO" ya da "BLOKNO + BIRIMNO" sutunlarinin girilmesi gerekiyor. Veri setinde
  # simdilik STRATUM sutunu olmadigi icin, en basta fonksiyon tanimlarken simdilik idarelik "stratum = BLOKNO"
  # girmistim. "ids" argumaninda da BLOKNO kullanilmasi gerektigi icin burada "ids = ~stratum" kullandim.

  # Yukarida bahsettigim 2 konunun gercek calisma icin hem burada hem de fonksiyon argumanlarinda duzeltilmesi gerekir.
  eval(parse(text = paste0(
    "hia_ext <- ext.calibrated(data = veri,
                            strata = ~", ilkayitno, ",
                            calmodel = ~", calmodel, ",
                            ids = ~", stratum, ",
                            weights = ~", duzfakto, ",
                            weights.cal = ~", faktor, ")"

  )))

  eval(parse(text = paste0(
    "sonuclar <- svystatTM(hia_ext,
                         y = ~iss_yeni,
                         ~IBBS_2,
                         estimator = c('", varyans, "'),
                         vartype = c('se', 'cv', 'cvpct', 'var'),
                         conf.int = TRUE,
                         conf.lev = 0.95,
                         deff = TRUE)"
  )))


  return(sonuclar)
}
