#' CV, Standart Hata, Deff Hesaplamalari
#'
#'
#' @param data "gk_caltrim" fonksiyonu sonrasinda elde edilen ozet veri setinin kullanilmasi gerekmektedir.
#' @param yas Yas degiskeni
#' @param cinsiyet Cinsiyet degiskeni
#' @param faktor Kalibrasyon ve trimming sonrasi elde edilen nihai agirliklar.
#' @param duzfakto Cevapsizlik duzeltmeleri sonrasinda kalibrasyona giren agirliklar.
#' @param ibbs2  IBBS2 degiskeni
#' @param kirkent Kirkent degiskeni
#' @param stratum Bloklarin (2 veya 3 blok) birlestirilmesiyle olusturulmus yeni tabakalar
#' @param ilkayitno Ilkayitno degiskeni
#' @param calmodel Kalibrasyon ve trimmingde kullandigimiz model. \cr
#' \cr
#' Ornegin "KIR_KENT:IBBS_2 + YASGR:CINSIYET + HHB - 1" \cr
#' \cr
#' Burada tanimlanan modele gore standart hata degerleri ve ona bagli olarak da CV ve deff degerleri az da olsa
#' degismektedir. Bu sebeple, modelin dogru tanimlanmasi onemlidir. \cr
#' "ibbs2" argumani ile hangi duzeyde varyans hesaplamak istedigimizi belirtiyoruz. Dolayisiyla "ibbs2" argumani ile
#' "calmodel" argumaninda tanimlanan modelin birbirine uyumlu olmasi gerekmektedir. \cr
#' Ornegin; "ibbs2 = IBBS_1" yazarak IBBS1 duzeyinde varyans hesaplamak istedigimizi belirtirken modeli de
#' "calmodel="KIR_KENT:IBBS_1-1" seklinde, yani ayni IBBS duzeyinde kurmaliyiz. IBBS duzeyimiz ve modelimiz
#' farkli olursa hata verecektir.
#' @param varyans Toplama iliskin varyans hesabi icin "Total", ortalamaya iliskin varyans hesabi icin "Mean" degerini alir.
#'
#' @return IBBS2 bazinda toplam, ortalama, CV, standart hata, guven araliklari ve deff degerlerini cikti olarak verir.
#' @export
#' @import ReGenesees
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
  # "check()" fonkiyonunu calistirdigimizda DURUM, iss_yeni ve sonuclar degiskenleri icin
  # "no visible binding for global variable 'DURUM'" seklinde bir uyari veriyor. Bu durumu duzeltmek icin
  # internette 2 farkli yol oneriliyor.

  # Birincisi : Asagidaki gibi fonksiyonun basinda butun degiskenlere NULL degerini atamak
  # Ikincisi : utils::globalVariables(c("DURUM", "iss_yeni", "sonuclar")) komutunu yazmak.

  # "gk_variance" fonksiyonumuzda sorun yaratan sadece bu 3 degisken oldugu icin 2 yontemi de uygulayabiliriz.
  # Fakat diger "gk_arrange", "gk_caltrim" gibi fonksiyonlarimizin icerisinde olusturdugumuz "D_urb_01", "D_urb_02", ... gibi
  # yuzlerce degisken oldugu icin burada aciklanan 2 yontemden herhangi birini uygulamak cok zaman alacak o yuzden
  # diger fonksiyonlarda, asagidaki gibi NULL atama islemini yapmadim.
  # Bunun nasil cozuleceginin arastirilmasi gerekiyor.
  DURUM <- iss_yeni <- sonuclar <- NULL


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
                         ~", ibbs2, ",",
                         "estimator = c('", varyans, "'),
                         vartype = c('se', 'cv', 'cvpct', 'var'),
                         conf.int = TRUE,
                         conf.lev = 0.95,
                         deff = TRUE)"
  )))


  return(sonuclar)
}
