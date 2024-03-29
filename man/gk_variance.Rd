% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gk_variance.R
\name{gk_variance}
\alias{gk_variance}
\title{CV, Standart Hata, Deff Hesaplamalari}
\usage{
gk_variance(
  data,
  yas = "fk_yas",
  cinsiyet = "fk_cinsi",
  faktor = "w",
  duzfakto = "duzfakto",
  ibbs2 = "IBBS_2",
  kirkent = "KIR_KENT",
  stratum = "BLOKNO",
  ilkayitno = "ILKAYITNO",
  calmodel = "KIR_KENT:IBBS_2 - 1",
  varyans = "Total"
)
}
\arguments{
\item{data}{"gk_caltrim" fonksiyonu sonrasinda elde edilen ozet veri setinin kullanilmasi gerekmektedir.}

\item{yas}{Yas degiskeni}

\item{cinsiyet}{Cinsiyet degiskeni}

\item{faktor}{Kalibrasyon ve trimming sonrasi elde edilen nihai agirliklar.}

\item{duzfakto}{Cevapsizlik duzeltmeleri sonrasinda kalibrasyona giren agirliklar.}

\item{ibbs2}{IBBS2 degiskeni}

\item{kirkent}{Kirkent degiskeni}

\item{stratum}{Bloklarin (2 veya 3 blok) birlestirilmesiyle olusturulmus yeni tabakalar}

\item{ilkayitno}{Ilkayitno degiskeni}

\item{calmodel}{Kalibrasyon ve trimmingde kullandigimiz model. \cr
\cr
Ornegin "KIR_KENT:IBBS_2 + YASGR:CINSIYET + HHB - 1" \cr
\cr
Burada tanimlanan modele gore standart hata degerleri ve ona bagli olarak da CV ve deff degerleri az da olsa
degismektedir. Bu sebeple, modelin dogru tanimlanmasi onemlidir. \cr
"ibbs2" argumani ile hangi duzeyde varyans hesaplamak istedigimizi belirtiyoruz. Dolayisiyla "ibbs2" argumani ile
"calmodel" argumaninda tanimlanan modelin birbirine uyumlu olmasi gerekmektedir. \cr
Ornegin; "ibbs2 = IBBS_1" yazarak IBBS1 duzeyinde varyans hesaplamak istedigimizi belirtirken modeli de
"calmodel="KIR_KENT:IBBS_1-1" seklinde, yani ayni IBBS duzeyinde kurmaliyiz. IBBS duzeyimiz ve modelimiz
farkli olursa hata verecektir.}

\item{varyans}{Toplama iliskin varyans hesabi icin "Total", ortalamaya iliskin varyans hesabi icin "Mean" degerini alir.}
}
\value{
IBBS2 bazinda toplam, ortalama, CV, standart hata, guven araliklari ve deff degerlerini cikti olarak verir.
}
\description{
CV, Standart Hata, Deff Hesaplamalari
}
