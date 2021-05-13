
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gklfs

<!-- badges: start -->

<!-- badges: end -->

“gklfs” paketi ile Hanehalkı İşgücü Araştırması veri setinin, kullanıcı
tarafından belirlenen kalibrasyon kriterlerine göre kalibre edilerek
nihai ağırlıkların hesaplamaları yapılmaktadır.

Pakette 3 adet fonksiyon bulunmaktadır;  
1\. “gk\_arrange” : Veri seti düzenlenerek kalibrasyon ve trimming
işlemlerine hazır hale getirilmektedir.  
2\. “gk\_check” : Veri setinde eksik veya yanlış girilen verilen
kontrolü yapılarak TRUE veya FALSE çıktısı vermektedir.  
3\. “gk\_caltrim” : Kullanıcı tarafından belirlenen kriterlere göre
kalibrasyon ve trimming işlemleri yapılmaktadır.

## Paketi Yükleme

Paketin güncel versiyonunu [GitHub](https://github.com/) üzerinden
aşağıdaki komutla indirebilirsiniz:

``` r
# install.packages("devtools")
devtools::install_github("gokhankocturk/gklfs")
```

## Örnek 1:

data\_arranged \<- gk\_arrange(data,  
proj\_yascins,  
proj\_il,  
proj\_nutskirkent,  
proj\_hhbkirkent,  
cinsiyet = “CINSIYET”,  
birimno = “BIRIMNO”,  
yas = “YAS”,  
hhbsay = 9,  
hhbkirkent = TRUE)

## Örnek 2:

gk\_check(data\_arranged)

## Örnek 3:

data\_calibrated \<- gk\_caltrim(data,  
calyap = TRUE,  
calsay = 8,  
trimyap = TRUE,  
trimsay = 4,  
hhbsay = 9,  
hhbkirkent = TRUE,  
yascinsiyet = TRUE,  
il = TRUE,  
nutskirkent = TRUE,  
kirnufus = 9000000,  
kentnufus = 70000000)
