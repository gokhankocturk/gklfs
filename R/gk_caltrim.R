#' Kalibrasyon & Trim
#'
#'
#' @param data Duzenlenmis, dummyleri olusturulmus, projekte oranlari eklenmis veri seti.
#' gk_arrange" fonksiyonundan elde edilen veri seti burada kullanilmalidir.
#' @param calyap Kalibrasyon yapilacaksa TRUE, yapilmayacaksa FALSE degerini alir.
#' @param calsay Kalibrasyon dongu sayisi
#' @param trimyap Trim yapilacaksa TRUE, yapilmayacaksa FALSE degerini alir.
#' @param trimsay Trim dongu sayisi
#' @param hhbsay Hanehalki buyuklugune gore olusturulacak grup sayisi. Ornegin
#' "hhbsay = 4" girildiginde hanehalki buyuklugu gruplari 1, 2, 3, 4+ seklinde olusacaktir.
#' gk_arrange" fonksiyonunda da ayni arguman bulunuyor. Ikisi birbirinden farkli
#' olduğunda dummy uyusmazligi nedeniyle program hata verecektir. O yuzden veri setinin duzenlenmesi (gk_arrange)
#' ve agirliklandirilmasi (gk_caltrim) asamalarinda ayni degeri almalidir.
#' @param hhbkirkent HHB * KIRKENT crossunda kalibrasyon yapilmasi isteniyorsa TRUE,
#' sadece HHB bazinda kalibrasyon yapilmasi isteniyorsa FALSE degerini alir. "gk_arrange"
#' fonksiyonunda da ayni arguman bulunuyor. Ikisi birbirinden farkli olduğunda dummy uyusmazligi
#' nedeniyle program hata verecektir. Ornegin "gk_arrange" fonksiyonu icerisinde "hhbkirkent = TRUE"
#' olarak tanimlandiginda dummyler "D_hhb_kk_1_2" seklinde olusturulurken "gk_caltrim" fonksiyonu
#' icerisinde "hhbkirkent = FALSE" olarak tanimlanirsa dummyler "D_hhb_1" seklinde olusacagi icin
#' program hata verecektir. O yuzden veri setinin duzenlenmesi (gk_arrange)
#' ve agirliklandirilmasi (gk_caltrim) asamalarinda ayni degeri almalidir.
#' @param yascinsiyet YAS * CINSIYET'e gore kalibrasyon yapilmasi isteniyorsa TRUE,
#' YAS * CINSIYET'e gore kalibrasyon yapilmasi istenmiyorsa FALSE degerini alir.
#' @param il IL'e gore kalibrasyon yapilmasi isteniyorsa TRUE, IL'e gore kalibrasyon
#' yapilmasi istenmiyorsa FALSE degerini alir.
#' @param nutskirkent NUTS2 * KIRKENT crossunda kalibrasyon yapilmasi isteniyorsa TRUE,
#' NUTS2 * KIRKENT crossunda kalibrasyon yapilmasi istenmiyorsa FALSE degerini alir.
#' @param kirnufus Projekte KIR toplam nufusu
#' @param kentnufus Projekte KENT toplam nufusu
#'
#' @return Kalibrasyon sonrasi elde edilen agirliklarla birlikte temel degiskenlerin yer
#' aldigi veri setini cikti olarak verir. Bu veri seti uzerinden issizlik, istihdam oranlari
#' hesaplanabilir.
#' @export
#'
#' @examples


gk_caltrim <- function(data,
                       proj_yascins,
                       proj_il,
                       proj_nutskirkent,
                       proj_hhbkirkent,
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
                       kentnufus = 70000000
) {

  pers_h <- as.data.table(data)
  pers_h[ , w := duzfakto]
  pers_h[ , w_old := w]

  # Fonksiyonda "calyap" argumani TRUE olarak gelecek. Eger kullanici bunu FALSE yaparsa
  # asagidaki dongulerin hicbirine girmeyecegi icin program calismayacak. Dolayisiyla bu
  # argumaninin kullanici tarafindan degistirilmemesi gerekiyor.
  if(calyap == TRUE){

    # HHB - Kirkent Kalibrasyonu

    # ONEMLI !!!!!
    # "gk_arrange" fonksiyonunda yer alan "hhbsay" argumaniyla veri setinin kac hhb grubuna gore
    # olusturulacagini kullanici belirliyordu. Burada kalibrasyon ve trim islemlerine girmeden once de
    # ayni "hhbsay" degerinin girilmesi gerekiyor. "gk_arrange" fonksiyonunda "hhbsay = 6",
    # "gk_caltrim" fonksiyonunda "hhbsay = 3" girecek olursak veri seti ve komutlar uyusmayacagi icin
    # hata verecektir.

    # Ayni sey "hhbkirkent" argumani icin de gecerli. "gk_arrange" fonksiyonunda "hhbkirkent = TRUE" girip,
    # "gk_caltrim" fonksiyonunda "hhbkirkent = FALSE" girilecek olursa veri seti ve komutlar uyusmayacagi
    # icin hata verecektir.

    # Ikinci for dongusunde gorulecegi uzere dongu 1'den kullanicinin belirledigi HHB grup sayisina kadar
    # ilerliyor. Dolayisiyla kullanici tarafindan belirtilen sayida gruba gore kodlar kendiliginden olusuyor.
    hhb_girilen <- hhbsay

    if(hhbkirkent == TRUE) {
      for(kirkent_kategori in 1:2) {
        for(hhb_kategori in 1:hhb_girilen) {
          eval(parse(text = paste0("pers_h[, p_hhb_kk_", kirkent_kategori, "_", hhb_kategori,
                                   ":= weighted.mean(D_hhb_kk_", kirkent_kategori, "_", hhb_kategori, ", w=w)]")))
          eval(parse(text = paste0("pers_h[, a_hhb_kk_", kirkent_kategori, "_", hhb_kategori,
                                   ":= weighted.mean(D_hhb_kk_", kirkent_kategori, "_", hhb_kategori, ", w=w)]")))
          eval(parse(text = paste0("pers_h[, w := ifelse(D_hhb_kk_", kirkent_kategori, "_", hhb_kategori, "==1, w_old*(PF_hhb_kk_", kirkent_kategori, "_", hhb_kategori, "/ p_hhb_kk_", kirkent_kategori, "_", hhb_kategori, ")",
                                   ", w_old*((1-a_hhb_kk_", kirkent_kategori, "_", hhb_kategori, "*(PF_hhb_kk_", kirkent_kategori, "_", hhb_kategori, "/p_hhb_kk_", kirkent_kategori, "_", hhb_kategori, "))/(1-a_hhb_kk_", kirkent_kategori, "_", hhb_kategori, ")))]")))
          eval(parse(text = paste0("pers_h[, w_old := w]")))
        }
      }
      # pers_h <- pers_h[, -(p_hhb_kk_1_1:a_hhb_kk_2_9)]
    }

    if(hhbkirkent == FALSE) {
      for(hhb_kategori in 1:hhb_girilen) {
        eval(parse(text = paste0("pers_h[, p_hhb_", hhb_kategori, ":= weighted.mean(D_hhb_", hhb_kategori, ", w=w)]")))
        eval(parse(text = paste0("pers_h[, a_hhb_", hhb_kategori, ":= weighted.mean(D_hhb_", hhb_kategori, ", w=w)]")))
        eval(parse(text = paste0("pers_h[, w := ifelse(D_hhb_", hhb_kategori, "==1, w_old*(PF_hhb_", hhb_kategori, "/ p_hhb_", hhb_kategori, ")",
                                 ", w_old*((1-a_hhb_", hhb_kategori, "*(PF_hhb_", hhb_kategori, "/p_hhb_", hhb_kategori, "))/(1-a_hhb_", hhb_kategori, ")))]")))
        eval(parse(text = paste0("pers_h[, w_old := w]")))
      }
      # pers_h <- pers_h[, -(p_hhb_kk_1_1:a_hhb_kk_2_9)]
    }

    # Yas - Cinsiyet Kalibrasyonu

    # Kullanici "yascinsiyet = TRUE" secenegini isaretlerse asagidaki dongulere girecek.
    # "weighted.mean(a, b)" fonksiyonu (a*b / sum(b)) islemini yapar. Burada "D_m_0_4" fert bazinda
    # 0-1 degerlerinden olusur. Eger fert ilgili gruptaysa 1, degilse 0 degerini alir. "p_m_0_4" bize
    # ilgili grupta yer alan fertlerin agirlikli oranini verir, yani (D_m_0_4 * w / sum(w)).
    # "DH_m_0_4" hane bazinda 0-1 degerlerinden olusur. Eger hanede 4 kisi yasiyor ve 1 tanesi "m_0_4" grubundaysa
    # hanenin butun fertlerinin DH_m_0_4 degeri 1 oluyor. Dolayisiyla "a_m_0_4" de icerisinde "m_0_4" yas
    # grubundan en az 1 fert bulunan hanelerin agirlikli oranini veriyor.

    if(yascinsiyet == TRUE){
      pers_h[, p_m_0_4 := weighted.mean(D_m_0_4, w = w)]
      pers_h[, a_m_0_4 := weighted.mean(DH_m_0_4, w = w)]
      pers_h[, w := ifelse(DH_m_0_4 == 1, w_old*(PF_m_0_4/p_m_0_4), w_old*((1-a_m_0_4*(PF_m_0_4/p_m_0_4))/(1-a_m_0_4)))]
      pers_h[, w_old := w]

      pers_h[, p_m_5_11 := weighted.mean(D_m_5_11, w = w)]
      pers_h[, a_m_5_11 := weighted.mean(DH_m_5_11, w = w)]
      pers_h[, w := ifelse(DH_m_5_11 == 1, w_old*(PF_m_5_11/p_m_5_11), w_old*((1-a_m_5_11*(PF_m_5_11/p_m_5_11))/(1-a_m_5_11)))]
      pers_h[, w_old := w]

      pers_h[, p_m_12_14 := weighted.mean(D_m_12_14, w = w)]
      pers_h[, a_m_12_14 := weighted.mean(DH_m_12_14, w = w)]
      pers_h[, w := ifelse(DH_m_12_14 == 1, w_old*(PF_m_12_14/p_m_12_14), w_old*((1-a_m_12_14*(PF_m_12_14/p_m_12_14))/(1-a_m_12_14)))]
      pers_h[, w_old := w]

      pers_h[, p_m_15_17 := weighted.mean(D_m_15_17, w = w)]
      pers_h[, a_m_15_17 := weighted.mean(DH_m_15_17, w = w)]
      pers_h[, w := ifelse(DH_m_15_17 == 1, w_old*(PF_m_15_17/p_m_15_17), w_old*((1-a_m_15_17*(PF_m_15_17/p_m_15_17))/(1-a_m_15_17)))]
      pers_h[, w_old := w]

      pers_h[, p_m_18_20 := weighted.mean(D_m_18_20, w = w)]
      pers_h[, a_m_18_20 := weighted.mean(DH_m_18_20, w = w)]
      pers_h[, w := ifelse(DH_m_18_20 == 1, w_old*(PF_m_18_20/p_m_18_20), w_old*((1-a_m_18_20*(PF_m_18_20/p_m_18_20))/(1-a_m_18_20)))]
      pers_h[, w_old := w]

      pers_h[, p_m_21_24 := weighted.mean(D_m_21_24, w = w)]
      pers_h[, a_m_21_24 := weighted.mean(DH_m_21_24, w = w)]
      pers_h[, w := ifelse(DH_m_21_24 == 1, w_old*(PF_m_21_24/p_m_21_24), w_old*((1-a_m_21_24*(PF_m_21_24/p_m_21_24))/(1-a_m_21_24)))]
      pers_h[, w_old := w]

      pers_h[, p_m_25_29 := weighted.mean(D_m_25_29, w = w)]
      pers_h[, a_m_25_29 := weighted.mean(DH_m_25_29, w = w)]
      pers_h[, w := ifelse(DH_m_25_29 == 1, w_old*(PF_m_25_29/p_m_25_29), w_old*((1-a_m_25_29*(PF_m_25_29/p_m_25_29))/(1-a_m_25_29)))]
      pers_h[, w_old := w]

      pers_h[, p_m_30_34 := weighted.mean(D_m_30_34, w = w)]
      pers_h[, a_m_30_34 := weighted.mean(DH_m_30_34, w = w)]
      pers_h[, w := ifelse(DH_m_30_34 == 1, w_old*(PF_m_30_34/p_m_30_34), w_old*((1-a_m_30_34*(PF_m_30_34/p_m_30_34))/(1-a_m_30_34)))]
      pers_h[, w_old := w]

      pers_h[, p_m_35_39 := weighted.mean(D_m_35_39, w = w)]
      pers_h[, a_m_35_39:= weighted.mean(DH_m_35_39, w = w)]
      pers_h[, w := ifelse(DH_m_35_39 == 1, w_old*(PF_m_35_39/p_m_35_39), w_old*((1-a_m_35_39*(PF_m_35_39/p_m_35_39))/(1-a_m_35_39)))]
      pers_h[, w_old := w]

      pers_h[, p_m_40_44 := weighted.mean(D_m_40_44, w = w)]
      pers_h[, a_m_40_44 := weighted.mean(DH_m_40_44, w = w)]
      pers_h[, w := ifelse(DH_m_40_44 == 1, w_old*(PF_m_40_44/p_m_40_44), w_old*((1-a_m_40_44*(PF_m_40_44/p_m_40_44))/(1-a_m_40_44)))]
      pers_h[, w_old := w]

      pers_h[, p_m_45_49 := weighted.mean(D_m_45_49, w = w)]
      pers_h[, a_m_45_49 := weighted.mean(DH_m_45_49, w = w)]
      pers_h[, w := ifelse(DH_m_45_49 == 1, w_old*(PF_m_45_49/p_m_45_49), w_old*((1-a_m_45_49*(PF_m_45_49/p_m_45_49))/(1-a_m_45_49)))]
      pers_h[, w_old := w]

      pers_h[, p_m_50_54 := weighted.mean(D_m_50_54, w = w)]
      pers_h[, a_m_50_54 := weighted.mean(DH_m_50_54, w = w)]
      pers_h[, w := ifelse(DH_m_50_54 == 1, w_old*(PF_m_50_54/p_m_50_54), w_old*((1-a_m_50_54*(PF_m_50_54/p_m_50_54))/(1-a_m_50_54)))]
      pers_h[, w_old := w]

      pers_h[, p_m_55_59:= weighted.mean(D_m_55_59, w = w)]
      pers_h[, a_m_55_59 := weighted.mean(DH_m_55_59, w = w)]
      pers_h[, w := ifelse(DH_m_55_59 == 1, w_old*(PF_m_55_59/p_m_55_59), w_old*((1-a_m_55_59*(PF_m_55_59/p_m_55_59))/(1-a_m_55_59)))]
      pers_h[, w_old := w]

      pers_h[, p_m_60_64 := weighted.mean(D_m_60_64, w = w)]
      pers_h[, a_m_60_64 := weighted.mean(DH_m_60_64, w = w)]
      pers_h[, w := ifelse(DH_m_60_64 == 1, w_old*(PF_m_60_64/p_m_60_64), w_old*((1-a_m_60_64*(PF_m_60_64/p_m_60_64))/(1-a_m_60_64)))]
      pers_h[, w_old := w]

      pers_h[, p_m_65_74 := weighted.mean(D_m_65_74, w = w)]
      pers_h[, a_m_65_74 := weighted.mean(DH_m_65_74, w = w)]
      pers_h[, w := ifelse(DH_m_65_74 == 1, w_old*(PF_m_65_74/p_m_65_74), w_old*((1-a_m_65_74*(PF_m_65_74/p_m_65_74))/(1-a_m_65_74)))]
      pers_h[, w_old := w]

      pers_h[, p_m_over75 := weighted.mean(D_m_over75, w = w)]
      pers_h[, a_m_over75:= weighted.mean(DH_m_over75, w = w)]
      pers_h[, w := ifelse(DH_m_over75 == 1, w_old*(PF_m_over75/p_m_over75), w_old*((1-a_m_over75*(PF_m_over75/p_m_over75))/(1-a_m_over75)))]
      pers_h[, w_old := w]

      pers_h[, p_f_0_4 := weighted.mean(D_f_0_4, w = w)]
      pers_h[, a_f_0_4 := weighted.mean(DH_f_0_4, w = w)]
      pers_h[, w := ifelse(DH_f_0_4 == 1, w_old*(PF_f_0_4/p_f_0_4), w_old*((1-a_f_0_4*(PF_f_0_4/p_f_0_4))/(1-a_f_0_4)))]
      pers_h[, w_old := w]

      pers_h[, p_f_5_11 := weighted.mean(D_f_5_11, w = w)]
      pers_h[, a_f_5_11 := weighted.mean(DH_f_5_11, w = w)]
      pers_h[, w := ifelse(DH_f_5_11 == 1, w_old*(PF_f_5_11/p_f_5_11), w_old*((1-a_f_5_11*(PF_f_5_11/p_f_5_11))/(1-a_f_5_11)))]
      pers_h[, w_old := w]

      pers_h[, p_f_12_14 := weighted.mean(D_f_12_14, w = w)]
      pers_h[, a_f_12_14 := weighted.mean(DH_f_12_14, w = w)]
      pers_h[, w := ifelse(DH_f_12_14 == 1, w_old*(PF_f_12_14/p_f_12_14), w_old*((1-a_f_12_14*(PF_f_12_14/p_f_12_14))/(1-a_f_12_14)))]
      pers_h[, w_old := w]

      pers_h[, p_f_15_17 := weighted.mean(D_f_15_17, w = w)]
      pers_h[, a_f_15_17 := weighted.mean(DH_f_15_17, w = w)]
      pers_h[, w := ifelse(DH_f_15_17 == 1, w_old*(PF_f_15_17/p_f_15_17), w_old*((1-a_f_15_17*(PF_f_15_17/p_f_15_17))/(1-a_f_15_17)))]
      pers_h[, w_old := w]

      pers_h[, p_f_18_20 := weighted.mean(D_f_18_20, w = w)]
      pers_h[, a_f_18_20 := weighted.mean(DH_f_18_20, w = w)]
      pers_h[, w := ifelse(DH_f_18_20 == 1, w_old*(PF_f_18_20/p_f_18_20), w_old*((1-a_f_18_20*(PF_f_18_20/p_f_18_20))/(1-a_f_18_20)))]
      pers_h[, w_old := w]

      pers_h[, p_f_21_24 := weighted.mean(D_f_21_24, w = w)]
      pers_h[, a_f_21_24 := weighted.mean(DH_f_21_24, w = w)]
      pers_h[, w := ifelse(DH_f_21_24 == 1, w_old*(PF_f_21_24/p_f_21_24), w_old*((1-a_f_21_24*(PF_f_21_24/p_f_21_24))/(1-a_f_21_24)))]
      pers_h[, w_old := w]

      pers_h[, p_f_25_29 := weighted.mean(D_f_25_29, w = w)]
      pers_h[, a_f_25_29 := weighted.mean(DH_f_25_29, w = w)]
      pers_h[, w := ifelse(DH_f_25_29 == 1, w_old*(PF_f_25_29/p_f_25_29), w_old*((1-a_f_25_29*(PF_f_25_29/p_f_25_29))/(1-a_f_25_29)))]
      pers_h[, w_old := w]

      pers_h[, p_f_30_34 := weighted.mean(D_f_30_34, w = w)]
      pers_h[, a_f_30_34 := weighted.mean(DH_f_30_34, w = w)]
      pers_h[, w := ifelse(DH_f_30_34 == 1, w_old*(PF_f_30_34/p_f_30_34), w_old*((1-a_f_30_34*(PF_f_30_34/p_f_30_34))/(1-a_f_30_34)))]
      pers_h[, w_old := w]

      pers_h[, p_f_35_39 := weighted.mean(D_f_35_39, w = w)]
      pers_h[, a_f_35_39:= weighted.mean(DH_f_35_39, w = w)]
      pers_h[, w := ifelse(DH_f_35_39 == 1, w_old*(PF_f_35_39/p_f_35_39), w_old*((1-a_f_35_39*(PF_f_35_39/p_f_35_39))/(1-a_f_35_39)))]
      pers_h[, w_old := w]

      pers_h[, p_f_40_44 := weighted.mean(D_f_40_44, w = w)]
      pers_h[, a_f_40_44 := weighted.mean(DH_f_40_44, w = w)]
      pers_h[, w := ifelse(DH_f_40_44 == 1, w_old*(PF_f_40_44/p_f_40_44), w_old*((1-a_f_40_44*(PF_f_40_44/p_f_40_44))/(1-a_f_40_44)))]
      pers_h[, w_old := w]

      pers_h[, p_f_45_49 := weighted.mean(D_f_45_49, w = w)]
      pers_h[, a_f_45_49 := weighted.mean(DH_f_45_49, w = w)]
      pers_h[, w := ifelse(DH_f_45_49 == 1, w_old*(PF_f_45_49/p_f_45_49), w_old*((1-a_f_45_49*(PF_f_45_49/p_f_45_49))/(1-a_f_45_49)))]
      pers_h[, w_old := w]

      pers_h[, p_f_50_54 := weighted.mean(D_f_50_54, w = w)]
      pers_h[, a_f_50_54 := weighted.mean(DH_f_50_54, w = w)]
      pers_h[, w := ifelse(DH_f_50_54 == 1, w_old*(PF_f_50_54/p_f_50_54), w_old*((1-a_f_50_54*(PF_f_50_54/p_f_50_54))/(1-a_f_50_54)))]
      pers_h[, w_old := w]

      pers_h[, p_f_55_59:= weighted.mean(D_f_55_59, w = w)]
      pers_h[, a_f_55_59 := weighted.mean(DH_f_55_59, w = w)]
      pers_h[, w := ifelse(DH_f_55_59 == 1, w_old*(PF_f_55_59/p_f_55_59), w_old*((1-a_f_55_59*(PF_f_55_59/p_f_55_59))/(1-a_f_55_59)))]
      pers_h[, w_old := w]

      pers_h[, p_f_60_64 := weighted.mean(D_f_60_64, w = w)]
      pers_h[, a_f_60_64 := weighted.mean(DH_f_60_64, w = w)]
      pers_h[, w := ifelse(DH_f_60_64 == 1, w_old*(PF_f_60_64/p_f_60_64), w_old*((1-a_f_60_64*(PF_f_60_64/p_f_60_64))/(1-a_f_60_64)))]
      pers_h[, w_old := w]

      pers_h[, p_f_65_74 := weighted.mean(D_f_65_74, w = w)]
      pers_h[, a_f_65_74 := weighted.mean(DH_f_65_74, w = w)]
      pers_h[, w := ifelse(DH_f_65_74 == 1, w_old*(PF_f_65_74/p_f_65_74), w_old*((1-a_f_65_74*(PF_f_65_74/p_f_65_74))/(1-a_f_65_74)))]
      pers_h[, w_old := w]

      pers_h[, p_f_over75 := weighted.mean(D_f_over75, w = w)]
      pers_h[, a_f_over75:= weighted.mean(DH_f_over75, w = w)]
      pers_h[, w := ifelse(DH_f_over75 == 1, w_old*(PF_f_over75/p_f_over75), w_old*((1-a_f_over75*(PF_f_over75/p_f_over75))/(1-a_f_over75)))]
      pers_h[, w_old := w]
      pers_h <- pers_h[, -(p_m_0_4:a_f_over75)]
    }

    # Il Kalibrasyonu

    # Kullanici uygulama ekraninda "IL" secenegini isaretlerse asagidaki dongulere girecek.
    # IL bazinda w'lari toplatarak arastirmadan elde edilen oranlari hesapliyoruz. Daha sonra
    # projekteden gelen oranlara gore duzeltme yaparak w sutununu guncelliyoruz.
    # Son satirda ise "cal_factor_il" sutunu artik isimize yaramayacagi icin siliyoruz.

    if(il == TRUE){

      pers_h[, p_il_01 := weighted.mean(D_il_01, w = w)]
      pers_h[, a_il_01 := weighted.mean(D_il_01, w = w)]
      pers_h[, w := ifelse(D_il_01 == 1, w_old*(PF_il_01/p_il_01), w_old*((1-a_il_01*(PF_il_01/p_il_01))/(1-a_il_01)))]
      pers_h[, w_old := w]

      pers_h[, p_il_02 := weighted.mean(D_il_02, w = w)]
      pers_h[, a_il_02 := weighted.mean(D_il_02, w = w)]
      pers_h[, w := ifelse(D_il_02 == 1, w_old*(PF_il_02/p_il_02), w_old*((1-a_il_02*(PF_il_02/p_il_02))/(1-a_il_02)))]
      pers_h[, w_old := w]

      pers_h[, p_il_03 := weighted.mean(D_il_03, w = w)]
      pers_h[, a_il_03 := weighted.mean(D_il_03, w = w)]
      pers_h[, w := ifelse(D_il_03 == 1, w_old*(PF_il_03/p_il_03), w_old*((1-a_il_03*(PF_il_03/p_il_03))/(1-a_il_03)))]
      pers_h[, w_old := w]

      pers_h[, p_il_04 := weighted.mean(D_il_04, w = w)]
      pers_h[, a_il_04 := weighted.mean(D_il_04, w = w)]
      pers_h[, w := ifelse(D_il_04 == 1, w_old*(PF_il_04/p_il_04), w_old*((1-a_il_04*(PF_il_04/p_il_04))/(1-a_il_04)))]
      pers_h[, w_old := w]

      pers_h[, p_il_05 := weighted.mean(D_il_05, w = w)]
      pers_h[, a_il_05 := weighted.mean(D_il_05, w = w)]
      pers_h[, w := ifelse(D_il_05 == 1, w_old*(PF_il_05/p_il_05), w_old*((1-a_il_05*(PF_il_05/p_il_05))/(1-a_il_05)))]
      pers_h[, w_old := w]

      pers_h[, p_il_06 := weighted.mean(D_il_06, w = w)]
      pers_h[, a_il_06 := weighted.mean(D_il_06, w = w)]
      pers_h[, w := ifelse(D_il_06 == 1, w_old*(PF_il_06/p_il_06), w_old*((1-a_il_06*(PF_il_06/p_il_06))/(1-a_il_06)))]
      pers_h[, w_old := w]

      pers_h[, p_il_07 := weighted.mean(D_il_07, w = w)]
      pers_h[, a_il_07 := weighted.mean(D_il_07, w = w)]
      pers_h[, w := ifelse(D_il_07 == 1, w_old*(PF_il_07/p_il_07), w_old*((1-a_il_07*(PF_il_07/p_il_07))/(1-a_il_07)))]
      pers_h[, w_old := w]

      pers_h[, p_il_08 := weighted.mean(D_il_08, w = w)]
      pers_h[, a_il_08 := weighted.mean(D_il_08, w = w)]
      pers_h[, w := ifelse(D_il_08 == 1, w_old*(PF_il_08/p_il_08), w_old*((1-a_il_08*(PF_il_08/p_il_08))/(1-a_il_08)))]
      pers_h[, w_old := w]

      pers_h[, p_il_09 := weighted.mean(D_il_09, w = w)]
      pers_h[, a_il_09 := weighted.mean(D_il_09, w = w)]
      pers_h[, w := ifelse(D_il_09 == 1, w_old*(PF_il_09/p_il_09), w_old*((1-a_il_09*(PF_il_09/p_il_09))/(1-a_il_09)))]
      pers_h[, w_old := w]

      pers_h[, p_il_10 := weighted.mean(D_il_10, w = w)]
      pers_h[, a_il_10 := weighted.mean(D_il_10, w = w)]
      pers_h[, w := ifelse(D_il_10 == 1, w_old*(PF_il_10/p_il_10), w_old*((1-a_il_10*(PF_il_10/p_il_10))/(1-a_il_10)))]
      pers_h[, w_old := w]

      pers_h[, p_il_11 := weighted.mean(D_il_11, w = w)]
      pers_h[, a_il_11 := weighted.mean(D_il_11, w = w)]
      pers_h[, w := ifelse(D_il_11 == 1, w_old*(PF_il_11/p_il_11), w_old*((1-a_il_11*(PF_il_11/p_il_11))/(1-a_il_11)))]
      pers_h[, w_old := w]

      pers_h[, p_il_12 := weighted.mean(D_il_12, w = w)]
      pers_h[, a_il_12 := weighted.mean(D_il_12, w = w)]
      pers_h[, w := ifelse(D_il_12 == 1, w_old*(PF_il_12/p_il_12), w_old*((1-a_il_12*(PF_il_12/p_il_12))/(1-a_il_12)))]
      pers_h[, w_old := w]

      pers_h[, p_il_13 := weighted.mean(D_il_13, w = w)]
      pers_h[, a_il_13 := weighted.mean(D_il_13, w = w)]
      pers_h[, w := ifelse(D_il_13 == 1, w_old*(PF_il_13/p_il_13), w_old*((1-a_il_13*(PF_il_13/p_il_13))/(1-a_il_13)))]
      pers_h[, w_old := w]

      pers_h[, p_il_14 := weighted.mean(D_il_14, w = w)]
      pers_h[, a_il_14 := weighted.mean(D_il_14, w = w)]
      pers_h[, w := ifelse(D_il_14 == 1, w_old*(PF_il_14/p_il_14), w_old*((1-a_il_14*(PF_il_14/p_il_14))/(1-a_il_14)))]
      pers_h[, w_old := w]

      pers_h[, p_il_15 := weighted.mean(D_il_15, w = w)]
      pers_h[, a_il_15 := weighted.mean(D_il_15, w = w)]
      pers_h[, w := ifelse(D_il_15 == 1, w_old*(PF_il_15/p_il_15), w_old*((1-a_il_15*(PF_il_15/p_il_15))/(1-a_il_15)))]
      pers_h[, w_old := w]

      pers_h[, p_il_16 := weighted.mean(D_il_16, w = w)]
      pers_h[, a_il_16 := weighted.mean(D_il_16, w = w)]
      pers_h[, w := ifelse(D_il_16 == 1, w_old*(PF_il_16/p_il_16), w_old*((1-a_il_16*(PF_il_16/p_il_16))/(1-a_il_16)))]
      pers_h[, w_old := w]

      pers_h[, p_il_17 := weighted.mean(D_il_17, w = w)]
      pers_h[, a_il_17 := weighted.mean(D_il_17, w = w)]
      pers_h[, w := ifelse(D_il_17 == 1, w_old*(PF_il_17/p_il_17), w_old*((1-a_il_17*(PF_il_17/p_il_17))/(1-a_il_17)))]
      pers_h[, w_old := w]

      pers_h[, p_il_18 := weighted.mean(D_il_18, w = w)]
      pers_h[, a_il_18 := weighted.mean(D_il_18, w = w)]
      pers_h[, w := ifelse(D_il_18 == 1, w_old*(PF_il_18/p_il_18), w_old*((1-a_il_18*(PF_il_18/p_il_18))/(1-a_il_18)))]
      pers_h[, w_old := w]

      pers_h[, p_il_19 := weighted.mean(D_il_19, w = w)]
      pers_h[, a_il_19 := weighted.mean(D_il_19, w = w)]
      pers_h[, w := ifelse(D_il_19 == 1, w_old*(PF_il_19/p_il_19), w_old*((1-a_il_19*(PF_il_19/p_il_19))/(1-a_il_19)))]
      pers_h[, w_old := w]

      pers_h[, p_il_20 := weighted.mean(D_il_20, w = w)]
      pers_h[, a_il_20 := weighted.mean(D_il_20, w = w)]
      pers_h[, w := ifelse(D_il_20 == 1, w_old*(PF_il_20/p_il_20), w_old*((1-a_il_20*(PF_il_20/p_il_20))/(1-a_il_20)))]
      pers_h[, w_old := w]

      pers_h[, p_il_21 := weighted.mean(D_il_21, w = w)]
      pers_h[, a_il_21 := weighted.mean(D_il_21, w = w)]
      pers_h[, w := ifelse(D_il_21 == 1, w_old*(PF_il_21/p_il_21), w_old*((1-a_il_21*(PF_il_21/p_il_21))/(1-a_il_21)))]
      pers_h[, w_old := w]

      pers_h[, p_il_22 := weighted.mean(D_il_22, w = w)]
      pers_h[, a_il_22 := weighted.mean(D_il_22, w = w)]
      pers_h[, w := ifelse(D_il_22 == 1, w_old*(PF_il_22/p_il_22), w_old*((1-a_il_22*(PF_il_22/p_il_22))/(1-a_il_22)))]
      pers_h[, w_old := w]

      pers_h[, p_il_23 := weighted.mean(D_il_23, w = w)]
      pers_h[, a_il_23 := weighted.mean(D_il_23, w = w)]
      pers_h[, w := ifelse(D_il_23 == 1, w_old*(PF_il_23/p_il_23), w_old*((1-a_il_23*(PF_il_23/p_il_23))/(1-a_il_23)))]
      pers_h[, w_old := w]

      pers_h[, p_il_24 := weighted.mean(D_il_24, w = w)]
      pers_h[, a_il_24 := weighted.mean(D_il_24, w = w)]
      pers_h[, w := ifelse(D_il_24 == 1, w_old*(PF_il_24/p_il_24), w_old*((1-a_il_24*(PF_il_24/p_il_24))/(1-a_il_24)))]
      pers_h[, w_old := w]

      pers_h[, p_il_25 := weighted.mean(D_il_25, w = w)]
      pers_h[, a_il_25 := weighted.mean(D_il_25, w = w)]
      pers_h[, w := ifelse(D_il_25 == 1, w_old*(PF_il_25/p_il_25), w_old*((1-a_il_25*(PF_il_25/p_il_25))/(1-a_il_25)))]
      pers_h[, w_old := w]

      pers_h[, p_il_26 := weighted.mean(D_il_26, w = w)]
      pers_h[, a_il_26 := weighted.mean(D_il_26, w = w)]
      pers_h[, w := ifelse(D_il_26 == 1, w_old*(PF_il_26/p_il_26), w_old*((1-a_il_26*(PF_il_26/p_il_26))/(1-a_il_26)))]
      pers_h[, w_old := w]

      pers_h[, p_il_27 := weighted.mean(D_il_27, w = w)]
      pers_h[, a_il_27 := weighted.mean(D_il_27, w = w)]
      pers_h[, w := ifelse(D_il_27 == 1, w_old*(PF_il_27/p_il_27), w_old*((1-a_il_27*(PF_il_27/p_il_27))/(1-a_il_27)))]
      pers_h[, w_old := w]

      pers_h[, p_il_28 := weighted.mean(D_il_28, w = w)]
      pers_h[, a_il_28 := weighted.mean(D_il_28, w = w)]
      pers_h[, w := ifelse(D_il_28 == 1, w_old*(PF_il_28/p_il_28), w_old*((1-a_il_28*(PF_il_28/p_il_28))/(1-a_il_28)))]
      pers_h[, w_old := w]

      pers_h[, p_il_29 := weighted.mean(D_il_29, w = w)]
      pers_h[, a_il_29 := weighted.mean(D_il_29, w = w)]
      pers_h[, w := ifelse(D_il_29 == 1, w_old*(PF_il_29/p_il_29), w_old*((1-a_il_29*(PF_il_29/p_il_29))/(1-a_il_29)))]
      pers_h[, w_old := w]

      pers_h[, p_il_30 := weighted.mean(D_il_30, w = w)]
      pers_h[, a_il_30 := weighted.mean(D_il_30, w = w)]
      pers_h[, w := ifelse(D_il_30 == 1, w_old*(PF_il_30/p_il_30), w_old*((1-a_il_30*(PF_il_30/p_il_30))/(1-a_il_30)))]
      pers_h[, w_old := w]

      pers_h[, p_il_31 := weighted.mean(D_il_31, w = w)]
      pers_h[, a_il_31 := weighted.mean(D_il_31, w = w)]
      pers_h[, w := ifelse(D_il_31 == 1, w_old*(PF_il_31/p_il_31), w_old*((1-a_il_31*(PF_il_31/p_il_31))/(1-a_il_31)))]
      pers_h[, w_old := w]

      pers_h[, p_il_32 := weighted.mean(D_il_32, w = w)]
      pers_h[, a_il_32 := weighted.mean(D_il_32, w = w)]
      pers_h[, w := ifelse(D_il_32 == 1, w_old*(PF_il_32/p_il_32), w_old*((1-a_il_32*(PF_il_32/p_il_32))/(1-a_il_32)))]
      pers_h[, w_old := w]

      pers_h[, p_il_33 := weighted.mean(D_il_33, w = w)]
      pers_h[, a_il_33 := weighted.mean(D_il_33, w = w)]
      pers_h[, w := ifelse(D_il_33 == 1, w_old*(PF_il_33/p_il_33), w_old*((1-a_il_33*(PF_il_33/p_il_33))/(1-a_il_33)))]
      pers_h[, w_old := w]

      pers_h[, p_il_34 := weighted.mean(D_il_34, w = w)]
      pers_h[, a_il_34 := weighted.mean(D_il_34, w = w)]
      pers_h[, w := ifelse(D_il_34 == 1, w_old*(PF_il_34/p_il_34), w_old*((1-a_il_34*(PF_il_34/p_il_34))/(1-a_il_34)))]
      pers_h[, w_old := w]

      pers_h[, p_il_35 := weighted.mean(D_il_35, w = w)]
      pers_h[, a_il_35 := weighted.mean(D_il_35, w = w)]
      pers_h[, w := ifelse(D_il_35 == 1, w_old*(PF_il_35/p_il_35), w_old*((1-a_il_35*(PF_il_35/p_il_35))/(1-a_il_35)))]
      pers_h[, w_old := w]

      pers_h[, p_il_36 := weighted.mean(D_il_36, w = w)]
      pers_h[, a_il_36 := weighted.mean(D_il_36, w = w)]
      pers_h[, w := ifelse(D_il_36 == 1, w_old*(PF_il_36/p_il_36), w_old*((1-a_il_36*(PF_il_36/p_il_36))/(1-a_il_36)))]
      pers_h[, w_old := w]

      pers_h[, p_il_37 := weighted.mean(D_il_37, w = w)]
      pers_h[, a_il_37 := weighted.mean(D_il_37, w = w)]
      pers_h[, w := ifelse(D_il_37 == 1, w_old*(PF_il_37/p_il_37), w_old*((1-a_il_37*(PF_il_37/p_il_37))/(1-a_il_37)))]
      pers_h[, w_old := w]

      pers_h[, p_il_38 := weighted.mean(D_il_38, w = w)]
      pers_h[, a_il_38 := weighted.mean(D_il_38, w = w)]
      pers_h[, w := ifelse(D_il_38 == 1, w_old*(PF_il_38/p_il_38), w_old*((1-a_il_38*(PF_il_38/p_il_38))/(1-a_il_38)))]
      pers_h[, w_old := w]

      pers_h[, p_il_39 := weighted.mean(D_il_39, w = w)]
      pers_h[, a_il_39 := weighted.mean(D_il_39, w = w)]
      pers_h[, w := ifelse(D_il_39 == 1, w_old*(PF_il_39/p_il_39), w_old*((1-a_il_39*(PF_il_39/p_il_39))/(1-a_il_39)))]
      pers_h[, w_old := w]

      pers_h[, p_il_40 := weighted.mean(D_il_40, w = w)]
      pers_h[, a_il_40 := weighted.mean(D_il_40, w = w)]
      pers_h[, w := ifelse(D_il_40 == 1, w_old*(PF_il_40/p_il_40), w_old*((1-a_il_40*(PF_il_40/p_il_40))/(1-a_il_40)))]
      pers_h[, w_old := w]

      pers_h[, p_il_41 := weighted.mean(D_il_41, w = w)]
      pers_h[, a_il_41 := weighted.mean(D_il_41, w = w)]
      pers_h[, w := ifelse(D_il_41 == 1, w_old*(PF_il_41/p_il_41), w_old*((1-a_il_41*(PF_il_41/p_il_41))/(1-a_il_41)))]
      pers_h[, w_old := w]

      pers_h[, p_il_42 := weighted.mean(D_il_42, w = w)]
      pers_h[, a_il_42 := weighted.mean(D_il_42, w = w)]
      pers_h[, w := ifelse(D_il_42 == 1, w_old*(PF_il_42/p_il_42), w_old*((1-a_il_42*(PF_il_42/p_il_42))/(1-a_il_42)))]
      pers_h[, w_old := w]

      pers_h[, p_il_43 := weighted.mean(D_il_43, w = w)]
      pers_h[, a_il_43 := weighted.mean(D_il_43, w = w)]
      pers_h[, w := ifelse(D_il_43 == 1, w_old*(PF_il_43/p_il_43), w_old*((1-a_il_43*(PF_il_43/p_il_43))/(1-a_il_43)))]
      pers_h[, w_old := w]

      pers_h[, p_il_44 := weighted.mean(D_il_44, w = w)]
      pers_h[, a_il_44 := weighted.mean(D_il_44, w = w)]
      pers_h[, w := ifelse(D_il_44 == 1, w_old*(PF_il_44/p_il_44), w_old*((1-a_il_44*(PF_il_44/p_il_44))/(1-a_il_44)))]
      pers_h[, w_old := w]

      pers_h[, p_il_45 := weighted.mean(D_il_45, w = w)]
      pers_h[, a_il_45 := weighted.mean(D_il_45, w = w)]
      pers_h[, w := ifelse(D_il_45 == 1, w_old*(PF_il_45/p_il_45), w_old*((1-a_il_45*(PF_il_45/p_il_45))/(1-a_il_45)))]
      pers_h[, w_old := w]

      pers_h[, p_il_46 := weighted.mean(D_il_46, w = w)]
      pers_h[, a_il_46 := weighted.mean(D_il_46, w = w)]
      pers_h[, w := ifelse(D_il_46 == 1, w_old*(PF_il_46/p_il_46), w_old*((1-a_il_46*(PF_il_46/p_il_46))/(1-a_il_46)))]
      pers_h[, w_old := w]

      pers_h[, p_il_47 := weighted.mean(D_il_47, w = w)]
      pers_h[, a_il_47 := weighted.mean(D_il_47, w = w)]
      pers_h[, w := ifelse(D_il_47 == 1, w_old*(PF_il_47/p_il_47), w_old*((1-a_il_47*(PF_il_47/p_il_47))/(1-a_il_47)))]
      pers_h[, w_old := w]

      pers_h[, p_il_48 := weighted.mean(D_il_48, w = w)]
      pers_h[, a_il_48 := weighted.mean(D_il_48, w = w)]
      pers_h[, w := ifelse(D_il_48 == 1, w_old*(PF_il_48/p_il_48), w_old*((1-a_il_48*(PF_il_48/p_il_48))/(1-a_il_48)))]
      pers_h[, w_old := w]

      pers_h[, p_il_49 := weighted.mean(D_il_49, w = w)]
      pers_h[, a_il_49 := weighted.mean(D_il_49, w = w)]
      pers_h[, w := ifelse(D_il_49 == 1, w_old*(PF_il_49/p_il_49), w_old*((1-a_il_49*(PF_il_49/p_il_49))/(1-a_il_49)))]
      pers_h[, w_old := w]

      pers_h[, p_il_50 := weighted.mean(D_il_50, w = w)]
      pers_h[, a_il_50 := weighted.mean(D_il_50, w = w)]
      pers_h[, w := ifelse(D_il_50 == 1, w_old*(PF_il_50/p_il_50), w_old*((1-a_il_50*(PF_il_50/p_il_50))/(1-a_il_50)))]
      pers_h[, w_old := w]

      pers_h[, p_il_51 := weighted.mean(D_il_51, w = w)]
      pers_h[, a_il_51 := weighted.mean(D_il_51, w = w)]
      pers_h[, w := ifelse(D_il_51 == 1, w_old*(PF_il_51/p_il_51), w_old*((1-a_il_51*(PF_il_51/p_il_51))/(1-a_il_51)))]
      pers_h[, w_old := w]

      pers_h[, p_il_52 := weighted.mean(D_il_52, w = w)]
      pers_h[, a_il_52 := weighted.mean(D_il_52, w = w)]
      pers_h[, w := ifelse(D_il_52 == 1, w_old*(PF_il_52/p_il_52), w_old*((1-a_il_52*(PF_il_52/p_il_52))/(1-a_il_52)))]
      pers_h[, w_old := w]

      pers_h[, p_il_53 := weighted.mean(D_il_53, w = w)]
      pers_h[, a_il_53 := weighted.mean(D_il_53, w = w)]
      pers_h[, w := ifelse(D_il_53 == 1, w_old*(PF_il_53/p_il_53), w_old*((1-a_il_53*(PF_il_53/p_il_53))/(1-a_il_53)))]
      pers_h[, w_old := w]

      pers_h[, p_il_54 := weighted.mean(D_il_54, w = w)]
      pers_h[, a_il_54 := weighted.mean(D_il_54, w = w)]
      pers_h[, w := ifelse(D_il_54 == 1, w_old*(PF_il_54/p_il_54), w_old*((1-a_il_54*(PF_il_54/p_il_54))/(1-a_il_54)))]
      pers_h[, w_old := w]

      pers_h[, p_il_55 := weighted.mean(D_il_55, w = w)]
      pers_h[, a_il_55 := weighted.mean(D_il_55, w = w)]
      pers_h[, w := ifelse(D_il_55 == 1, w_old*(PF_il_55/p_il_55), w_old*((1-a_il_55*(PF_il_55/p_il_55))/(1-a_il_55)))]
      pers_h[, w_old := w]

      pers_h[, p_il_56 := weighted.mean(D_il_56, w = w)]
      pers_h[, a_il_56 := weighted.mean(D_il_56, w = w)]
      pers_h[, w := ifelse(D_il_56 == 1, w_old*(PF_il_56/p_il_56), w_old*((1-a_il_56*(PF_il_56/p_il_56))/(1-a_il_56)))]
      pers_h[, w_old := w]

      pers_h[, p_il_57 := weighted.mean(D_il_57, w = w)]
      pers_h[, a_il_57 := weighted.mean(D_il_57, w = w)]
      pers_h[, w := ifelse(D_il_57 == 1, w_old*(PF_il_57/p_il_57), w_old*((1-a_il_57*(PF_il_57/p_il_57))/(1-a_il_57)))]
      pers_h[, w_old := w]

      pers_h[, p_il_58 := weighted.mean(D_il_58, w = w)]
      pers_h[, a_il_58 := weighted.mean(D_il_58, w = w)]
      pers_h[, w := ifelse(D_il_58 == 1, w_old*(PF_il_58/p_il_58), w_old*((1-a_il_58*(PF_il_58/p_il_58))/(1-a_il_58)))]
      pers_h[, w_old := w]

      pers_h[, p_il_59 := weighted.mean(D_il_59, w = w)]
      pers_h[, a_il_59 := weighted.mean(D_il_59, w = w)]
      pers_h[, w := ifelse(D_il_59 == 1, w_old*(PF_il_59/p_il_59), w_old*((1-a_il_59*(PF_il_59/p_il_59))/(1-a_il_59)))]
      pers_h[, w_old := w]

      pers_h[, p_il_60 := weighted.mean(D_il_60, w = w)]
      pers_h[, a_il_60 := weighted.mean(D_il_60, w = w)]
      pers_h[, w := ifelse(D_il_60 == 1, w_old*(PF_il_60/p_il_60), w_old*((1-a_il_60*(PF_il_60/p_il_60))/(1-a_il_60)))]
      pers_h[, w_old := w]

      pers_h[, p_il_61 := weighted.mean(D_il_61, w = w)]
      pers_h[, a_il_61 := weighted.mean(D_il_61, w = w)]
      pers_h[, w := ifelse(D_il_61 == 1, w_old*(PF_il_61/p_il_61), w_old*((1-a_il_61*(PF_il_61/p_il_61))/(1-a_il_61)))]
      pers_h[, w_old := w]

      pers_h[, p_il_62 := weighted.mean(D_il_62, w = w)]
      pers_h[, a_il_62 := weighted.mean(D_il_62, w = w)]
      pers_h[, w := ifelse(D_il_62 == 1, w_old*(PF_il_62/p_il_62), w_old*((1-a_il_62*(PF_il_62/p_il_62))/(1-a_il_62)))]
      pers_h[, w_old := w]

      pers_h[, p_il_63 := weighted.mean(D_il_63, w = w)]
      pers_h[, a_il_63 := weighted.mean(D_il_63, w = w)]
      pers_h[, w := ifelse(D_il_63 == 1, w_old*(PF_il_63/p_il_63), w_old*((1-a_il_63*(PF_il_63/p_il_63))/(1-a_il_63)))]
      pers_h[, w_old := w]

      pers_h[, p_il_64 := weighted.mean(D_il_64, w = w)]
      pers_h[, a_il_64 := weighted.mean(D_il_64, w = w)]
      pers_h[, w := ifelse(D_il_64 == 1, w_old*(PF_il_64/p_il_64), w_old*((1-a_il_64*(PF_il_64/p_il_64))/(1-a_il_64)))]
      pers_h[, w_old := w]

      pers_h[, p_il_65 := weighted.mean(D_il_65, w = w)]
      pers_h[, a_il_65 := weighted.mean(D_il_65, w = w)]
      pers_h[, w := ifelse(D_il_65 == 1, w_old*(PF_il_65/p_il_65), w_old*((1-a_il_65*(PF_il_65/p_il_65))/(1-a_il_65)))]
      pers_h[, w_old := w]

      pers_h[, p_il_66 := weighted.mean(D_il_66, w = w)]
      pers_h[, a_il_66 := weighted.mean(D_il_66, w = w)]
      pers_h[, w := ifelse(D_il_66 == 1, w_old*(PF_il_66/p_il_66), w_old*((1-a_il_66*(PF_il_66/p_il_66))/(1-a_il_66)))]
      pers_h[, w_old := w]

      pers_h[, p_il_67 := weighted.mean(D_il_67, w = w)]
      pers_h[, a_il_67 := weighted.mean(D_il_67, w = w)]
      pers_h[, w := ifelse(D_il_67 == 1, w_old*(PF_il_67/p_il_67), w_old*((1-a_il_67*(PF_il_67/p_il_67))/(1-a_il_67)))]
      pers_h[, w_old := w]

      pers_h[, p_il_68 := weighted.mean(D_il_68, w = w)]
      pers_h[, a_il_68 := weighted.mean(D_il_68, w = w)]
      pers_h[, w := ifelse(D_il_68 == 1, w_old*(PF_il_68/p_il_68), w_old*((1-a_il_68*(PF_il_68/p_il_68))/(1-a_il_68)))]
      pers_h[, w_old := w]

      pers_h[, p_il_69 := weighted.mean(D_il_69, w = w)]
      pers_h[, a_il_69 := weighted.mean(D_il_69, w = w)]
      pers_h[, w := ifelse(D_il_69 == 1, w_old*(PF_il_69/p_il_69), w_old*((1-a_il_69*(PF_il_69/p_il_69))/(1-a_il_69)))]
      pers_h[, w_old := w]

      pers_h[, p_il_70 := weighted.mean(D_il_70, w = w)]
      pers_h[, a_il_70 := weighted.mean(D_il_70, w = w)]
      pers_h[, w := ifelse(D_il_70 == 1, w_old*(PF_il_70/p_il_70), w_old*((1-a_il_70*(PF_il_70/p_il_70))/(1-a_il_70)))]
      pers_h[, w_old := w]

      pers_h[, p_il_71 := weighted.mean(D_il_71, w = w)]
      pers_h[, a_il_71 := weighted.mean(D_il_71, w = w)]
      pers_h[, w := ifelse(D_il_71 == 1, w_old*(PF_il_71/p_il_71), w_old*((1-a_il_71*(PF_il_71/p_il_71))/(1-a_il_71)))]
      pers_h[, w_old := w]

      pers_h[, p_il_72 := weighted.mean(D_il_72, w = w)]
      pers_h[, a_il_72 := weighted.mean(D_il_72, w = w)]
      pers_h[, w := ifelse(D_il_72 == 1, w_old*(PF_il_72/p_il_72), w_old*((1-a_il_72*(PF_il_72/p_il_72))/(1-a_il_72)))]
      pers_h[, w_old := w]

      pers_h[, p_il_73 := weighted.mean(D_il_73, w = w)]
      pers_h[, a_il_73 := weighted.mean(D_il_73, w = w)]
      pers_h[, w := ifelse(D_il_73 == 1, w_old*(PF_il_73/p_il_73), w_old*((1-a_il_73*(PF_il_73/p_il_73))/(1-a_il_73)))]
      pers_h[, w_old := w]

      pers_h[, p_il_74 := weighted.mean(D_il_74, w = w)]
      pers_h[, a_il_74 := weighted.mean(D_il_74, w = w)]
      pers_h[, w := ifelse(D_il_74 == 1, w_old*(PF_il_74/p_il_74), w_old*((1-a_il_74*(PF_il_74/p_il_74))/(1-a_il_74)))]
      pers_h[, w_old := w]

      pers_h[, p_il_75 := weighted.mean(D_il_75, w = w)]
      pers_h[, a_il_75 := weighted.mean(D_il_75, w = w)]
      pers_h[, w := ifelse(D_il_75 == 1, w_old*(PF_il_75/p_il_75), w_old*((1-a_il_75*(PF_il_75/p_il_75))/(1-a_il_75)))]
      pers_h[, w_old := w]

      pers_h[, p_il_76 := weighted.mean(D_il_76, w = w)]
      pers_h[, a_il_76 := weighted.mean(D_il_76, w = w)]
      pers_h[, w := ifelse(D_il_76 == 1, w_old*(PF_il_76/p_il_76), w_old*((1-a_il_76*(PF_il_76/p_il_76))/(1-a_il_76)))]
      pers_h[, w_old := w]

      pers_h[, p_il_77 := weighted.mean(D_il_77, w = w)]
      pers_h[, a_il_77 := weighted.mean(D_il_77, w = w)]
      pers_h[, w := ifelse(D_il_77 == 1, w_old*(PF_il_77/p_il_77), w_old*((1-a_il_77*(PF_il_77/p_il_77))/(1-a_il_77)))]
      pers_h[, w_old := w]

      pers_h[, p_il_78 := weighted.mean(D_il_78, w = w)]
      pers_h[, a_il_78 := weighted.mean(D_il_78, w = w)]
      pers_h[, w := ifelse(D_il_78 == 1, w_old*(PF_il_78/p_il_78), w_old*((1-a_il_78*(PF_il_78/p_il_78))/(1-a_il_78)))]
      pers_h[, w_old := w]

      pers_h[, p_il_79 := weighted.mean(D_il_79, w = w)]
      pers_h[, a_il_79 := weighted.mean(D_il_79, w = w)]
      pers_h[, w := ifelse(D_il_79 == 1, w_old*(PF_il_79/p_il_79), w_old*((1-a_il_79*(PF_il_79/p_il_79))/(1-a_il_79)))]
      pers_h[, w_old := w]

      pers_h[, p_il_80 := weighted.mean(D_il_80, w = w)]
      pers_h[, a_il_80 := weighted.mean(D_il_80, w = w)]
      pers_h[, w := ifelse(D_il_80 == 1, w_old*(PF_il_80/p_il_80), w_old*((1-a_il_80*(PF_il_80/p_il_80))/(1-a_il_80)))]
      pers_h[, w_old := w]

      pers_h[, p_il_81 := weighted.mean(D_il_81, w = w)]
      pers_h[, a_il_81 := weighted.mean(D_il_81, w = w)]
      pers_h[, w := ifelse(D_il_81 == 1, w_old*(PF_il_81/p_il_81), w_old*((1-a_il_81*(PF_il_81/p_il_81))/(1-a_il_81)))]
      pers_h[, w_old := w]

      pers_h <- pers_h[, -(p_il_01:a_il_81)]

    }
    ### -------------------------------------------------- ###

    # Nuts2 / Kirkent Kalibrasyonu

    # Kullanici "nutskirkent = TRUE" secenegini isaretlerse asagidaki dongulere girecek.
    # Nuts2 - Kirkent bazinda w'lari toplatarak arastirmadan elde edilen oranlari hesapliyoruz. Daha sonra
    # projekteden gelen oranlara gore duzeltme yaparak w sutununu guncelliyoruz.
    # Son satirda ise "cal_factor_nur" sutunu artik isimize yaramayacagi icin siliyoruz.

    if(nutskirkent == TRUE){

      pers_h[, p_rur_02 := weighted.mean(D_rur_02, w = w)]
      pers_h[, a_rur_02 := weighted.mean(D_rur_02, w = w)]
      pers_h[, w := ifelse(D_rur_02 == 1, w_old*(PF_rur_02/p_rur_02), w_old*((1-a_rur_02*(PF_rur_02/p_rur_02))/(1-a_rur_02)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_03 := weighted.mean(D_rur_03, w = w)]
      pers_h[, a_rur_03 := weighted.mean(D_rur_03, w = w)]
      pers_h[, w := ifelse(D_rur_03 == 1, w_old*(PF_rur_03/p_rur_03), w_old*((1-a_rur_03*(PF_rur_03/p_rur_03))/(1-a_rur_03)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_04 := weighted.mean(D_rur_04, w = w)]
      pers_h[, a_rur_04 := weighted.mean(D_rur_04, w = w)]
      pers_h[, w := ifelse(D_rur_04 == 1, w_old*(PF_rur_04/p_rur_04), w_old*((1-a_rur_04*(PF_rur_04/p_rur_04))/(1-a_rur_04)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_05 := weighted.mean(D_rur_05, w = w)]
      pers_h[, a_rur_05 := weighted.mean(D_rur_05, w = w)]
      pers_h[, w := ifelse(D_rur_05 == 1, w_old*(PF_rur_05/p_rur_05), w_old*((1-a_rur_05*(PF_rur_05/p_rur_05))/(1-a_rur_05)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_06 := weighted.mean(D_rur_06, w = w)]
      pers_h[, a_rur_06 := weighted.mean(D_rur_06, w = w)]
      pers_h[, w := ifelse(D_rur_06 == 1, w_old*(PF_rur_06/p_rur_06), w_old*((1-a_rur_06*(PF_rur_06/p_rur_06))/(1-a_rur_06)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_07 := weighted.mean(D_rur_07, w = w)]
      pers_h[, a_rur_07 := weighted.mean(D_rur_07, w = w)]
      pers_h[, w := ifelse(D_rur_07 == 1, w_old*(PF_rur_07/p_rur_07), w_old*((1-a_rur_07*(PF_rur_07/p_rur_07))/(1-a_rur_07)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_08 := weighted.mean(D_rur_08, w = w)]
      pers_h[, a_rur_08 := weighted.mean(D_rur_08, w = w)]
      pers_h[, w := ifelse(D_rur_08 == 1, w_old*(PF_rur_08/p_rur_08), w_old*((1-a_rur_08*(PF_rur_08/p_rur_08))/(1-a_rur_08)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_09 := weighted.mean(D_rur_09, w = w)]
      pers_h[, a_rur_09 := weighted.mean(D_rur_09, w = w)]
      pers_h[, w := ifelse(D_rur_09 == 1, w_old*(PF_rur_09/p_rur_09), w_old*((1-a_rur_09*(PF_rur_09/p_rur_09))/(1-a_rur_09)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_10 := weighted.mean(D_rur_10, w = w)]
      pers_h[, a_rur_10 := weighted.mean(D_rur_10, w = w)]
      pers_h[, w := ifelse(D_rur_10 == 1, w_old*(PF_rur_10/p_rur_10), w_old*((1-a_rur_10*(PF_rur_10/p_rur_10))/(1-a_rur_10)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_11 := weighted.mean(D_rur_11, w = w)]
      pers_h[, a_rur_11 := weighted.mean(D_rur_11, w = w)]
      pers_h[, w := ifelse(D_rur_11 == 1, w_old*(PF_rur_11/p_rur_11), w_old*((1-a_rur_11*(PF_rur_11/p_rur_11))/(1-a_rur_11)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_12 := weighted.mean(D_rur_12, w = w)]
      pers_h[, a_rur_12 := weighted.mean(D_rur_12, w = w)]
      pers_h[, w := ifelse(D_rur_12 == 1, w_old*(PF_rur_12/p_rur_12), w_old*((1-a_rur_12*(PF_rur_12/p_rur_12))/(1-a_rur_12)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_13 := weighted.mean(D_rur_13, w = w)]
      pers_h[, a_rur_13 := weighted.mean(D_rur_13, w = w)]
      pers_h[, w := ifelse(D_rur_13 == 1, w_old*(PF_rur_13/p_rur_13), w_old*((1-a_rur_13*(PF_rur_13/p_rur_13))/(1-a_rur_13)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_14 := weighted.mean(D_rur_14, w = w)]
      pers_h[, a_rur_14 := weighted.mean(D_rur_14, w = w)]
      pers_h[, w := ifelse(D_rur_14 == 1, w_old*(PF_rur_14/p_rur_14), w_old*((1-a_rur_14*(PF_rur_14/p_rur_14))/(1-a_rur_14)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_15 := weighted.mean(D_rur_15, w = w)]
      pers_h[, a_rur_15 := weighted.mean(D_rur_15, w = w)]
      pers_h[, w := ifelse(D_rur_15 == 1, w_old*(PF_rur_15/p_rur_15), w_old*((1-a_rur_15*(PF_rur_15/p_rur_15))/(1-a_rur_15)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_16 := weighted.mean(D_rur_16, w = w)]
      pers_h[, a_rur_16 := weighted.mean(D_rur_16, w = w)]
      pers_h[, w := ifelse(D_rur_16 == 1, w_old*(PF_rur_16/p_rur_16), w_old*((1-a_rur_16*(PF_rur_16/p_rur_16))/(1-a_rur_16)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_17 := weighted.mean(D_rur_17, w = w)]
      pers_h[, a_rur_17 := weighted.mean(D_rur_17, w = w)]
      pers_h[, w := ifelse(D_rur_17 == 1, w_old*(PF_rur_17/p_rur_17), w_old*((1-a_rur_17*(PF_rur_17/p_rur_17))/(1-a_rur_17)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_18 := weighted.mean(D_rur_18, w = w)]
      pers_h[, a_rur_18 := weighted.mean(D_rur_18, w = w)]
      pers_h[, w := ifelse(D_rur_18 == 1, w_old*(PF_rur_18/p_rur_18), w_old*((1-a_rur_18*(PF_rur_18/p_rur_18))/(1-a_rur_18)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_19 := weighted.mean(D_rur_19, w = w)]
      pers_h[, a_rur_19 := weighted.mean(D_rur_19, w = w)]
      pers_h[, w := ifelse(D_rur_19 == 1, w_old*(PF_rur_19/p_rur_19), w_old*((1-a_rur_19*(PF_rur_19/p_rur_19))/(1-a_rur_19)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_20 := weighted.mean(D_rur_20, w = w)]
      pers_h[, a_rur_20 := weighted.mean(D_rur_20, w = w)]
      pers_h[, w := ifelse(D_rur_20 == 1, w_old*(PF_rur_20/p_rur_20), w_old*((1-a_rur_20*(PF_rur_20/p_rur_20))/(1-a_rur_20)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_21 := weighted.mean(D_rur_21, w = w)]
      pers_h[, a_rur_21 := weighted.mean(D_rur_21, w = w)]
      pers_h[, w := ifelse(D_rur_21 == 1, w_old*(PF_rur_21/p_rur_21), w_old*((1-a_rur_21*(PF_rur_21/p_rur_21))/(1-a_rur_21)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_22 := weighted.mean(D_rur_22, w = w)]
      pers_h[, a_rur_22 := weighted.mean(D_rur_22, w = w)]
      pers_h[, w := ifelse(D_rur_22 == 1, w_old*(PF_rur_22/p_rur_22), w_old*((1-a_rur_22*(PF_rur_22/p_rur_22))/(1-a_rur_22)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_23 := weighted.mean(D_rur_23, w = w)]
      pers_h[, a_rur_23 := weighted.mean(D_rur_23, w = w)]
      pers_h[, w := ifelse(D_rur_23 == 1, w_old*(PF_rur_23/p_rur_23), w_old*((1-a_rur_23*(PF_rur_23/p_rur_23))/(1-a_rur_23)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_24 := weighted.mean(D_rur_24, w = w)]
      pers_h[, a_rur_24 := weighted.mean(D_rur_24, w = w)]
      pers_h[, w := ifelse(D_rur_24 == 1, w_old*(PF_rur_24/p_rur_24), w_old*((1-a_rur_24*(PF_rur_24/p_rur_24))/(1-a_rur_24)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_25 := weighted.mean(D_rur_25, w = w)]
      pers_h[, a_rur_25 := weighted.mean(D_rur_25, w = w)]
      pers_h[, w := ifelse(D_rur_25 == 1, w_old*(PF_rur_25/p_rur_25), w_old*((1-a_rur_25*(PF_rur_25/p_rur_25))/(1-a_rur_25)))]
      pers_h[, w_old := w]

      pers_h[, p_rur_26 := weighted.mean(D_rur_26, w = w)]
      pers_h[, a_rur_26 := weighted.mean(D_rur_26, w = w)]
      pers_h[, w := ifelse(D_rur_26 == 1, w_old*(PF_rur_26/p_rur_26), w_old*((1-a_rur_26*(PF_rur_26/p_rur_26))/(1-a_rur_26)))]
      pers_h[, w_old := w]




      pers_h[, p_urb_01 := weighted.mean(D_urb_01, w = w)]
      pers_h[, a_urb_01 := weighted.mean(D_urb_01, w = w)]
      pers_h[, w := ifelse(D_urb_01 == 1, w_old*(PF_urb_01/p_urb_01), w_old*((1-a_urb_01*(PF_urb_01/p_urb_01))/(1-a_urb_01)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_02 := weighted.mean(D_urb_02, w = w)]
      pers_h[, a_urb_02 := weighted.mean(D_urb_02, w = w)]
      pers_h[, w := ifelse(D_urb_02 == 1, w_old*(PF_urb_02/p_urb_02), w_old*((1-a_urb_02*(PF_urb_02/p_urb_02))/(1-a_urb_02)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_03 := weighted.mean(D_urb_03, w = w)]
      pers_h[, a_urb_03 := weighted.mean(D_urb_03, w = w)]
      pers_h[, w := ifelse(D_urb_03 == 1, w_old*(PF_urb_03/p_urb_03), w_old*((1-a_urb_03*(PF_urb_03/p_urb_03))/(1-a_urb_03)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_04 := weighted.mean(D_urb_04, w = w)]
      pers_h[, a_urb_04 := weighted.mean(D_urb_04, w = w)]
      pers_h[, w := ifelse(D_urb_04 == 1, w_old*(PF_urb_04/p_urb_04), w_old*((1-a_urb_04*(PF_urb_04/p_urb_04))/(1-a_urb_04)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_05 := weighted.mean(D_urb_05, w = w)]
      pers_h[, a_urb_05 := weighted.mean(D_urb_05, w = w)]
      pers_h[, w := ifelse(D_urb_05 == 1, w_old*(PF_urb_05/p_urb_05), w_old*((1-a_urb_05*(PF_urb_05/p_urb_05))/(1-a_urb_05)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_06 := weighted.mean(D_urb_06, w = w)]
      pers_h[, a_urb_06 := weighted.mean(D_urb_06, w = w)]
      pers_h[, w := ifelse(D_urb_06 == 1, w_old*(PF_urb_06/p_urb_06), w_old*((1-a_urb_06*(PF_urb_06/p_urb_06))/(1-a_urb_06)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_07 := weighted.mean(D_urb_07, w = w)]
      pers_h[, a_urb_07 := weighted.mean(D_urb_07, w = w)]
      pers_h[, w := ifelse(D_urb_07 == 1, w_old*(PF_urb_07/p_urb_07), w_old*((1-a_urb_07*(PF_urb_07/p_urb_07))/(1-a_urb_07)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_08 := weighted.mean(D_urb_08, w = w)]
      pers_h[, a_urb_08 := weighted.mean(D_urb_08, w = w)]
      pers_h[, w := ifelse(D_urb_08 == 1, w_old*(PF_urb_08/p_urb_08), w_old*((1-a_urb_08*(PF_urb_08/p_urb_08))/(1-a_urb_08)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_09 := weighted.mean(D_urb_09, w = w)]
      pers_h[, a_urb_09 := weighted.mean(D_urb_09, w = w)]
      pers_h[, w := ifelse(D_urb_09 == 1, w_old*(PF_urb_09/p_urb_09), w_old*((1-a_urb_09*(PF_urb_09/p_urb_09))/(1-a_urb_09)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_10 := weighted.mean(D_urb_10, w = w)]
      pers_h[, a_urb_10 := weighted.mean(D_urb_10, w = w)]
      pers_h[, w := ifelse(D_urb_10 == 1, w_old*(PF_urb_10/p_urb_10), w_old*((1-a_urb_10*(PF_urb_10/p_urb_10))/(1-a_urb_10)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_11 := weighted.mean(D_urb_11, w = w)]
      pers_h[, a_urb_11 := weighted.mean(D_urb_11, w = w)]
      pers_h[, w := ifelse(D_urb_11 == 1, w_old*(PF_urb_11/p_urb_11), w_old*((1-a_urb_11*(PF_urb_11/p_urb_11))/(1-a_urb_11)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_12 := weighted.mean(D_urb_12, w = w)]
      pers_h[, a_urb_12 := weighted.mean(D_urb_12, w = w)]
      pers_h[, w := ifelse(D_urb_12 == 1, w_old*(PF_urb_12/p_urb_12), w_old*((1-a_urb_12*(PF_urb_12/p_urb_12))/(1-a_urb_12)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_13 := weighted.mean(D_urb_13, w = w)]
      pers_h[, a_urb_13 := weighted.mean(D_urb_13, w = w)]
      pers_h[, w := ifelse(D_urb_13 == 1, w_old*(PF_urb_13/p_urb_13), w_old*((1-a_urb_13*(PF_urb_13/p_urb_13))/(1-a_urb_13)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_14 := weighted.mean(D_urb_14, w = w)]
      pers_h[, a_urb_14 := weighted.mean(D_urb_14, w = w)]
      pers_h[, w := ifelse(D_urb_14 == 1, w_old*(PF_urb_14/p_urb_14), w_old*((1-a_urb_14*(PF_urb_14/p_urb_14))/(1-a_urb_14)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_15 := weighted.mean(D_urb_15, w = w)]
      pers_h[, a_urb_15 := weighted.mean(D_urb_15, w = w)]
      pers_h[, w := ifelse(D_urb_15 == 1, w_old*(PF_urb_15/p_urb_15), w_old*((1-a_urb_15*(PF_urb_15/p_urb_15))/(1-a_urb_15)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_16 := weighted.mean(D_urb_16, w = w)]
      pers_h[, a_urb_16 := weighted.mean(D_urb_16, w = w)]
      pers_h[, w := ifelse(D_urb_16 == 1, w_old*(PF_urb_16/p_urb_16), w_old*((1-a_urb_16*(PF_urb_16/p_urb_16))/(1-a_urb_16)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_17 := weighted.mean(D_urb_17, w = w)]
      pers_h[, a_urb_17 := weighted.mean(D_urb_17, w = w)]
      pers_h[, w := ifelse(D_urb_17 == 1, w_old*(PF_urb_17/p_urb_17), w_old*((1-a_urb_17*(PF_urb_17/p_urb_17))/(1-a_urb_17)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_18 := weighted.mean(D_urb_18, w = w)]
      pers_h[, a_urb_18 := weighted.mean(D_urb_18, w = w)]
      pers_h[, w := ifelse(D_urb_18 == 1, w_old*(PF_urb_18/p_urb_18), w_old*((1-a_urb_18*(PF_urb_18/p_urb_18))/(1-a_urb_18)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_19 := weighted.mean(D_urb_19, w = w)]
      pers_h[, a_urb_19 := weighted.mean(D_urb_19, w = w)]
      pers_h[, w := ifelse(D_urb_19 == 1, w_old*(PF_urb_19/p_urb_19), w_old*((1-a_urb_19*(PF_urb_19/p_urb_19))/(1-a_urb_19)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_20 := weighted.mean(D_urb_20, w = w)]
      pers_h[, a_urb_20 := weighted.mean(D_urb_20, w = w)]
      pers_h[, w := ifelse(D_urb_20 == 1, w_old*(PF_urb_20/p_urb_20), w_old*((1-a_urb_20*(PF_urb_20/p_urb_20))/(1-a_urb_20)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_21 := weighted.mean(D_urb_21, w = w)]
      pers_h[, a_urb_21 := weighted.mean(D_urb_21, w = w)]
      pers_h[, w := ifelse(D_urb_21 == 1, w_old*(PF_urb_21/p_urb_21), w_old*((1-a_urb_21*(PF_urb_21/p_urb_21))/(1-a_urb_21)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_22 := weighted.mean(D_urb_22, w = w)]
      pers_h[, a_urb_22 := weighted.mean(D_urb_22, w = w)]
      pers_h[, w := ifelse(D_urb_22 == 1, w_old*(PF_urb_22/p_urb_22), w_old*((1-a_urb_22*(PF_urb_22/p_urb_22))/(1-a_urb_22)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_23 := weighted.mean(D_urb_23, w = w)]
      pers_h[, a_urb_23 := weighted.mean(D_urb_23, w = w)]
      pers_h[, w := ifelse(D_urb_23 == 1, w_old*(PF_urb_23/p_urb_23), w_old*((1-a_urb_23*(PF_urb_23/p_urb_23))/(1-a_urb_23)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_24 := weighted.mean(D_urb_24, w = w)]
      pers_h[, a_urb_24 := weighted.mean(D_urb_24, w = w)]
      pers_h[, w := ifelse(D_urb_24 == 1, w_old*(PF_urb_24/p_urb_24), w_old*((1-a_urb_24*(PF_urb_24/p_urb_24))/(1-a_urb_24)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_25 := weighted.mean(D_urb_25, w = w)]
      pers_h[, a_urb_25 := weighted.mean(D_urb_25, w = w)]
      pers_h[, w := ifelse(D_urb_25 == 1, w_old*(PF_urb_25/p_urb_25), w_old*((1-a_urb_25*(PF_urb_25/p_urb_25))/(1-a_urb_25)))]
      pers_h[, w_old := w]

      pers_h[, p_urb_26 := weighted.mean(D_urb_26, w = w)]
      pers_h[, a_urb_26 := weighted.mean(D_urb_26, w = w)]
      pers_h[, w := ifelse(D_urb_26 == 1, w_old*(PF_urb_26/p_urb_26), w_old*((1-a_urb_26*(PF_urb_26/p_urb_26))/(1-a_urb_26)))]
      pers_h[, w_old := w]

      pers_h <- pers_h[, -(p_rur_02:a_urb_26)]
    }

    # BURAYA KADARKI KISIMDA,YANI KULLANICI "HHB-KIRKENT, YAS-CINS, IL, NUTS2-KIRKENT"
    # SIRASINA GORE BIR KALIBRASYON YAPILMIS OLUYOR.

    ### --------------------------------------------------------------------------------------------------- ###
    ### --------------------------------------------------------------------------------------------------- ###

    # Normalde kullanici "trim" inputu icin herhangi birsey girmediyse asagidaki FOR dongusu hata veriyordu.
    # O yuzden "trimming_1" degiskeni NA oldugunda 1 degerini atiyoruz. Bu, 1 tane trim yapilacak anlamina
    # gelmiyor. Cunku FOR dongusunun sonunda gorulecegi uzere kod TRIM'e "i != trimming_1" oldugu surece
    # giriyor. Dolayisiyla daha ilk dongude "i != 1" sarti saglanmayacagi icin TRIM'e girmeyecek.
    # Bu da demek oluyor ki uygulama ekraninda "trimming_1" degiskenine 4 yazildigi zaman 3 kere trime girecektir.

    trimming_1 <- trimsay
    iterasyon_1 <- calsay

    if(trimyap == FALSE) {
      trimming_1 <- 1
    }


    for(i in 1:trimming_1){
      for(j in 1:iterasyon_1){

        # Cal 1 : HHB - Kirkent Kalibrasyonu

        # ONEMLI !!!!!
        # "gk_arrange" fonksiyonunda yer alan "hhbsay" argumaniyla veri setinin kac hhb grubuna gore
        # olusturulacagini kullanici belirliyordu. Burada kalibrasyon ve trim islemlerine girmeden once de
        # ayni "hhbsay" degerinin girilmesi gerekiyor. "gk_arrange" fonksiyonunda "hhbsay = 6",
        # "gk_caltrim" fonksiyonunda "hhbsay = 3" girecek olursak veri seti ve komutlar uyusmayacagi icin
        # hata verecektir.

        # Ayni sey "hhbkirkent" argumani icin de gecerli. "gk_arrange" fonksiyonunda "hhbkirkent = TRUE" girip,
        # "gk_caltrim" fonksiyonunda "hhbkirkent = FALSE" girilecek olursa veri seti ve komutlar uyusmayacagi
        # icin hata verecektir.

        # Ikinci for dongusunde gorulecegi uzere dongu 1'den kullanicinin belirledigi HHB grup sayisina kadar
        # ilerliyor. Dolayisiyla kullanici tarafindan belirtilen sayida gruba gore kodlar kendiliginden olusuyor.

        if(hhbkirkent == TRUE) {
          for(kirkent_kategori in 1:2) {
            for(hhb_kategori in 1:hhb_girilen) {
              eval(parse(text = paste0("pers_h[, p_hhb_kk_", kirkent_kategori, "_", hhb_kategori,
                                       ":= weighted.mean(D_hhb_kk_", kirkent_kategori, "_", hhb_kategori, ", w=w)]")))
              eval(parse(text = paste0("pers_h[, a_hhb_kk_", kirkent_kategori, "_", hhb_kategori,
                                       ":= weighted.mean(D_hhb_kk_", kirkent_kategori, "_", hhb_kategori, ", w=w)]")))
              eval(parse(text = paste0("pers_h[, w := ifelse(D_hhb_kk_", kirkent_kategori, "_", hhb_kategori, "==1, w_old*(PF_hhb_kk_", kirkent_kategori, "_", hhb_kategori, "/ p_hhb_kk_", kirkent_kategori, "_", hhb_kategori, ")",
                                       ", w_old*((1-a_hhb_kk_", kirkent_kategori, "_", hhb_kategori, "*(PF_hhb_kk_", kirkent_kategori, "_", hhb_kategori, "/p_hhb_kk_", kirkent_kategori, "_", hhb_kategori, "))/(1-a_hhb_kk_", kirkent_kategori, "_", hhb_kategori, ")))]")))
              eval(parse(text = paste0("pers_h[, w_old := w]")))
            }
          }
          # pers_h <- pers_h[, -(p_hhb_kk_1_1:a_hhb_kk_2_9)]
        }

        if(hhbkirkent == FALSE) {
          for(hhb_kategori in 1:hhb_girilen) {
            eval(parse(text = paste0("pers_h[, p_hhb_", hhb_kategori, ":= weighted.mean(D_hhb_", hhb_kategori, ", w=w)]")))
            eval(parse(text = paste0("pers_h[, a_hhb_", hhb_kategori, ":= weighted.mean(D_hhb_", hhb_kategori, ", w=w)]")))
            eval(parse(text = paste0("pers_h[, w := ifelse(D_hhb_", hhb_kategori, "==1, w_old*(PF_hhb_", hhb_kategori, "/ p_hhb_", hhb_kategori, ")",
                                     ", w_old*((1-a_hhb_", hhb_kategori, "*(PF_hhb_", hhb_kategori, "/p_hhb_", hhb_kategori, "))/(1-a_hhb_", hhb_kategori, ")))]")))
            eval(parse(text = paste0("pers_h[, w_old := w]")))
          }
          # pers_h <- pers_h[, -(p_hhb_kk_1_1:a_hhb_kk_2_9)]
        }


        # Cal 1 : Yas - Cinsiyet Kalibrasyonu

        # Kullanici "yascinsiyet = TRUE" secenegini isaretlerse asagidaki dongulere girecek.
        # "weighted.mean(a, b)" fonksiyonu (a*b / sum(b)) islemini yapar. Burada "D_m_0_4" fert bazinda
        # 0-1 degerlerinden olusur. Eger fert ilgili gruptaysa 1, degilse 0 degerini alir. "p_m_0_4" bize
        # ilgili grupta yer alan fertlerin agirlikli oranini verir, yani (D_m_0_4 * w / sum(w)).
        # "DH_m_0_4" hane bazinda 0-1 degerlerinden olusur. Eger hanede 4 kisi yasiyor ve 1 tanesi "m_0_4" grubundaysa
        # hanenin butun fertlerinin DH_m_0_4 degeri 1 oluyor. Dolayisiyla "a_m_0_4" de icerisinde "m_0_4" yas
        # grubundan en az 1 fert bulunan hanelerin agirlikli oranini veriyor.

        if(yascinsiyet == TRUE){
          pers_h[, p_m_0_4 := weighted.mean(D_m_0_4, w = w)]
          pers_h[, a_m_0_4 := weighted.mean(DH_m_0_4, w = w)]
          pers_h[, w := ifelse(DH_m_0_4 == 1, w_old*(PF_m_0_4/p_m_0_4), w_old*((1-a_m_0_4*(PF_m_0_4/p_m_0_4))/(1-a_m_0_4)))]
          pers_h[, w_old := w]

          pers_h[, p_m_5_11 := weighted.mean(D_m_5_11, w = w)]
          pers_h[, a_m_5_11 := weighted.mean(DH_m_5_11, w = w)]
          pers_h[, w := ifelse(DH_m_5_11 == 1, w_old*(PF_m_5_11/p_m_5_11), w_old*((1-a_m_5_11*(PF_m_5_11/p_m_5_11))/(1-a_m_5_11)))]
          pers_h[, w_old := w]

          pers_h[, p_m_12_14 := weighted.mean(D_m_12_14, w = w)]
          pers_h[, a_m_12_14 := weighted.mean(DH_m_12_14, w = w)]
          pers_h[, w := ifelse(DH_m_12_14 == 1, w_old*(PF_m_12_14/p_m_12_14), w_old*((1-a_m_12_14*(PF_m_12_14/p_m_12_14))/(1-a_m_12_14)))]
          pers_h[, w_old := w]

          pers_h[, p_m_15_17 := weighted.mean(D_m_15_17, w = w)]
          pers_h[, a_m_15_17 := weighted.mean(DH_m_15_17, w = w)]
          pers_h[, w := ifelse(DH_m_15_17 == 1, w_old*(PF_m_15_17/p_m_15_17), w_old*((1-a_m_15_17*(PF_m_15_17/p_m_15_17))/(1-a_m_15_17)))]
          pers_h[, w_old := w]

          pers_h[, p_m_18_20 := weighted.mean(D_m_18_20, w = w)]
          pers_h[, a_m_18_20 := weighted.mean(DH_m_18_20, w = w)]
          pers_h[, w := ifelse(DH_m_18_20 == 1, w_old*(PF_m_18_20/p_m_18_20), w_old*((1-a_m_18_20*(PF_m_18_20/p_m_18_20))/(1-a_m_18_20)))]
          pers_h[, w_old := w]

          pers_h[, p_m_21_24 := weighted.mean(D_m_21_24, w = w)]
          pers_h[, a_m_21_24 := weighted.mean(DH_m_21_24, w = w)]
          pers_h[, w := ifelse(DH_m_21_24 == 1, w_old*(PF_m_21_24/p_m_21_24), w_old*((1-a_m_21_24*(PF_m_21_24/p_m_21_24))/(1-a_m_21_24)))]
          pers_h[, w_old := w]

          pers_h[, p_m_25_29 := weighted.mean(D_m_25_29, w = w)]
          pers_h[, a_m_25_29 := weighted.mean(DH_m_25_29, w = w)]
          pers_h[, w := ifelse(DH_m_25_29 == 1, w_old*(PF_m_25_29/p_m_25_29), w_old*((1-a_m_25_29*(PF_m_25_29/p_m_25_29))/(1-a_m_25_29)))]
          pers_h[, w_old := w]

          pers_h[, p_m_30_34 := weighted.mean(D_m_30_34, w = w)]
          pers_h[, a_m_30_34 := weighted.mean(DH_m_30_34, w = w)]
          pers_h[, w := ifelse(DH_m_30_34 == 1, w_old*(PF_m_30_34/p_m_30_34), w_old*((1-a_m_30_34*(PF_m_30_34/p_m_30_34))/(1-a_m_30_34)))]
          pers_h[, w_old := w]

          pers_h[, p_m_35_39 := weighted.mean(D_m_35_39, w = w)]
          pers_h[, a_m_35_39:= weighted.mean(DH_m_35_39, w = w)]
          pers_h[, w := ifelse(DH_m_35_39 == 1, w_old*(PF_m_35_39/p_m_35_39), w_old*((1-a_m_35_39*(PF_m_35_39/p_m_35_39))/(1-a_m_35_39)))]
          pers_h[, w_old := w]

          pers_h[, p_m_40_44 := weighted.mean(D_m_40_44, w = w)]
          pers_h[, a_m_40_44 := weighted.mean(DH_m_40_44, w = w)]
          pers_h[, w := ifelse(DH_m_40_44 == 1, w_old*(PF_m_40_44/p_m_40_44), w_old*((1-a_m_40_44*(PF_m_40_44/p_m_40_44))/(1-a_m_40_44)))]
          pers_h[, w_old := w]

          pers_h[, p_m_45_49 := weighted.mean(D_m_45_49, w = w)]
          pers_h[, a_m_45_49 := weighted.mean(DH_m_45_49, w = w)]
          pers_h[, w := ifelse(DH_m_45_49 == 1, w_old*(PF_m_45_49/p_m_45_49), w_old*((1-a_m_45_49*(PF_m_45_49/p_m_45_49))/(1-a_m_45_49)))]
          pers_h[, w_old := w]

          pers_h[, p_m_50_54 := weighted.mean(D_m_50_54, w = w)]
          pers_h[, a_m_50_54 := weighted.mean(DH_m_50_54, w = w)]
          pers_h[, w := ifelse(DH_m_50_54 == 1, w_old*(PF_m_50_54/p_m_50_54), w_old*((1-a_m_50_54*(PF_m_50_54/p_m_50_54))/(1-a_m_50_54)))]
          pers_h[, w_old := w]

          pers_h[, p_m_55_59:= weighted.mean(D_m_55_59, w = w)]
          pers_h[, a_m_55_59 := weighted.mean(DH_m_55_59, w = w)]
          pers_h[, w := ifelse(DH_m_55_59 == 1, w_old*(PF_m_55_59/p_m_55_59), w_old*((1-a_m_55_59*(PF_m_55_59/p_m_55_59))/(1-a_m_55_59)))]
          pers_h[, w_old := w]

          pers_h[, p_m_60_64 := weighted.mean(D_m_60_64, w = w)]
          pers_h[, a_m_60_64 := weighted.mean(DH_m_60_64, w = w)]
          pers_h[, w := ifelse(DH_m_60_64 == 1, w_old*(PF_m_60_64/p_m_60_64), w_old*((1-a_m_60_64*(PF_m_60_64/p_m_60_64))/(1-a_m_60_64)))]
          pers_h[, w_old := w]

          pers_h[, p_m_65_74 := weighted.mean(D_m_65_74, w = w)]
          pers_h[, a_m_65_74 := weighted.mean(DH_m_65_74, w = w)]
          pers_h[, w := ifelse(DH_m_65_74 == 1, w_old*(PF_m_65_74/p_m_65_74), w_old*((1-a_m_65_74*(PF_m_65_74/p_m_65_74))/(1-a_m_65_74)))]
          pers_h[, w_old := w]

          pers_h[, p_m_over75 := weighted.mean(D_m_over75, w = w)]
          pers_h[, a_m_over75:= weighted.mean(DH_m_over75, w = w)]
          pers_h[, w := ifelse(DH_m_over75 == 1, w_old*(PF_m_over75/p_m_over75), w_old*((1-a_m_over75*(PF_m_over75/p_m_over75))/(1-a_m_over75)))]
          pers_h[, w_old := w]

          pers_h[, p_f_0_4 := weighted.mean(D_f_0_4, w = w)]
          pers_h[, a_f_0_4 := weighted.mean(DH_f_0_4, w = w)]
          pers_h[, w := ifelse(DH_f_0_4 == 1, w_old*(PF_f_0_4/p_f_0_4), w_old*((1-a_f_0_4*(PF_f_0_4/p_f_0_4))/(1-a_f_0_4)))]
          pers_h[, w_old := w]

          pers_h[, p_f_5_11 := weighted.mean(D_f_5_11, w = w)]
          pers_h[, a_f_5_11 := weighted.mean(DH_f_5_11, w = w)]
          pers_h[, w := ifelse(DH_f_5_11 == 1, w_old*(PF_f_5_11/p_f_5_11), w_old*((1-a_f_5_11*(PF_f_5_11/p_f_5_11))/(1-a_f_5_11)))]
          pers_h[, w_old := w]

          pers_h[, p_f_12_14 := weighted.mean(D_f_12_14, w = w)]
          pers_h[, a_f_12_14 := weighted.mean(DH_f_12_14, w = w)]
          pers_h[, w := ifelse(DH_f_12_14 == 1, w_old*(PF_f_12_14/p_f_12_14), w_old*((1-a_f_12_14*(PF_f_12_14/p_f_12_14))/(1-a_f_12_14)))]
          pers_h[, w_old := w]

          pers_h[, p_f_15_17 := weighted.mean(D_f_15_17, w = w)]
          pers_h[, a_f_15_17 := weighted.mean(DH_f_15_17, w = w)]
          pers_h[, w := ifelse(DH_f_15_17 == 1, w_old*(PF_f_15_17/p_f_15_17), w_old*((1-a_f_15_17*(PF_f_15_17/p_f_15_17))/(1-a_f_15_17)))]
          pers_h[, w_old := w]

          pers_h[, p_f_18_20 := weighted.mean(D_f_18_20, w = w)]
          pers_h[, a_f_18_20 := weighted.mean(DH_f_18_20, w = w)]
          pers_h[, w := ifelse(DH_f_18_20 == 1, w_old*(PF_f_18_20/p_f_18_20), w_old*((1-a_f_18_20*(PF_f_18_20/p_f_18_20))/(1-a_f_18_20)))]
          pers_h[, w_old := w]

          pers_h[, p_f_21_24 := weighted.mean(D_f_21_24, w = w)]
          pers_h[, a_f_21_24 := weighted.mean(DH_f_21_24, w = w)]
          pers_h[, w := ifelse(DH_f_21_24 == 1, w_old*(PF_f_21_24/p_f_21_24), w_old*((1-a_f_21_24*(PF_f_21_24/p_f_21_24))/(1-a_f_21_24)))]
          pers_h[, w_old := w]

          pers_h[, p_f_25_29 := weighted.mean(D_f_25_29, w = w)]
          pers_h[, a_f_25_29 := weighted.mean(DH_f_25_29, w = w)]
          pers_h[, w := ifelse(DH_f_25_29 == 1, w_old*(PF_f_25_29/p_f_25_29), w_old*((1-a_f_25_29*(PF_f_25_29/p_f_25_29))/(1-a_f_25_29)))]
          pers_h[, w_old := w]

          pers_h[, p_f_30_34 := weighted.mean(D_f_30_34, w = w)]
          pers_h[, a_f_30_34 := weighted.mean(DH_f_30_34, w = w)]
          pers_h[, w := ifelse(DH_f_30_34 == 1, w_old*(PF_f_30_34/p_f_30_34), w_old*((1-a_f_30_34*(PF_f_30_34/p_f_30_34))/(1-a_f_30_34)))]
          pers_h[, w_old := w]

          pers_h[, p_f_35_39 := weighted.mean(D_f_35_39, w = w)]
          pers_h[, a_f_35_39:= weighted.mean(DH_f_35_39, w = w)]
          pers_h[, w := ifelse(DH_f_35_39 == 1, w_old*(PF_f_35_39/p_f_35_39), w_old*((1-a_f_35_39*(PF_f_35_39/p_f_35_39))/(1-a_f_35_39)))]
          pers_h[, w_old := w]

          pers_h[, p_f_40_44 := weighted.mean(D_f_40_44, w = w)]
          pers_h[, a_f_40_44 := weighted.mean(DH_f_40_44, w = w)]
          pers_h[, w := ifelse(DH_f_40_44 == 1, w_old*(PF_f_40_44/p_f_40_44), w_old*((1-a_f_40_44*(PF_f_40_44/p_f_40_44))/(1-a_f_40_44)))]
          pers_h[, w_old := w]

          pers_h[, p_f_45_49 := weighted.mean(D_f_45_49, w = w)]
          pers_h[, a_f_45_49 := weighted.mean(DH_f_45_49, w = w)]
          pers_h[, w := ifelse(DH_f_45_49 == 1, w_old*(PF_f_45_49/p_f_45_49), w_old*((1-a_f_45_49*(PF_f_45_49/p_f_45_49))/(1-a_f_45_49)))]
          pers_h[, w_old := w]

          pers_h[, p_f_50_54 := weighted.mean(D_f_50_54, w = w)]
          pers_h[, a_f_50_54 := weighted.mean(DH_f_50_54, w = w)]
          pers_h[, w := ifelse(DH_f_50_54 == 1, w_old*(PF_f_50_54/p_f_50_54), w_old*((1-a_f_50_54*(PF_f_50_54/p_f_50_54))/(1-a_f_50_54)))]
          pers_h[, w_old := w]

          pers_h[, p_f_55_59:= weighted.mean(D_f_55_59, w = w)]
          pers_h[, a_f_55_59 := weighted.mean(DH_f_55_59, w = w)]
          pers_h[, w := ifelse(DH_f_55_59 == 1, w_old*(PF_f_55_59/p_f_55_59), w_old*((1-a_f_55_59*(PF_f_55_59/p_f_55_59))/(1-a_f_55_59)))]
          pers_h[, w_old := w]

          pers_h[, p_f_60_64 := weighted.mean(D_f_60_64, w = w)]
          pers_h[, a_f_60_64 := weighted.mean(DH_f_60_64, w = w)]
          pers_h[, w := ifelse(DH_f_60_64 == 1, w_old*(PF_f_60_64/p_f_60_64), w_old*((1-a_f_60_64*(PF_f_60_64/p_f_60_64))/(1-a_f_60_64)))]
          pers_h[, w_old := w]

          pers_h[, p_f_65_74 := weighted.mean(D_f_65_74, w = w)]
          pers_h[, a_f_65_74 := weighted.mean(DH_f_65_74, w = w)]
          pers_h[, w := ifelse(DH_f_65_74 == 1, w_old*(PF_f_65_74/p_f_65_74), w_old*((1-a_f_65_74*(PF_f_65_74/p_f_65_74))/(1-a_f_65_74)))]
          pers_h[, w_old := w]

          pers_h[, p_f_over75 := weighted.mean(D_f_over75, w = w)]
          pers_h[, a_f_over75:= weighted.mean(DH_f_over75, w = w)]
          pers_h[, w := ifelse(DH_f_over75 == 1, w_old*(PF_f_over75/p_f_over75), w_old*((1-a_f_over75*(PF_f_over75/p_f_over75))/(1-a_f_over75)))]
          pers_h[, w_old := w]
          pers_h <- pers_h[, -(p_m_0_4:a_f_over75)]
        }

        # Cal 1 : Il Kalibrasyonu

        # Kullanici uygulama ekraninda "IL" secenegini isaretlerse asagidaki dongulere girecek.
        # IL bazinda w'lari toplatarak arastirmadan elde edilen oranlari hesapliyoruz. Daha sonra
        # projekteden gelen oranlara gore duzeltme yaparak w sutununu guncelliyoruz.
        # Son satirda ise "cal_factor_il" sutunu artik isimize yaramayacagi icin siliyoruz.

        if(il == TRUE){

          pers_h[, p_il_01 := weighted.mean(D_il_01, w = w)]
          pers_h[, a_il_01 := weighted.mean(D_il_01, w = w)]
          pers_h[, w := ifelse(D_il_01 == 1, w_old*(PF_il_01/p_il_01), w_old*((1-a_il_01*(PF_il_01/p_il_01))/(1-a_il_01)))]
          pers_h[, w_old := w]

          pers_h[, p_il_02 := weighted.mean(D_il_02, w = w)]
          pers_h[, a_il_02 := weighted.mean(D_il_02, w = w)]
          pers_h[, w := ifelse(D_il_02 == 1, w_old*(PF_il_02/p_il_02), w_old*((1-a_il_02*(PF_il_02/p_il_02))/(1-a_il_02)))]
          pers_h[, w_old := w]

          pers_h[, p_il_03 := weighted.mean(D_il_03, w = w)]
          pers_h[, a_il_03 := weighted.mean(D_il_03, w = w)]
          pers_h[, w := ifelse(D_il_03 == 1, w_old*(PF_il_03/p_il_03), w_old*((1-a_il_03*(PF_il_03/p_il_03))/(1-a_il_03)))]
          pers_h[, w_old := w]

          pers_h[, p_il_04 := weighted.mean(D_il_04, w = w)]
          pers_h[, a_il_04 := weighted.mean(D_il_04, w = w)]
          pers_h[, w := ifelse(D_il_04 == 1, w_old*(PF_il_04/p_il_04), w_old*((1-a_il_04*(PF_il_04/p_il_04))/(1-a_il_04)))]
          pers_h[, w_old := w]

          pers_h[, p_il_05 := weighted.mean(D_il_05, w = w)]
          pers_h[, a_il_05 := weighted.mean(D_il_05, w = w)]
          pers_h[, w := ifelse(D_il_05 == 1, w_old*(PF_il_05/p_il_05), w_old*((1-a_il_05*(PF_il_05/p_il_05))/(1-a_il_05)))]
          pers_h[, w_old := w]

          pers_h[, p_il_06 := weighted.mean(D_il_06, w = w)]
          pers_h[, a_il_06 := weighted.mean(D_il_06, w = w)]
          pers_h[, w := ifelse(D_il_06 == 1, w_old*(PF_il_06/p_il_06), w_old*((1-a_il_06*(PF_il_06/p_il_06))/(1-a_il_06)))]
          pers_h[, w_old := w]

          pers_h[, p_il_07 := weighted.mean(D_il_07, w = w)]
          pers_h[, a_il_07 := weighted.mean(D_il_07, w = w)]
          pers_h[, w := ifelse(D_il_07 == 1, w_old*(PF_il_07/p_il_07), w_old*((1-a_il_07*(PF_il_07/p_il_07))/(1-a_il_07)))]
          pers_h[, w_old := w]

          pers_h[, p_il_08 := weighted.mean(D_il_08, w = w)]
          pers_h[, a_il_08 := weighted.mean(D_il_08, w = w)]
          pers_h[, w := ifelse(D_il_08 == 1, w_old*(PF_il_08/p_il_08), w_old*((1-a_il_08*(PF_il_08/p_il_08))/(1-a_il_08)))]
          pers_h[, w_old := w]

          pers_h[, p_il_09 := weighted.mean(D_il_09, w = w)]
          pers_h[, a_il_09 := weighted.mean(D_il_09, w = w)]
          pers_h[, w := ifelse(D_il_09 == 1, w_old*(PF_il_09/p_il_09), w_old*((1-a_il_09*(PF_il_09/p_il_09))/(1-a_il_09)))]
          pers_h[, w_old := w]

          pers_h[, p_il_10 := weighted.mean(D_il_10, w = w)]
          pers_h[, a_il_10 := weighted.mean(D_il_10, w = w)]
          pers_h[, w := ifelse(D_il_10 == 1, w_old*(PF_il_10/p_il_10), w_old*((1-a_il_10*(PF_il_10/p_il_10))/(1-a_il_10)))]
          pers_h[, w_old := w]

          pers_h[, p_il_11 := weighted.mean(D_il_11, w = w)]
          pers_h[, a_il_11 := weighted.mean(D_il_11, w = w)]
          pers_h[, w := ifelse(D_il_11 == 1, w_old*(PF_il_11/p_il_11), w_old*((1-a_il_11*(PF_il_11/p_il_11))/(1-a_il_11)))]
          pers_h[, w_old := w]

          pers_h[, p_il_12 := weighted.mean(D_il_12, w = w)]
          pers_h[, a_il_12 := weighted.mean(D_il_12, w = w)]
          pers_h[, w := ifelse(D_il_12 == 1, w_old*(PF_il_12/p_il_12), w_old*((1-a_il_12*(PF_il_12/p_il_12))/(1-a_il_12)))]
          pers_h[, w_old := w]

          pers_h[, p_il_13 := weighted.mean(D_il_13, w = w)]
          pers_h[, a_il_13 := weighted.mean(D_il_13, w = w)]
          pers_h[, w := ifelse(D_il_13 == 1, w_old*(PF_il_13/p_il_13), w_old*((1-a_il_13*(PF_il_13/p_il_13))/(1-a_il_13)))]
          pers_h[, w_old := w]

          pers_h[, p_il_14 := weighted.mean(D_il_14, w = w)]
          pers_h[, a_il_14 := weighted.mean(D_il_14, w = w)]
          pers_h[, w := ifelse(D_il_14 == 1, w_old*(PF_il_14/p_il_14), w_old*((1-a_il_14*(PF_il_14/p_il_14))/(1-a_il_14)))]
          pers_h[, w_old := w]

          pers_h[, p_il_15 := weighted.mean(D_il_15, w = w)]
          pers_h[, a_il_15 := weighted.mean(D_il_15, w = w)]
          pers_h[, w := ifelse(D_il_15 == 1, w_old*(PF_il_15/p_il_15), w_old*((1-a_il_15*(PF_il_15/p_il_15))/(1-a_il_15)))]
          pers_h[, w_old := w]

          pers_h[, p_il_16 := weighted.mean(D_il_16, w = w)]
          pers_h[, a_il_16 := weighted.mean(D_il_16, w = w)]
          pers_h[, w := ifelse(D_il_16 == 1, w_old*(PF_il_16/p_il_16), w_old*((1-a_il_16*(PF_il_16/p_il_16))/(1-a_il_16)))]
          pers_h[, w_old := w]

          pers_h[, p_il_17 := weighted.mean(D_il_17, w = w)]
          pers_h[, a_il_17 := weighted.mean(D_il_17, w = w)]
          pers_h[, w := ifelse(D_il_17 == 1, w_old*(PF_il_17/p_il_17), w_old*((1-a_il_17*(PF_il_17/p_il_17))/(1-a_il_17)))]
          pers_h[, w_old := w]

          pers_h[, p_il_18 := weighted.mean(D_il_18, w = w)]
          pers_h[, a_il_18 := weighted.mean(D_il_18, w = w)]
          pers_h[, w := ifelse(D_il_18 == 1, w_old*(PF_il_18/p_il_18), w_old*((1-a_il_18*(PF_il_18/p_il_18))/(1-a_il_18)))]
          pers_h[, w_old := w]

          pers_h[, p_il_19 := weighted.mean(D_il_19, w = w)]
          pers_h[, a_il_19 := weighted.mean(D_il_19, w = w)]
          pers_h[, w := ifelse(D_il_19 == 1, w_old*(PF_il_19/p_il_19), w_old*((1-a_il_19*(PF_il_19/p_il_19))/(1-a_il_19)))]
          pers_h[, w_old := w]

          pers_h[, p_il_20 := weighted.mean(D_il_20, w = w)]
          pers_h[, a_il_20 := weighted.mean(D_il_20, w = w)]
          pers_h[, w := ifelse(D_il_20 == 1, w_old*(PF_il_20/p_il_20), w_old*((1-a_il_20*(PF_il_20/p_il_20))/(1-a_il_20)))]
          pers_h[, w_old := w]

          pers_h[, p_il_21 := weighted.mean(D_il_21, w = w)]
          pers_h[, a_il_21 := weighted.mean(D_il_21, w = w)]
          pers_h[, w := ifelse(D_il_21 == 1, w_old*(PF_il_21/p_il_21), w_old*((1-a_il_21*(PF_il_21/p_il_21))/(1-a_il_21)))]
          pers_h[, w_old := w]

          pers_h[, p_il_22 := weighted.mean(D_il_22, w = w)]
          pers_h[, a_il_22 := weighted.mean(D_il_22, w = w)]
          pers_h[, w := ifelse(D_il_22 == 1, w_old*(PF_il_22/p_il_22), w_old*((1-a_il_22*(PF_il_22/p_il_22))/(1-a_il_22)))]
          pers_h[, w_old := w]

          pers_h[, p_il_23 := weighted.mean(D_il_23, w = w)]
          pers_h[, a_il_23 := weighted.mean(D_il_23, w = w)]
          pers_h[, w := ifelse(D_il_23 == 1, w_old*(PF_il_23/p_il_23), w_old*((1-a_il_23*(PF_il_23/p_il_23))/(1-a_il_23)))]
          pers_h[, w_old := w]

          pers_h[, p_il_24 := weighted.mean(D_il_24, w = w)]
          pers_h[, a_il_24 := weighted.mean(D_il_24, w = w)]
          pers_h[, w := ifelse(D_il_24 == 1, w_old*(PF_il_24/p_il_24), w_old*((1-a_il_24*(PF_il_24/p_il_24))/(1-a_il_24)))]
          pers_h[, w_old := w]

          pers_h[, p_il_25 := weighted.mean(D_il_25, w = w)]
          pers_h[, a_il_25 := weighted.mean(D_il_25, w = w)]
          pers_h[, w := ifelse(D_il_25 == 1, w_old*(PF_il_25/p_il_25), w_old*((1-a_il_25*(PF_il_25/p_il_25))/(1-a_il_25)))]
          pers_h[, w_old := w]

          pers_h[, p_il_26 := weighted.mean(D_il_26, w = w)]
          pers_h[, a_il_26 := weighted.mean(D_il_26, w = w)]
          pers_h[, w := ifelse(D_il_26 == 1, w_old*(PF_il_26/p_il_26), w_old*((1-a_il_26*(PF_il_26/p_il_26))/(1-a_il_26)))]
          pers_h[, w_old := w]

          pers_h[, p_il_27 := weighted.mean(D_il_27, w = w)]
          pers_h[, a_il_27 := weighted.mean(D_il_27, w = w)]
          pers_h[, w := ifelse(D_il_27 == 1, w_old*(PF_il_27/p_il_27), w_old*((1-a_il_27*(PF_il_27/p_il_27))/(1-a_il_27)))]
          pers_h[, w_old := w]

          pers_h[, p_il_28 := weighted.mean(D_il_28, w = w)]
          pers_h[, a_il_28 := weighted.mean(D_il_28, w = w)]
          pers_h[, w := ifelse(D_il_28 == 1, w_old*(PF_il_28/p_il_28), w_old*((1-a_il_28*(PF_il_28/p_il_28))/(1-a_il_28)))]
          pers_h[, w_old := w]

          pers_h[, p_il_29 := weighted.mean(D_il_29, w = w)]
          pers_h[, a_il_29 := weighted.mean(D_il_29, w = w)]
          pers_h[, w := ifelse(D_il_29 == 1, w_old*(PF_il_29/p_il_29), w_old*((1-a_il_29*(PF_il_29/p_il_29))/(1-a_il_29)))]
          pers_h[, w_old := w]

          pers_h[, p_il_30 := weighted.mean(D_il_30, w = w)]
          pers_h[, a_il_30 := weighted.mean(D_il_30, w = w)]
          pers_h[, w := ifelse(D_il_30 == 1, w_old*(PF_il_30/p_il_30), w_old*((1-a_il_30*(PF_il_30/p_il_30))/(1-a_il_30)))]
          pers_h[, w_old := w]

          pers_h[, p_il_31 := weighted.mean(D_il_31, w = w)]
          pers_h[, a_il_31 := weighted.mean(D_il_31, w = w)]
          pers_h[, w := ifelse(D_il_31 == 1, w_old*(PF_il_31/p_il_31), w_old*((1-a_il_31*(PF_il_31/p_il_31))/(1-a_il_31)))]
          pers_h[, w_old := w]

          pers_h[, p_il_32 := weighted.mean(D_il_32, w = w)]
          pers_h[, a_il_32 := weighted.mean(D_il_32, w = w)]
          pers_h[, w := ifelse(D_il_32 == 1, w_old*(PF_il_32/p_il_32), w_old*((1-a_il_32*(PF_il_32/p_il_32))/(1-a_il_32)))]
          pers_h[, w_old := w]

          pers_h[, p_il_33 := weighted.mean(D_il_33, w = w)]
          pers_h[, a_il_33 := weighted.mean(D_il_33, w = w)]
          pers_h[, w := ifelse(D_il_33 == 1, w_old*(PF_il_33/p_il_33), w_old*((1-a_il_33*(PF_il_33/p_il_33))/(1-a_il_33)))]
          pers_h[, w_old := w]

          pers_h[, p_il_34 := weighted.mean(D_il_34, w = w)]
          pers_h[, a_il_34 := weighted.mean(D_il_34, w = w)]
          pers_h[, w := ifelse(D_il_34 == 1, w_old*(PF_il_34/p_il_34), w_old*((1-a_il_34*(PF_il_34/p_il_34))/(1-a_il_34)))]
          pers_h[, w_old := w]

          pers_h[, p_il_35 := weighted.mean(D_il_35, w = w)]
          pers_h[, a_il_35 := weighted.mean(D_il_35, w = w)]
          pers_h[, w := ifelse(D_il_35 == 1, w_old*(PF_il_35/p_il_35), w_old*((1-a_il_35*(PF_il_35/p_il_35))/(1-a_il_35)))]
          pers_h[, w_old := w]

          pers_h[, p_il_36 := weighted.mean(D_il_36, w = w)]
          pers_h[, a_il_36 := weighted.mean(D_il_36, w = w)]
          pers_h[, w := ifelse(D_il_36 == 1, w_old*(PF_il_36/p_il_36), w_old*((1-a_il_36*(PF_il_36/p_il_36))/(1-a_il_36)))]
          pers_h[, w_old := w]

          pers_h[, p_il_37 := weighted.mean(D_il_37, w = w)]
          pers_h[, a_il_37 := weighted.mean(D_il_37, w = w)]
          pers_h[, w := ifelse(D_il_37 == 1, w_old*(PF_il_37/p_il_37), w_old*((1-a_il_37*(PF_il_37/p_il_37))/(1-a_il_37)))]
          pers_h[, w_old := w]

          pers_h[, p_il_38 := weighted.mean(D_il_38, w = w)]
          pers_h[, a_il_38 := weighted.mean(D_il_38, w = w)]
          pers_h[, w := ifelse(D_il_38 == 1, w_old*(PF_il_38/p_il_38), w_old*((1-a_il_38*(PF_il_38/p_il_38))/(1-a_il_38)))]
          pers_h[, w_old := w]

          pers_h[, p_il_39 := weighted.mean(D_il_39, w = w)]
          pers_h[, a_il_39 := weighted.mean(D_il_39, w = w)]
          pers_h[, w := ifelse(D_il_39 == 1, w_old*(PF_il_39/p_il_39), w_old*((1-a_il_39*(PF_il_39/p_il_39))/(1-a_il_39)))]
          pers_h[, w_old := w]

          pers_h[, p_il_40 := weighted.mean(D_il_40, w = w)]
          pers_h[, a_il_40 := weighted.mean(D_il_40, w = w)]
          pers_h[, w := ifelse(D_il_40 == 1, w_old*(PF_il_40/p_il_40), w_old*((1-a_il_40*(PF_il_40/p_il_40))/(1-a_il_40)))]
          pers_h[, w_old := w]

          pers_h[, p_il_41 := weighted.mean(D_il_41, w = w)]
          pers_h[, a_il_41 := weighted.mean(D_il_41, w = w)]
          pers_h[, w := ifelse(D_il_41 == 1, w_old*(PF_il_41/p_il_41), w_old*((1-a_il_41*(PF_il_41/p_il_41))/(1-a_il_41)))]
          pers_h[, w_old := w]

          pers_h[, p_il_42 := weighted.mean(D_il_42, w = w)]
          pers_h[, a_il_42 := weighted.mean(D_il_42, w = w)]
          pers_h[, w := ifelse(D_il_42 == 1, w_old*(PF_il_42/p_il_42), w_old*((1-a_il_42*(PF_il_42/p_il_42))/(1-a_il_42)))]
          pers_h[, w_old := w]

          pers_h[, p_il_43 := weighted.mean(D_il_43, w = w)]
          pers_h[, a_il_43 := weighted.mean(D_il_43, w = w)]
          pers_h[, w := ifelse(D_il_43 == 1, w_old*(PF_il_43/p_il_43), w_old*((1-a_il_43*(PF_il_43/p_il_43))/(1-a_il_43)))]
          pers_h[, w_old := w]

          pers_h[, p_il_44 := weighted.mean(D_il_44, w = w)]
          pers_h[, a_il_44 := weighted.mean(D_il_44, w = w)]
          pers_h[, w := ifelse(D_il_44 == 1, w_old*(PF_il_44/p_il_44), w_old*((1-a_il_44*(PF_il_44/p_il_44))/(1-a_il_44)))]
          pers_h[, w_old := w]

          pers_h[, p_il_45 := weighted.mean(D_il_45, w = w)]
          pers_h[, a_il_45 := weighted.mean(D_il_45, w = w)]
          pers_h[, w := ifelse(D_il_45 == 1, w_old*(PF_il_45/p_il_45), w_old*((1-a_il_45*(PF_il_45/p_il_45))/(1-a_il_45)))]
          pers_h[, w_old := w]

          pers_h[, p_il_46 := weighted.mean(D_il_46, w = w)]
          pers_h[, a_il_46 := weighted.mean(D_il_46, w = w)]
          pers_h[, w := ifelse(D_il_46 == 1, w_old*(PF_il_46/p_il_46), w_old*((1-a_il_46*(PF_il_46/p_il_46))/(1-a_il_46)))]
          pers_h[, w_old := w]

          pers_h[, p_il_47 := weighted.mean(D_il_47, w = w)]
          pers_h[, a_il_47 := weighted.mean(D_il_47, w = w)]
          pers_h[, w := ifelse(D_il_47 == 1, w_old*(PF_il_47/p_il_47), w_old*((1-a_il_47*(PF_il_47/p_il_47))/(1-a_il_47)))]
          pers_h[, w_old := w]

          pers_h[, p_il_48 := weighted.mean(D_il_48, w = w)]
          pers_h[, a_il_48 := weighted.mean(D_il_48, w = w)]
          pers_h[, w := ifelse(D_il_48 == 1, w_old*(PF_il_48/p_il_48), w_old*((1-a_il_48*(PF_il_48/p_il_48))/(1-a_il_48)))]
          pers_h[, w_old := w]

          pers_h[, p_il_49 := weighted.mean(D_il_49, w = w)]
          pers_h[, a_il_49 := weighted.mean(D_il_49, w = w)]
          pers_h[, w := ifelse(D_il_49 == 1, w_old*(PF_il_49/p_il_49), w_old*((1-a_il_49*(PF_il_49/p_il_49))/(1-a_il_49)))]
          pers_h[, w_old := w]

          pers_h[, p_il_50 := weighted.mean(D_il_50, w = w)]
          pers_h[, a_il_50 := weighted.mean(D_il_50, w = w)]
          pers_h[, w := ifelse(D_il_50 == 1, w_old*(PF_il_50/p_il_50), w_old*((1-a_il_50*(PF_il_50/p_il_50))/(1-a_il_50)))]
          pers_h[, w_old := w]

          pers_h[, p_il_51 := weighted.mean(D_il_51, w = w)]
          pers_h[, a_il_51 := weighted.mean(D_il_51, w = w)]
          pers_h[, w := ifelse(D_il_51 == 1, w_old*(PF_il_51/p_il_51), w_old*((1-a_il_51*(PF_il_51/p_il_51))/(1-a_il_51)))]
          pers_h[, w_old := w]

          pers_h[, p_il_52 := weighted.mean(D_il_52, w = w)]
          pers_h[, a_il_52 := weighted.mean(D_il_52, w = w)]
          pers_h[, w := ifelse(D_il_52 == 1, w_old*(PF_il_52/p_il_52), w_old*((1-a_il_52*(PF_il_52/p_il_52))/(1-a_il_52)))]
          pers_h[, w_old := w]

          pers_h[, p_il_53 := weighted.mean(D_il_53, w = w)]
          pers_h[, a_il_53 := weighted.mean(D_il_53, w = w)]
          pers_h[, w := ifelse(D_il_53 == 1, w_old*(PF_il_53/p_il_53), w_old*((1-a_il_53*(PF_il_53/p_il_53))/(1-a_il_53)))]
          pers_h[, w_old := w]

          pers_h[, p_il_54 := weighted.mean(D_il_54, w = w)]
          pers_h[, a_il_54 := weighted.mean(D_il_54, w = w)]
          pers_h[, w := ifelse(D_il_54 == 1, w_old*(PF_il_54/p_il_54), w_old*((1-a_il_54*(PF_il_54/p_il_54))/(1-a_il_54)))]
          pers_h[, w_old := w]

          pers_h[, p_il_55 := weighted.mean(D_il_55, w = w)]
          pers_h[, a_il_55 := weighted.mean(D_il_55, w = w)]
          pers_h[, w := ifelse(D_il_55 == 1, w_old*(PF_il_55/p_il_55), w_old*((1-a_il_55*(PF_il_55/p_il_55))/(1-a_il_55)))]
          pers_h[, w_old := w]

          pers_h[, p_il_56 := weighted.mean(D_il_56, w = w)]
          pers_h[, a_il_56 := weighted.mean(D_il_56, w = w)]
          pers_h[, w := ifelse(D_il_56 == 1, w_old*(PF_il_56/p_il_56), w_old*((1-a_il_56*(PF_il_56/p_il_56))/(1-a_il_56)))]
          pers_h[, w_old := w]

          pers_h[, p_il_57 := weighted.mean(D_il_57, w = w)]
          pers_h[, a_il_57 := weighted.mean(D_il_57, w = w)]
          pers_h[, w := ifelse(D_il_57 == 1, w_old*(PF_il_57/p_il_57), w_old*((1-a_il_57*(PF_il_57/p_il_57))/(1-a_il_57)))]
          pers_h[, w_old := w]

          pers_h[, p_il_58 := weighted.mean(D_il_58, w = w)]
          pers_h[, a_il_58 := weighted.mean(D_il_58, w = w)]
          pers_h[, w := ifelse(D_il_58 == 1, w_old*(PF_il_58/p_il_58), w_old*((1-a_il_58*(PF_il_58/p_il_58))/(1-a_il_58)))]
          pers_h[, w_old := w]

          pers_h[, p_il_59 := weighted.mean(D_il_59, w = w)]
          pers_h[, a_il_59 := weighted.mean(D_il_59, w = w)]
          pers_h[, w := ifelse(D_il_59 == 1, w_old*(PF_il_59/p_il_59), w_old*((1-a_il_59*(PF_il_59/p_il_59))/(1-a_il_59)))]
          pers_h[, w_old := w]

          pers_h[, p_il_60 := weighted.mean(D_il_60, w = w)]
          pers_h[, a_il_60 := weighted.mean(D_il_60, w = w)]
          pers_h[, w := ifelse(D_il_60 == 1, w_old*(PF_il_60/p_il_60), w_old*((1-a_il_60*(PF_il_60/p_il_60))/(1-a_il_60)))]
          pers_h[, w_old := w]

          pers_h[, p_il_61 := weighted.mean(D_il_61, w = w)]
          pers_h[, a_il_61 := weighted.mean(D_il_61, w = w)]
          pers_h[, w := ifelse(D_il_61 == 1, w_old*(PF_il_61/p_il_61), w_old*((1-a_il_61*(PF_il_61/p_il_61))/(1-a_il_61)))]
          pers_h[, w_old := w]

          pers_h[, p_il_62 := weighted.mean(D_il_62, w = w)]
          pers_h[, a_il_62 := weighted.mean(D_il_62, w = w)]
          pers_h[, w := ifelse(D_il_62 == 1, w_old*(PF_il_62/p_il_62), w_old*((1-a_il_62*(PF_il_62/p_il_62))/(1-a_il_62)))]
          pers_h[, w_old := w]

          pers_h[, p_il_63 := weighted.mean(D_il_63, w = w)]
          pers_h[, a_il_63 := weighted.mean(D_il_63, w = w)]
          pers_h[, w := ifelse(D_il_63 == 1, w_old*(PF_il_63/p_il_63), w_old*((1-a_il_63*(PF_il_63/p_il_63))/(1-a_il_63)))]
          pers_h[, w_old := w]

          pers_h[, p_il_64 := weighted.mean(D_il_64, w = w)]
          pers_h[, a_il_64 := weighted.mean(D_il_64, w = w)]
          pers_h[, w := ifelse(D_il_64 == 1, w_old*(PF_il_64/p_il_64), w_old*((1-a_il_64*(PF_il_64/p_il_64))/(1-a_il_64)))]
          pers_h[, w_old := w]

          pers_h[, p_il_65 := weighted.mean(D_il_65, w = w)]
          pers_h[, a_il_65 := weighted.mean(D_il_65, w = w)]
          pers_h[, w := ifelse(D_il_65 == 1, w_old*(PF_il_65/p_il_65), w_old*((1-a_il_65*(PF_il_65/p_il_65))/(1-a_il_65)))]
          pers_h[, w_old := w]

          pers_h[, p_il_66 := weighted.mean(D_il_66, w = w)]
          pers_h[, a_il_66 := weighted.mean(D_il_66, w = w)]
          pers_h[, w := ifelse(D_il_66 == 1, w_old*(PF_il_66/p_il_66), w_old*((1-a_il_66*(PF_il_66/p_il_66))/(1-a_il_66)))]
          pers_h[, w_old := w]

          pers_h[, p_il_67 := weighted.mean(D_il_67, w = w)]
          pers_h[, a_il_67 := weighted.mean(D_il_67, w = w)]
          pers_h[, w := ifelse(D_il_67 == 1, w_old*(PF_il_67/p_il_67), w_old*((1-a_il_67*(PF_il_67/p_il_67))/(1-a_il_67)))]
          pers_h[, w_old := w]

          pers_h[, p_il_68 := weighted.mean(D_il_68, w = w)]
          pers_h[, a_il_68 := weighted.mean(D_il_68, w = w)]
          pers_h[, w := ifelse(D_il_68 == 1, w_old*(PF_il_68/p_il_68), w_old*((1-a_il_68*(PF_il_68/p_il_68))/(1-a_il_68)))]
          pers_h[, w_old := w]

          pers_h[, p_il_69 := weighted.mean(D_il_69, w = w)]
          pers_h[, a_il_69 := weighted.mean(D_il_69, w = w)]
          pers_h[, w := ifelse(D_il_69 == 1, w_old*(PF_il_69/p_il_69), w_old*((1-a_il_69*(PF_il_69/p_il_69))/(1-a_il_69)))]
          pers_h[, w_old := w]

          pers_h[, p_il_70 := weighted.mean(D_il_70, w = w)]
          pers_h[, a_il_70 := weighted.mean(D_il_70, w = w)]
          pers_h[, w := ifelse(D_il_70 == 1, w_old*(PF_il_70/p_il_70), w_old*((1-a_il_70*(PF_il_70/p_il_70))/(1-a_il_70)))]
          pers_h[, w_old := w]

          pers_h[, p_il_71 := weighted.mean(D_il_71, w = w)]
          pers_h[, a_il_71 := weighted.mean(D_il_71, w = w)]
          pers_h[, w := ifelse(D_il_71 == 1, w_old*(PF_il_71/p_il_71), w_old*((1-a_il_71*(PF_il_71/p_il_71))/(1-a_il_71)))]
          pers_h[, w_old := w]

          pers_h[, p_il_72 := weighted.mean(D_il_72, w = w)]
          pers_h[, a_il_72 := weighted.mean(D_il_72, w = w)]
          pers_h[, w := ifelse(D_il_72 == 1, w_old*(PF_il_72/p_il_72), w_old*((1-a_il_72*(PF_il_72/p_il_72))/(1-a_il_72)))]
          pers_h[, w_old := w]

          pers_h[, p_il_73 := weighted.mean(D_il_73, w = w)]
          pers_h[, a_il_73 := weighted.mean(D_il_73, w = w)]
          pers_h[, w := ifelse(D_il_73 == 1, w_old*(PF_il_73/p_il_73), w_old*((1-a_il_73*(PF_il_73/p_il_73))/(1-a_il_73)))]
          pers_h[, w_old := w]

          pers_h[, p_il_74 := weighted.mean(D_il_74, w = w)]
          pers_h[, a_il_74 := weighted.mean(D_il_74, w = w)]
          pers_h[, w := ifelse(D_il_74 == 1, w_old*(PF_il_74/p_il_74), w_old*((1-a_il_74*(PF_il_74/p_il_74))/(1-a_il_74)))]
          pers_h[, w_old := w]

          pers_h[, p_il_75 := weighted.mean(D_il_75, w = w)]
          pers_h[, a_il_75 := weighted.mean(D_il_75, w = w)]
          pers_h[, w := ifelse(D_il_75 == 1, w_old*(PF_il_75/p_il_75), w_old*((1-a_il_75*(PF_il_75/p_il_75))/(1-a_il_75)))]
          pers_h[, w_old := w]

          pers_h[, p_il_76 := weighted.mean(D_il_76, w = w)]
          pers_h[, a_il_76 := weighted.mean(D_il_76, w = w)]
          pers_h[, w := ifelse(D_il_76 == 1, w_old*(PF_il_76/p_il_76), w_old*((1-a_il_76*(PF_il_76/p_il_76))/(1-a_il_76)))]
          pers_h[, w_old := w]

          pers_h[, p_il_77 := weighted.mean(D_il_77, w = w)]
          pers_h[, a_il_77 := weighted.mean(D_il_77, w = w)]
          pers_h[, w := ifelse(D_il_77 == 1, w_old*(PF_il_77/p_il_77), w_old*((1-a_il_77*(PF_il_77/p_il_77))/(1-a_il_77)))]
          pers_h[, w_old := w]

          pers_h[, p_il_78 := weighted.mean(D_il_78, w = w)]
          pers_h[, a_il_78 := weighted.mean(D_il_78, w = w)]
          pers_h[, w := ifelse(D_il_78 == 1, w_old*(PF_il_78/p_il_78), w_old*((1-a_il_78*(PF_il_78/p_il_78))/(1-a_il_78)))]
          pers_h[, w_old := w]

          pers_h[, p_il_79 := weighted.mean(D_il_79, w = w)]
          pers_h[, a_il_79 := weighted.mean(D_il_79, w = w)]
          pers_h[, w := ifelse(D_il_79 == 1, w_old*(PF_il_79/p_il_79), w_old*((1-a_il_79*(PF_il_79/p_il_79))/(1-a_il_79)))]
          pers_h[, w_old := w]

          pers_h[, p_il_80 := weighted.mean(D_il_80, w = w)]
          pers_h[, a_il_80 := weighted.mean(D_il_80, w = w)]
          pers_h[, w := ifelse(D_il_80 == 1, w_old*(PF_il_80/p_il_80), w_old*((1-a_il_80*(PF_il_80/p_il_80))/(1-a_il_80)))]
          pers_h[, w_old := w]

          pers_h[, p_il_81 := weighted.mean(D_il_81, w = w)]
          pers_h[, a_il_81 := weighted.mean(D_il_81, w = w)]
          pers_h[, w := ifelse(D_il_81 == 1, w_old*(PF_il_81/p_il_81), w_old*((1-a_il_81*(PF_il_81/p_il_81))/(1-a_il_81)))]
          pers_h[, w_old := w]

          pers_h <- pers_h[, -(p_il_01:a_il_81)]

        }
        ### -------------------------------------------------- ###

        # Cal 1 : Nuts2 / Kirkent Kalibrasyonu

        # Kullanici "nutskirkent = TRUE" secenegini isaretlerse asagidaki dongulere girecek.
        # Nuts2 - Kirkent bazinda w'lari toplatarak arastirmadan elde edilen oranlari hesapliyoruz. Daha sonra
        # projekteden gelen oranlara gore duzeltme yaparak w sutununu guncelliyoruz.
        # Son satirda ise "cal_factor_nur" sutunu artik isimize yaramayacagi icin siliyoruz.

        if(nutskirkent == TRUE){

          pers_h[, p_rur_02 := weighted.mean(D_rur_02, w = w)]
          pers_h[, a_rur_02 := weighted.mean(D_rur_02, w = w)]
          pers_h[, w := ifelse(D_rur_02 == 1, w_old*(PF_rur_02/p_rur_02), w_old*((1-a_rur_02*(PF_rur_02/p_rur_02))/(1-a_rur_02)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_03 := weighted.mean(D_rur_03, w = w)]
          pers_h[, a_rur_03 := weighted.mean(D_rur_03, w = w)]
          pers_h[, w := ifelse(D_rur_03 == 1, w_old*(PF_rur_03/p_rur_03), w_old*((1-a_rur_03*(PF_rur_03/p_rur_03))/(1-a_rur_03)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_04 := weighted.mean(D_rur_04, w = w)]
          pers_h[, a_rur_04 := weighted.mean(D_rur_04, w = w)]
          pers_h[, w := ifelse(D_rur_04 == 1, w_old*(PF_rur_04/p_rur_04), w_old*((1-a_rur_04*(PF_rur_04/p_rur_04))/(1-a_rur_04)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_05 := weighted.mean(D_rur_05, w = w)]
          pers_h[, a_rur_05 := weighted.mean(D_rur_05, w = w)]
          pers_h[, w := ifelse(D_rur_05 == 1, w_old*(PF_rur_05/p_rur_05), w_old*((1-a_rur_05*(PF_rur_05/p_rur_05))/(1-a_rur_05)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_06 := weighted.mean(D_rur_06, w = w)]
          pers_h[, a_rur_06 := weighted.mean(D_rur_06, w = w)]
          pers_h[, w := ifelse(D_rur_06 == 1, w_old*(PF_rur_06/p_rur_06), w_old*((1-a_rur_06*(PF_rur_06/p_rur_06))/(1-a_rur_06)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_07 := weighted.mean(D_rur_07, w = w)]
          pers_h[, a_rur_07 := weighted.mean(D_rur_07, w = w)]
          pers_h[, w := ifelse(D_rur_07 == 1, w_old*(PF_rur_07/p_rur_07), w_old*((1-a_rur_07*(PF_rur_07/p_rur_07))/(1-a_rur_07)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_08 := weighted.mean(D_rur_08, w = w)]
          pers_h[, a_rur_08 := weighted.mean(D_rur_08, w = w)]
          pers_h[, w := ifelse(D_rur_08 == 1, w_old*(PF_rur_08/p_rur_08), w_old*((1-a_rur_08*(PF_rur_08/p_rur_08))/(1-a_rur_08)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_09 := weighted.mean(D_rur_09, w = w)]
          pers_h[, a_rur_09 := weighted.mean(D_rur_09, w = w)]
          pers_h[, w := ifelse(D_rur_09 == 1, w_old*(PF_rur_09/p_rur_09), w_old*((1-a_rur_09*(PF_rur_09/p_rur_09))/(1-a_rur_09)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_10 := weighted.mean(D_rur_10, w = w)]
          pers_h[, a_rur_10 := weighted.mean(D_rur_10, w = w)]
          pers_h[, w := ifelse(D_rur_10 == 1, w_old*(PF_rur_10/p_rur_10), w_old*((1-a_rur_10*(PF_rur_10/p_rur_10))/(1-a_rur_10)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_11 := weighted.mean(D_rur_11, w = w)]
          pers_h[, a_rur_11 := weighted.mean(D_rur_11, w = w)]
          pers_h[, w := ifelse(D_rur_11 == 1, w_old*(PF_rur_11/p_rur_11), w_old*((1-a_rur_11*(PF_rur_11/p_rur_11))/(1-a_rur_11)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_12 := weighted.mean(D_rur_12, w = w)]
          pers_h[, a_rur_12 := weighted.mean(D_rur_12, w = w)]
          pers_h[, w := ifelse(D_rur_12 == 1, w_old*(PF_rur_12/p_rur_12), w_old*((1-a_rur_12*(PF_rur_12/p_rur_12))/(1-a_rur_12)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_13 := weighted.mean(D_rur_13, w = w)]
          pers_h[, a_rur_13 := weighted.mean(D_rur_13, w = w)]
          pers_h[, w := ifelse(D_rur_13 == 1, w_old*(PF_rur_13/p_rur_13), w_old*((1-a_rur_13*(PF_rur_13/p_rur_13))/(1-a_rur_13)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_14 := weighted.mean(D_rur_14, w = w)]
          pers_h[, a_rur_14 := weighted.mean(D_rur_14, w = w)]
          pers_h[, w := ifelse(D_rur_14 == 1, w_old*(PF_rur_14/p_rur_14), w_old*((1-a_rur_14*(PF_rur_14/p_rur_14))/(1-a_rur_14)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_15 := weighted.mean(D_rur_15, w = w)]
          pers_h[, a_rur_15 := weighted.mean(D_rur_15, w = w)]
          pers_h[, w := ifelse(D_rur_15 == 1, w_old*(PF_rur_15/p_rur_15), w_old*((1-a_rur_15*(PF_rur_15/p_rur_15))/(1-a_rur_15)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_16 := weighted.mean(D_rur_16, w = w)]
          pers_h[, a_rur_16 := weighted.mean(D_rur_16, w = w)]
          pers_h[, w := ifelse(D_rur_16 == 1, w_old*(PF_rur_16/p_rur_16), w_old*((1-a_rur_16*(PF_rur_16/p_rur_16))/(1-a_rur_16)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_17 := weighted.mean(D_rur_17, w = w)]
          pers_h[, a_rur_17 := weighted.mean(D_rur_17, w = w)]
          pers_h[, w := ifelse(D_rur_17 == 1, w_old*(PF_rur_17/p_rur_17), w_old*((1-a_rur_17*(PF_rur_17/p_rur_17))/(1-a_rur_17)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_18 := weighted.mean(D_rur_18, w = w)]
          pers_h[, a_rur_18 := weighted.mean(D_rur_18, w = w)]
          pers_h[, w := ifelse(D_rur_18 == 1, w_old*(PF_rur_18/p_rur_18), w_old*((1-a_rur_18*(PF_rur_18/p_rur_18))/(1-a_rur_18)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_19 := weighted.mean(D_rur_19, w = w)]
          pers_h[, a_rur_19 := weighted.mean(D_rur_19, w = w)]
          pers_h[, w := ifelse(D_rur_19 == 1, w_old*(PF_rur_19/p_rur_19), w_old*((1-a_rur_19*(PF_rur_19/p_rur_19))/(1-a_rur_19)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_20 := weighted.mean(D_rur_20, w = w)]
          pers_h[, a_rur_20 := weighted.mean(D_rur_20, w = w)]
          pers_h[, w := ifelse(D_rur_20 == 1, w_old*(PF_rur_20/p_rur_20), w_old*((1-a_rur_20*(PF_rur_20/p_rur_20))/(1-a_rur_20)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_21 := weighted.mean(D_rur_21, w = w)]
          pers_h[, a_rur_21 := weighted.mean(D_rur_21, w = w)]
          pers_h[, w := ifelse(D_rur_21 == 1, w_old*(PF_rur_21/p_rur_21), w_old*((1-a_rur_21*(PF_rur_21/p_rur_21))/(1-a_rur_21)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_22 := weighted.mean(D_rur_22, w = w)]
          pers_h[, a_rur_22 := weighted.mean(D_rur_22, w = w)]
          pers_h[, w := ifelse(D_rur_22 == 1, w_old*(PF_rur_22/p_rur_22), w_old*((1-a_rur_22*(PF_rur_22/p_rur_22))/(1-a_rur_22)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_23 := weighted.mean(D_rur_23, w = w)]
          pers_h[, a_rur_23 := weighted.mean(D_rur_23, w = w)]
          pers_h[, w := ifelse(D_rur_23 == 1, w_old*(PF_rur_23/p_rur_23), w_old*((1-a_rur_23*(PF_rur_23/p_rur_23))/(1-a_rur_23)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_24 := weighted.mean(D_rur_24, w = w)]
          pers_h[, a_rur_24 := weighted.mean(D_rur_24, w = w)]
          pers_h[, w := ifelse(D_rur_24 == 1, w_old*(PF_rur_24/p_rur_24), w_old*((1-a_rur_24*(PF_rur_24/p_rur_24))/(1-a_rur_24)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_25 := weighted.mean(D_rur_25, w = w)]
          pers_h[, a_rur_25 := weighted.mean(D_rur_25, w = w)]
          pers_h[, w := ifelse(D_rur_25 == 1, w_old*(PF_rur_25/p_rur_25), w_old*((1-a_rur_25*(PF_rur_25/p_rur_25))/(1-a_rur_25)))]
          pers_h[, w_old := w]

          pers_h[, p_rur_26 := weighted.mean(D_rur_26, w = w)]
          pers_h[, a_rur_26 := weighted.mean(D_rur_26, w = w)]
          pers_h[, w := ifelse(D_rur_26 == 1, w_old*(PF_rur_26/p_rur_26), w_old*((1-a_rur_26*(PF_rur_26/p_rur_26))/(1-a_rur_26)))]
          pers_h[, w_old := w]



          pers_h[, p_urb_01 := weighted.mean(D_urb_01, w = w)]
          pers_h[, a_urb_01 := weighted.mean(D_urb_01, w = w)]
          pers_h[, w := ifelse(D_urb_01 == 1, w_old*(PF_urb_01/p_urb_01), w_old*((1-a_urb_01*(PF_urb_01/p_urb_01))/(1-a_urb_01)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_02 := weighted.mean(D_urb_02, w = w)]
          pers_h[, a_urb_02 := weighted.mean(D_urb_02, w = w)]
          pers_h[, w := ifelse(D_urb_02 == 1, w_old*(PF_urb_02/p_urb_02), w_old*((1-a_urb_02*(PF_urb_02/p_urb_02))/(1-a_urb_02)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_03 := weighted.mean(D_urb_03, w = w)]
          pers_h[, a_urb_03 := weighted.mean(D_urb_03, w = w)]
          pers_h[, w := ifelse(D_urb_03 == 1, w_old*(PF_urb_03/p_urb_03), w_old*((1-a_urb_03*(PF_urb_03/p_urb_03))/(1-a_urb_03)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_04 := weighted.mean(D_urb_04, w = w)]
          pers_h[, a_urb_04 := weighted.mean(D_urb_04, w = w)]
          pers_h[, w := ifelse(D_urb_04 == 1, w_old*(PF_urb_04/p_urb_04), w_old*((1-a_urb_04*(PF_urb_04/p_urb_04))/(1-a_urb_04)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_05 := weighted.mean(D_urb_05, w = w)]
          pers_h[, a_urb_05 := weighted.mean(D_urb_05, w = w)]
          pers_h[, w := ifelse(D_urb_05 == 1, w_old*(PF_urb_05/p_urb_05), w_old*((1-a_urb_05*(PF_urb_05/p_urb_05))/(1-a_urb_05)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_06 := weighted.mean(D_urb_06, w = w)]
          pers_h[, a_urb_06 := weighted.mean(D_urb_06, w = w)]
          pers_h[, w := ifelse(D_urb_06 == 1, w_old*(PF_urb_06/p_urb_06), w_old*((1-a_urb_06*(PF_urb_06/p_urb_06))/(1-a_urb_06)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_07 := weighted.mean(D_urb_07, w = w)]
          pers_h[, a_urb_07 := weighted.mean(D_urb_07, w = w)]
          pers_h[, w := ifelse(D_urb_07 == 1, w_old*(PF_urb_07/p_urb_07), w_old*((1-a_urb_07*(PF_urb_07/p_urb_07))/(1-a_urb_07)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_08 := weighted.mean(D_urb_08, w = w)]
          pers_h[, a_urb_08 := weighted.mean(D_urb_08, w = w)]
          pers_h[, w := ifelse(D_urb_08 == 1, w_old*(PF_urb_08/p_urb_08), w_old*((1-a_urb_08*(PF_urb_08/p_urb_08))/(1-a_urb_08)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_09 := weighted.mean(D_urb_09, w = w)]
          pers_h[, a_urb_09 := weighted.mean(D_urb_09, w = w)]
          pers_h[, w := ifelse(D_urb_09 == 1, w_old*(PF_urb_09/p_urb_09), w_old*((1-a_urb_09*(PF_urb_09/p_urb_09))/(1-a_urb_09)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_10 := weighted.mean(D_urb_10, w = w)]
          pers_h[, a_urb_10 := weighted.mean(D_urb_10, w = w)]
          pers_h[, w := ifelse(D_urb_10 == 1, w_old*(PF_urb_10/p_urb_10), w_old*((1-a_urb_10*(PF_urb_10/p_urb_10))/(1-a_urb_10)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_11 := weighted.mean(D_urb_11, w = w)]
          pers_h[, a_urb_11 := weighted.mean(D_urb_11, w = w)]
          pers_h[, w := ifelse(D_urb_11 == 1, w_old*(PF_urb_11/p_urb_11), w_old*((1-a_urb_11*(PF_urb_11/p_urb_11))/(1-a_urb_11)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_12 := weighted.mean(D_urb_12, w = w)]
          pers_h[, a_urb_12 := weighted.mean(D_urb_12, w = w)]
          pers_h[, w := ifelse(D_urb_12 == 1, w_old*(PF_urb_12/p_urb_12), w_old*((1-a_urb_12*(PF_urb_12/p_urb_12))/(1-a_urb_12)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_13 := weighted.mean(D_urb_13, w = w)]
          pers_h[, a_urb_13 := weighted.mean(D_urb_13, w = w)]
          pers_h[, w := ifelse(D_urb_13 == 1, w_old*(PF_urb_13/p_urb_13), w_old*((1-a_urb_13*(PF_urb_13/p_urb_13))/(1-a_urb_13)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_14 := weighted.mean(D_urb_14, w = w)]
          pers_h[, a_urb_14 := weighted.mean(D_urb_14, w = w)]
          pers_h[, w := ifelse(D_urb_14 == 1, w_old*(PF_urb_14/p_urb_14), w_old*((1-a_urb_14*(PF_urb_14/p_urb_14))/(1-a_urb_14)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_15 := weighted.mean(D_urb_15, w = w)]
          pers_h[, a_urb_15 := weighted.mean(D_urb_15, w = w)]
          pers_h[, w := ifelse(D_urb_15 == 1, w_old*(PF_urb_15/p_urb_15), w_old*((1-a_urb_15*(PF_urb_15/p_urb_15))/(1-a_urb_15)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_16 := weighted.mean(D_urb_16, w = w)]
          pers_h[, a_urb_16 := weighted.mean(D_urb_16, w = w)]
          pers_h[, w := ifelse(D_urb_16 == 1, w_old*(PF_urb_16/p_urb_16), w_old*((1-a_urb_16*(PF_urb_16/p_urb_16))/(1-a_urb_16)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_17 := weighted.mean(D_urb_17, w = w)]
          pers_h[, a_urb_17 := weighted.mean(D_urb_17, w = w)]
          pers_h[, w := ifelse(D_urb_17 == 1, w_old*(PF_urb_17/p_urb_17), w_old*((1-a_urb_17*(PF_urb_17/p_urb_17))/(1-a_urb_17)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_18 := weighted.mean(D_urb_18, w = w)]
          pers_h[, a_urb_18 := weighted.mean(D_urb_18, w = w)]
          pers_h[, w := ifelse(D_urb_18 == 1, w_old*(PF_urb_18/p_urb_18), w_old*((1-a_urb_18*(PF_urb_18/p_urb_18))/(1-a_urb_18)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_19 := weighted.mean(D_urb_19, w = w)]
          pers_h[, a_urb_19 := weighted.mean(D_urb_19, w = w)]
          pers_h[, w := ifelse(D_urb_19 == 1, w_old*(PF_urb_19/p_urb_19), w_old*((1-a_urb_19*(PF_urb_19/p_urb_19))/(1-a_urb_19)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_20 := weighted.mean(D_urb_20, w = w)]
          pers_h[, a_urb_20 := weighted.mean(D_urb_20, w = w)]
          pers_h[, w := ifelse(D_urb_20 == 1, w_old*(PF_urb_20/p_urb_20), w_old*((1-a_urb_20*(PF_urb_20/p_urb_20))/(1-a_urb_20)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_21 := weighted.mean(D_urb_21, w = w)]
          pers_h[, a_urb_21 := weighted.mean(D_urb_21, w = w)]
          pers_h[, w := ifelse(D_urb_21 == 1, w_old*(PF_urb_21/p_urb_21), w_old*((1-a_urb_21*(PF_urb_21/p_urb_21))/(1-a_urb_21)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_22 := weighted.mean(D_urb_22, w = w)]
          pers_h[, a_urb_22 := weighted.mean(D_urb_22, w = w)]
          pers_h[, w := ifelse(D_urb_22 == 1, w_old*(PF_urb_22/p_urb_22), w_old*((1-a_urb_22*(PF_urb_22/p_urb_22))/(1-a_urb_22)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_23 := weighted.mean(D_urb_23, w = w)]
          pers_h[, a_urb_23 := weighted.mean(D_urb_23, w = w)]
          pers_h[, w := ifelse(D_urb_23 == 1, w_old*(PF_urb_23/p_urb_23), w_old*((1-a_urb_23*(PF_urb_23/p_urb_23))/(1-a_urb_23)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_24 := weighted.mean(D_urb_24, w = w)]
          pers_h[, a_urb_24 := weighted.mean(D_urb_24, w = w)]
          pers_h[, w := ifelse(D_urb_24 == 1, w_old*(PF_urb_24/p_urb_24), w_old*((1-a_urb_24*(PF_urb_24/p_urb_24))/(1-a_urb_24)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_25 := weighted.mean(D_urb_25, w = w)]
          pers_h[, a_urb_25 := weighted.mean(D_urb_25, w = w)]
          pers_h[, w := ifelse(D_urb_25 == 1, w_old*(PF_urb_25/p_urb_25), w_old*((1-a_urb_25*(PF_urb_25/p_urb_25))/(1-a_urb_25)))]
          pers_h[, w_old := w]

          pers_h[, p_urb_26 := weighted.mean(D_urb_26, w = w)]
          pers_h[, a_urb_26 := weighted.mean(D_urb_26, w = w)]
          pers_h[, w := ifelse(D_urb_26 == 1, w_old*(PF_urb_26/p_urb_26), w_old*((1-a_urb_26*(PF_urb_26/p_urb_26))/(1-a_urb_26)))]
          pers_h[, w_old := w]

          pers_h <- pers_h[, -(p_rur_02:a_urb_26)]
        }
      }

      # Cal 1 : Trimming 1

      # Burada "!=" kullandik cunku en son trimde degil NUTS2-KIRKENT kalibrasyonunda kalmasini istiyoruz.

      if(trimyap == TRUE & i != trimming_1){
        w_mean <- mean(pers_h$w, na.rm = T)
        pers_h[w > 3 * w_mean, w := 3 * w_mean]
        pers_h[, w_old := w]
        pers_h[w < 0.3 * w_mean, w := 0.3 * w_mean]
        pers_h[, w_old := w]
      }
    }
  }

  # ONEMLIIIII
  # BURADA NORMALDE KIR-KENT ICIN AYRI AYRI DEGIL TOPLAM BAZDA TEK BIR DUZELTME KATSAYISIYLA CARPTIKTAN SONRA
  # IKINCI KALIBRASYONA GECIYORDUK. FAKAT ARTIK IKINCI KALIBRASYONA IHTIYAC OLMADIGI ICIN BURADA YAPILAN
  # ISLEM NIHAI DUZELTME OLACAK. BU DUZELTMENIN DE TOPLAM BAZDA DEGIL KIR-KENT BAZINDA YAPILMASININ DAHA
  # UYGUN OLACAGI DUSUNULDUGU ICIN BU SEKILDE YAPILDI.
  # ONEMLIIIII

  # Birinci kalibrasyon islemleri bittikten sonra PROJEKTE nufusa tutturmak icin "duzfakto_carpan_kir" ve
  # "duzfakto_carpan_kent" katsayilarini hesapliyoruz. KIR ve KENT PROJEKTE nufuslarini tutan katsayilarla
  # sonuclari acikliyoruz..

  a <- pers_h %>% group_by(KIR_KENT) %>% summarize(toplam_w = sum(w))

  kir_toplam <- a$toplam_w[1]
  kent_toplam <- a$toplam_w[2]

  duzfakto_carpan_kir <- kirnufus / kir_toplam
  duzfakto_carpan_kent <- kentnufus / kent_toplam

  # duzfakto.carpan <- (kirnufus + kentnufus) / sum(pers_h$w)
  pers_h <- as.data.table(pers_h)
  pers_h[KIR_KENT == 1, w := duzfakto_carpan_kir * w]
  pers_h[KIR_KENT == 2, w := duzfakto_carpan_kent * w]

  pers_h[, w_old := w]
  ozet_veri <- pers_h %>% select("ILKAYITNO", "ALTORNEK", "BLOKNO", "bulten", "IBBS_1", "IBBS_2", "nur", "KIR_KENT", "DURUM", "IKFA4", "hhb", "fk_cinsi", "gender", "fk_yas", "age_g", "AG", "duzfakto", "w")

  if(hhbkirkent == TRUE) {
    ozet_veri[KIR_KENT == 1, PF_hhbkirkent := paste0("PF_hhb_kk_", KIR_KENT, "_", hhb)]
    ozet_veri[KIR_KENT == 2, PF_hhbkirkent := paste0("PF_hhb_kk_", KIR_KENT, "_", hhb)]
  }

  if(hhbkirkent == FALSE) {
    ozet_veri[ , PF_hhbkirkent := paste0("PF_hhb_", hhb)]
  }

  ozet_veri[ , PF_ilkayitno := paste0("PF_il_", ILKAYITNO)]

  ########################## SONUCLAR #########################################
  ########################## SONUCLAR #########################################

  # gk_caltrim fonksiyonu sonrasinda; nihai agirliklarin oldugu veri seti, kontrol tablolari, min-max
  # degerleri, ikfa4, durum dagilimlari gibi farkli sonuclarin yer aldıgı "SONUCLAR" adinda bir liste
  # olusturuyoruz. Burada bahsedilen her ciktiyi SONUCLAR listesinin icerisine kaydedecegiz ve fonksiyon
  # ciktisi olarak bu ciktiyi verecegiz. Dolayisiyla kullanici "liste" formatinda bir sonuc elde ederek
  # istedigi veri setine veya tabloya "SONUCLAR$veri, SONUCLAR$kontrol_yc$min, SONUCLAR$kontrol_yc$max
  # seklinde ulasabilecektir. O yuzden baslangicta bos bir "SONUCLAR" listesi olusturuyoruz.
  SONUCLAR <- list()

  ##### kontrol_yc #####
  kontrol_yc <- list()
  xxx_yc <- ozet_veri %>% group_by(AG) %>% summarize(toplam_w = sum(w), toplam_n = n())
  xxx_yc$hia_oran <- prop.table(xxx_yc$toplam_w)

  proj_yc <- proj_yascins
  yyy_yc <- proj_yc %>% pivot_longer(cols = names(proj_yc), names_to = "proj", values_to = "proj_oran")

  zzz_yc <- xxx_yc %>% left_join(yyy_yc, by = c("AG" = "proj"))
  zzz_yc$hia_proj_oran <- zzz_yc$hia_oran / zzz_yc$proj_oran

  kontrol_yc$tablo <- zzz_yc
  kontrol_yc$min <- min(zzz_yc$hia_proj_oran)
  kontrol_yc$max <- max(zzz_yc$hia_proj_oran)

  SONUCLAR$kontrol_yc <- kontrol_yc

  ##### kontrol_il #####
  kontrol_il <- list()
  xxx_il <- ozet_veri %>% group_by(ILKAYITNO) %>% summarize(toplam_w = sum(w), toplam_n = n())
  xxx_il$hia_oran <- prop.table(xxx_il$toplam_w)

  proj_ill <- proj_il
  yyy_il <- proj_ill %>% pivot_longer(cols = names(proj_ill), names_to = "proj", values_to = "proj_oran")

  xxx_il <- as.data.table(xxx_il)
  xxx_il[ , IL_proj := paste0("PF_il_", ILKAYITNO)]

  zzz_il <- xxx_il %>% left_join(yyy_il, by = c("IL_proj" = "proj"))
  zzz_il$hia_proj_oran <- zzz_il$hia_oran / zzz_il$proj_oran

  kontrol_il$tablo <- zzz_il
  kontrol_il$min <- min(zzz_il$hia_proj_oran)
  kontrol_il$max <- max(zzz_il$hia_proj_oran)

  SONUCLAR$kontrol_il <- kontrol_il

  return(SONUCLAR)
}
