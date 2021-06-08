#' Veri Setinin Duzenlenmesi
#'
#' @param data Duzenlenmemis HIA veri seti. \cr
#' Bu veri seti kullanilarak yasgrubu, cinsiyet, hhb, IBBS_2 gibi
#' degiskenlerde duzenlemeler yapilarak dummy degiskenler olusturulmaktadir. \cr
#' Ayrica "gk_caltrim" fonksiyonunda, kalibrasyon icin kullanilmak uzere projekte populasyonlari da veri setine eklenmektedir. \cr
#' NOT: Veri setinde yer alacak degiskenlerin standart bir formatta olmasi gerekmektedir. \cr
#' Ornegin; \cr
#' ILKAYITNO: veri tipi character ; 01, 02, 03 seklinde \cr
#' IBBS_1: veri tipi character ; TR1, TR2 seklinde \cr
#' IBBS_2: veri tipi character ; TR10, TR21 seklinde \cr
#' KIR_KENT: veri tipi numeric ; 1, 2 seklinde \cr
#' CINSIYET: veri tipi numeric ; 1, 2 seklinde \cr
#' FAKTOR: veri tipi numeric \cr
#' HHB: veri tipi numeric ; 1, 2 ,3 seklinde \cr
#' duzfakto: veri tipi numeric \cr
#' fk_fkiml: veri tipi numeric ; 1, 2, 3 seklinde
#' @param cinsiyet Cinsiyet degiskeni icin kullanilacak sutun adi (veri tipi numeric: 1, 2 seklinde)
#' @param birimno Birimno sutun adi (veri tipi numeric: 325069, 325070 seklinde)
#' @param yas Yas sutun adi (veri tipi numeric: 26, 35 seklinde)
#' @param hhbsay Hanehalki buyuklugu degiskeninin kac grup olacagini belirtmektedir. Ornegin "hhbsay = 4"
#' degeri kullanildiginda kalibrasyon islemleri 1, 2, 3 ve 4+ hanehalki buyuklugu gruplarinda yapilacaktir.
#' @param hhbkirkent HHB * KIRKENT crossunda kalibrasyon yapilmasi isteniyorsa TRUE,
#' sadece HHB bazinda kalibrasyon yapilmasi isteniyorsa FALSE degerini almaktadir.
#' @param proj_yascins Yas - Cinsiyet projekte oranlari (tek satir halinde)
#' @param proj_il IL projekte oranlari (tek satir halinde)
#' @param proj_nutskirkent NUTS2 - Kirkent projekte oranlari (tek satir halinde)
#' @param proj_hhbkirkent HHB - Kirkent ADNKS oranlari (Kirkent ayrimi olacaksa HHB*2,
#' kirkent ayrimi olmayacaksa HHB grup sayisi kadar satir olacak) \cr
#' \link{https://www.google.com}

#'
#' @export
#' @return "gk_caltrim" fonksiyonuna girdi olacak duzenlenmis PERS dosyasini cikti olarak verir.
#'
#' @examples
#' @import data.table
#' @import rlang
#' @import magrittr
#' @import dplyr
#' @import stringr
#' @import tidyr

gk_arrange <- function(data,
                       proj_yascins,
                       proj_il,
                       proj_nutskirkent,
                       proj_hhbkirkent,
                       cinsiyet = "CINSIYET",
                       birimno = "BIRIMNO",
                       yas = "YAS",
                       hhbsay = 9,
                       hhbkirkent = TRUE) {

  ############################################################################
  # YAS-CINSIYET PROJEKTELERININ DUZENLENMESI
  ############################################################################
  pop_prop_ag_satir <- gather(proj_yascins, key = "AG", value = "fr")
  ############################################################################


  ############################################################################
  # IL PROJEKTELERININ DUZENLENMESI
  ############################################################################
  pop_prop_il_satir <- gather(proj_il, key = "IL", value = "fr")
  # pop_il_satir verisinde 1-81 arasi degerlerin yer aldigi character veri tipinde ILKAYITNO sutununu olusturduk.
  pop_prop_il_satir$ILKAYITNO <- as.character(1:81)
  pop_prop_il_satir <- as.data.table(pop_prop_il_satir)

  # ILKAYITNO sutununda yer alan degerlerin "01, 02, ..." seklinde olmasini istedigimiz icin bu kodu kullandik.
  pop_prop_il_satir[nchar(ILKAYITNO) < 2, ILKAYITNO := paste0("0", ILKAYITNO)]
  pop_prop_il_satir
  ############################################################################


  ############################################################################
  # NUTS2-KIRKENT PROJEKTELERININ DUZENLENMESI
  ############################################################################
  pop_prop_nuts_ur_satir <- gather(proj_nutskirkent, key = "NUTS2_KK", value = "fr")
  pop_prop_nuts_ur_satir$fr <- as.numeric(pop_prop_nuts_ur_satir$fr)

  # Istanbul'un kiri olmadigi icin NUTS2_KK sutununda "PF_rur_01" olan satiri siliyoruz.
  # "which" fonksiyonu ilgili kritere uyan satirin ID numarasini verir.
  pop_prop_nuts_ur_satir <- as.data.table(pop_prop_nuts_ur_satir[-which(pop_prop_nuts_ur_satir$NUTS2_KK == "PF_rur_01"), ])
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_02", nur := "TR21_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_03", nur := "TR22_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_04", nur := "TR31_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_05", nur := "TR32_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_06", nur := "TR33_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_07", nur := "TR41_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_08", nur := "TR42_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_09", nur := "TR51_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_10", nur := "TR52_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_11", nur := "TR61_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_12", nur := "TR62_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_13", nur := "TR63_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_14", nur := "TR71_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_15", nur := "TR72_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_16", nur := "TR81_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_17", nur := "TR82_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_18", nur := "TR83_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_19", nur := "TR90_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_20", nur := "TRA1_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_21", nur := "TRA2_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_22", nur := "TRB1_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_23", nur := "TRB2_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_24", nur := "TRC1_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_25", nur := "TRC2_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_rur_26", nur := "TRC3_rur"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_01", nur := "TR10_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_02", nur := "TR21_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_03", nur := "TR22_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_04", nur := "TR31_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_05", nur := "TR32_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_06", nur := "TR33_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_07", nur := "TR41_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_08", nur := "TR42_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_09", nur := "TR51_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_10", nur := "TR52_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_11", nur := "TR61_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_12", nur := "TR62_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_13", nur := "TR63_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_14", nur := "TR71_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_15", nur := "TR72_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_16", nur := "TR81_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_17", nur := "TR82_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_18", nur := "TR83_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_19", nur := "TR90_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_20", nur := "TRA1_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_21", nur := "TRA2_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_22", nur := "TRB1_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_23", nur := "TRB2_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_24", nur := "TRC1_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_25", nur := "TRC2_urb"]
  pop_prop_nuts_ur_satir[NUTS2_KK == "PF_urb_26", nur := "TRC3_urb"]
  ############################################################################


  ############################################################################
  # HHB-KIRKENT PROJEKTELERININ DUZENLENMESI
  ############################################################################
  if(hhbkirkent == TRUE) {
    pop_hh_kirkent_adnks <- proj_hhbkirkent %>%
      mutate(hhsize = ifelse(hhsize >= hhbsay, hhbsay, hhsize)) %>%
      group_by(kirkent, hhsize) %>% summarize(ADNKS = sum(ADNKS, na.rm = TRUE))

    pop_hh_kirkent_adnks <- as.data.table(pop_hh_kirkent_adnks)

    pop_hh_kirkent_adnks[, hhb_kirkent := paste0(kirkent, "-", hhsize)]
    pop_hh_kirkent_adnks[, ADNKS_isim := paste0("PF_hhb_kk_",kirkent, "_", hhsize)]
  }

  if(hhbkirkent == FALSE) {
    pop_hh_kirkent_adnks <- proj_hhbkirkent %>%
      mutate(hhsize = ifelse(hhsize >= hhbsay, hhbsay, hhsize)) %>%
      group_by(hhsize) %>% summarize(ADNKS = sum(ADNKS, na.rm = TRUE))

    pop_hh_kirkent_adnks <- as.data.table(pop_hh_kirkent_adnks)

    pop_hh_kirkent_adnks[, hhb_kirkent := hhsize]
    pop_hh_kirkent_adnks[, ADNKS_isim := paste0("PF_hhb_", hhsize)]
  }

  pop_prop_hh_kirkent_adnks_satir <- pop_hh_kirkent_adnks
  pop_prop_hh_kirkent_adnks_satir$ayni <- 999
  pop_prop_hh_kirkent_adnks_satir <- pop_prop_hh_kirkent_adnks_satir %>% select(ADNKS_isim, ADNKS, ayni)
  pop_prop_hh_kirkent_adnks_satir <- pop_prop_hh_kirkent_adnks_satir %>% pivot_wider(names_from = ADNKS_isim, values_from = ADNKS)
  pop_prop_hh_kirkent_adnks_satir <- pop_prop_hh_kirkent_adnks_satir %>% select(-ayni)
  ############################################################################



  ############################################################################
  # PERS DATASININ OLUSTURULMASI
  ############################################################################

  data <- as.data.table(data)
  pers <- data[, fk_fkiml := 1:.N]
  setnames(pers, old = c(cinsiyet, birimno, yas), new = c("fk_cinsi","bulten","fk_yas"))
  pers[fk_cinsi == 2, gender := "female"]
  pers[fk_cinsi == 1, gender := "male"]
  pers[fk_yas %between% c(0,4), age_g := "0-4"]
  pers[fk_yas %between% c(5,11), age_g := "5-11"]
  pers[fk_yas %between% c(12,14), age_g := "12-14"]
  pers[fk_yas %between% c(15,17), age_g := "15-17"]
  pers[fk_yas %between% c(18,20), age_g := "18-20"]
  pers[fk_yas %between% c(21,24), age_g := "21-24"]
  pers[fk_yas %between% c(25,29), age_g := "25-29"]
  pers[fk_yas %between% c(30,34), age_g := "30-34"]
  pers[fk_yas %between% c(35,39), age_g := "35-39"]
  pers[fk_yas %between% c(40,44), age_g := "40-44"]
  pers[fk_yas %between% c(45,49), age_g := "45-49"]
  pers[fk_yas %between% c(50,54), age_g := "50-54"]
  pers[fk_yas %between% c(55,59), age_g := "55-59"]
  pers[fk_yas %between% c(60,64), age_g := "60-64"]
  pers[fk_yas %between% c(65,74), age_g := "65-74"]
  pers[fk_yas > 74, age_g := "75+"]
  pers[, ILKAYITNO := str_trim(ILKAYITNO)]

  pers[str_trim(IBBS_2) == "TR21" & KIR_KENT == 1, nur := "TR21_rur"]
  pers[str_trim(IBBS_2) == "TR22" & KIR_KENT == 1, nur := "TR22_rur"]
  pers[str_trim(IBBS_2) == "TR31" & KIR_KENT == 1, nur := "TR31_rur"]
  pers[str_trim(IBBS_2) == "TR32" & KIR_KENT == 1, nur := "TR32_rur"]
  pers[str_trim(IBBS_2) == "TR33" & KIR_KENT == 1, nur := "TR33_rur"]
  pers[str_trim(IBBS_2) == "TR41" & KIR_KENT == 1, nur := "TR41_rur"]
  pers[str_trim(IBBS_2) == "TR42" & KIR_KENT == 1, nur := "TR42_rur"]
  pers[str_trim(IBBS_2) == "TR51" & KIR_KENT == 1, nur := "TR51_rur"]
  pers[str_trim(IBBS_2) == "TR52" & KIR_KENT == 1, nur := "TR52_rur"]
  pers[str_trim(IBBS_2) == "TR61" & KIR_KENT == 1, nur := "TR61_rur"]
  pers[str_trim(IBBS_2) == "TR62" & KIR_KENT == 1, nur := "TR62_rur"]
  pers[str_trim(IBBS_2) == "TR63" & KIR_KENT == 1, nur := "TR63_rur"]
  pers[str_trim(IBBS_2) == "TR71" & KIR_KENT == 1, nur := "TR71_rur"]
  pers[str_trim(IBBS_2) == "TR72" & KIR_KENT == 1, nur := "TR72_rur"]
  pers[str_trim(IBBS_2) == "TR81" & KIR_KENT == 1, nur := "TR81_rur"]
  pers[str_trim(IBBS_2) == "TR82" & KIR_KENT == 1, nur := "TR82_rur"]
  pers[str_trim(IBBS_2) == "TR83" & KIR_KENT == 1, nur := "TR83_rur"]
  pers[str_trim(IBBS_2) == "TR90" & KIR_KENT == 1, nur := "TR90_rur"]
  pers[str_trim(IBBS_2) == "TRA1" & KIR_KENT == 1, nur := "TRA1_rur"]
  pers[str_trim(IBBS_2) == "TRA2" & KIR_KENT == 1, nur := "TRA2_rur"]
  pers[str_trim(IBBS_2) == "TRB1" & KIR_KENT == 1, nur := "TRB1_rur"]
  pers[str_trim(IBBS_2) == "TRB2" & KIR_KENT == 1, nur := "TRB2_rur"]
  pers[str_trim(IBBS_2) == "TRC1" & KIR_KENT == 1, nur := "TRC1_rur"]
  pers[str_trim(IBBS_2) == "TRC2" & KIR_KENT == 1, nur := "TRC2_rur"]
  pers[str_trim(IBBS_2) == "TRC3" & KIR_KENT == 1, nur := "TRC3_rur"]
  pers[str_trim(IBBS_2) == "TR10" & KIR_KENT == 2, nur := "TR10_urb"]
  pers[str_trim(IBBS_2) == "TR21" & KIR_KENT == 2, nur := "TR21_urb"]
  pers[str_trim(IBBS_2) == "TR22" & KIR_KENT == 2, nur := "TR22_urb"]
  pers[str_trim(IBBS_2) == "TR31" & KIR_KENT == 2, nur := "TR31_urb"]
  pers[str_trim(IBBS_2) == "TR32" & KIR_KENT == 2, nur := "TR32_urb"]
  pers[str_trim(IBBS_2) == "TR33" & KIR_KENT == 2, nur := "TR33_urb"]
  pers[str_trim(IBBS_2) == "TR41" & KIR_KENT == 2, nur := "TR41_urb"]
  pers[str_trim(IBBS_2) == "TR42" & KIR_KENT == 2, nur := "TR42_urb"]
  pers[str_trim(IBBS_2) == "TR51" & KIR_KENT == 2, nur := "TR51_urb"]
  pers[str_trim(IBBS_2) == "TR52" & KIR_KENT == 2, nur := "TR52_urb"]
  pers[str_trim(IBBS_2) == "TR61" & KIR_KENT == 2, nur := "TR61_urb"]
  pers[str_trim(IBBS_2) == "TR62" & KIR_KENT == 2, nur := "TR62_urb"]
  pers[str_trim(IBBS_2) == "TR63" & KIR_KENT == 2, nur := "TR63_urb"]
  pers[str_trim(IBBS_2) == "TR71" & KIR_KENT == 2, nur := "TR71_urb"]
  pers[str_trim(IBBS_2) == "TR72" & KIR_KENT == 2, nur := "TR72_urb"]
  pers[str_trim(IBBS_2) == "TR81" & KIR_KENT == 2, nur := "TR81_urb"]
  pers[str_trim(IBBS_2) == "TR82" & KIR_KENT == 2, nur := "TR82_urb"]
  pers[str_trim(IBBS_2) == "TR83" & KIR_KENT == 2, nur := "TR83_urb"]
  pers[str_trim(IBBS_2) == "TR90" & KIR_KENT == 2, nur := "TR90_urb"]
  pers[str_trim(IBBS_2) == "TRA1" & KIR_KENT == 2, nur := "TRA1_urb"]
  pers[str_trim(IBBS_2) == "TRA2" & KIR_KENT == 2, nur := "TRA2_urb"]
  pers[str_trim(IBBS_2) == "TRB1" & KIR_KENT == 2, nur := "TRB1_urb"]
  pers[str_trim(IBBS_2) == "TRB2" & KIR_KENT == 2, nur := "TRB2_urb"]
  pers[str_trim(IBBS_2) == "TRC1" & KIR_KENT == 2, nur := "TRC1_urb"]
  pers[str_trim(IBBS_2) == "TRC2" & KIR_KENT == 2, nur := "TRC2_urb"]
  pers[str_trim(IBBS_2) == "TRC3" & KIR_KENT == 2, nur := "TRC3_urb"]

  pers[, D_m_0_4 := ifelse(gender == "male" & age_g == "0-4" ,1,0)]
  pers[, D_m_5_11:= ifelse(gender =='male' & age_g == '5-11' ,1,0)]
  pers[, D_m_12_14:= ifelse(gender =='male' & age_g == '12-14' ,1,0)]
  pers[, D_m_15_17:= ifelse(gender =='male' & age_g == '15-17' ,1,0)]
  pers[, D_m_18_20:= ifelse(gender =='male' & age_g == '18-20' ,1,0)]
  pers[, D_m_21_24:= ifelse(gender =='male' & age_g == '21-24' ,1,0)]
  pers[, D_m_25_29:= ifelse(gender =='male' & age_g == '25-29' ,1,0)]
  pers[, D_m_30_34:= ifelse(gender =='male' & age_g == '30-34' ,1,0)]
  pers[, D_m_35_39:= ifelse(gender =='male' & age_g == '35-39' ,1,0)]
  pers[, D_m_40_44:= ifelse(gender =='male' & age_g == '40-44' ,1,0)]
  pers[, D_m_45_49:= ifelse(gender =='male' & age_g == '45-49' ,1,0)]
  pers[, D_m_50_54:= ifelse(gender =='male' & age_g == '50-54' ,1,0)]
  pers[, D_m_55_59:= ifelse(gender =='male' & age_g == '55-59' ,1,0)]
  pers[, D_m_60_64:= ifelse(gender =='male' & age_g == '60-64' ,1,0)]
  pers[, D_m_65_74:= ifelse(gender =='male' & age_g == '65-74' ,1,0)]
  pers[, D_m_over75:= ifelse(gender =='male' & age_g == '75+' ,1,0)]
  pers[, D_f_0_4:= ifelse(gender =='female' & age_g == '0-4' ,1,0)]
  pers[, D_f_5_11:= ifelse(gender =='female' & age_g == '5-11' ,1,0)]
  pers[, D_f_12_14:= ifelse(gender =='female' & age_g == '12-14' ,1,0)]
  pers[, D_f_15_17:= ifelse(gender =='female' & age_g == '15-17' ,1,0)]
  pers[, D_f_18_20:= ifelse(gender =='female' & age_g == '18-20' ,1,0)]
  pers[, D_f_21_24:= ifelse(gender =='female' & age_g == '21-24' ,1,0)]
  pers[, D_f_25_29:= ifelse(gender =='female' & age_g == '25-29' ,1,0)]
  pers[, D_f_30_34:= ifelse(gender =='female' & age_g == '30-34' ,1,0)]
  pers[, D_f_35_39:= ifelse(gender =='female' & age_g == '35-39' ,1,0)]
  pers[, D_f_40_44:= ifelse(gender =='female' & age_g == '40-44' ,1,0)]
  pers[, D_f_45_49:= ifelse(gender =='female' & age_g == '45-49' ,1,0)]
  pers[, D_f_50_54:= ifelse(gender =='female' & age_g == '50-54' ,1,0)]
  pers[, D_f_55_59:= ifelse(gender =='female' & age_g == '55-59' ,1,0)]
  pers[, D_f_60_64:= ifelse(gender =='female' & age_g == '60-64' ,1,0)]
  pers[, D_f_65_74:= ifelse(gender =='female' & age_g == '65-74' ,1,0)]
  pers[, D_f_over75:= ifelse(gender =='female' & age_g == '75+' ,1,0)]
  pers[, GENDAGE := (D_m_0_4+D_m_5_11+D_m_12_14+D_m_15_17+D_m_18_20+D_m_21_24+D_m_25_29+D_m_30_34+D_m_35_39+
                       D_m_40_44+D_m_45_49+D_m_50_54+D_m_55_59+D_m_60_64+D_m_65_74+D_m_over75+D_f_0_4+D_f_5_11+D_f_12_14+D_f_15_17+D_f_18_20+D_f_21_24+D_f_25_29+D_f_30_34+D_f_35_39+
                       D_f_40_44+D_f_45_49+D_f_50_54+D_f_55_59+D_f_60_64+ D_f_65_74+D_f_over75)]

  pers[gender == 'male' & age_g == "0-4", AG := "PF_m_0_4"]
  pers[gender == 'male' & age_g == '5-11', AG := "PF_m_5_11"]
  pers[gender == 'male' & age_g == '12-14', AG := "PF_m_12_14"]
  pers[gender == 'male' & age_g == '15-17', AG := "PF_m_15_17"]
  pers[gender == 'male' & age_g == '18-20', AG := "PF_m_18_20"]
  pers[gender == 'male' & age_g == '21-24', AG := "PF_m_21_24"]
  pers[gender == 'male' & age_g == '25-29', AG := "PF_m_25_29"]
  pers[gender == 'male' & age_g == '30-34', AG := "PF_m_30_34"]
  pers[gender == 'male' & age_g == '35-39', AG := "PF_m_35_39"]
  pers[gender == 'male' & age_g == '40-44', AG := "PF_m_40_44"]
  pers[gender == 'male' & age_g == '45-49', AG := "PF_m_45_49"]
  pers[gender == 'male' & age_g == '50-54', AG := "PF_m_50_54"]
  pers[gender == 'male' & age_g == '55-59', AG := "PF_m_55_59"]
  pers[gender == 'male' & age_g == '60-64', AG := "PF_m_60_64"]
  pers[gender == 'male' & age_g == '65-74', AG := "PF_m_65_74"]
  pers[gender == 'male' & age_g == '75+', AG := "PF_m_over75"]
  pers[gender == 'female' & age_g == "0-4", AG := "PF_f_0_4"]
  pers[gender == 'female' & age_g == '5-11', AG := "PF_f_5_11"]
  pers[gender == 'female' & age_g == '12-14', AG := "PF_f_12_14"]
  pers[gender == 'female' & age_g == '15-17', AG := "PF_f_15_17"]
  pers[gender == 'female' & age_g == '18-20', AG := "PF_f_18_20"]
  pers[gender == 'female' & age_g == '21-24', AG := "PF_f_21_24"]
  pers[gender == 'female' & age_g == '25-29', AG := "PF_f_25_29"]
  pers[gender == 'female' & age_g == '30-34', AG := "PF_f_30_34"]
  pers[gender == 'female' & age_g == '35-39', AG := "PF_f_35_39"]
  pers[gender == 'female' & age_g == '40-44', AG := "PF_f_40_44"]
  pers[gender == 'female' & age_g == '45-49', AG := "PF_f_45_49"]
  pers[gender == 'female' & age_g == '50-54', AG := "PF_f_50_54"]
  pers[gender == 'female' & age_g == '55-59', AG := "PF_f_55_59"]
  pers[gender == 'female' & age_g == '60-64', AG := "PF_f_60_64"]
  pers[gender == 'female' & age_g == '65-74', AG := "PF_f_65_74"]
  pers[gender == 'female' & age_g == '75+', AG := "PF_f_over75"]

  pers[, D_rur_02 := ifelse(nur == "TR21_rur" ,1,0)]
  pers[, D_rur_03 := ifelse(nur == "TR22_rur" ,1,0)]
  pers[, D_rur_04 := ifelse(nur == "TR31_rur" ,1,0)]
  pers[, D_rur_05 := ifelse(nur == "TR32_rur" ,1,0)]
  pers[, D_rur_06 := ifelse(nur == "TR33_rur" ,1,0)]
  pers[, D_rur_07 := ifelse(nur == "TR41_rur" ,1,0)]
  pers[, D_rur_08 := ifelse(nur == "TR42_rur" ,1,0)]
  pers[, D_rur_09 := ifelse(nur == "TR51_rur" ,1,0)]
  pers[, D_rur_10 := ifelse(nur == "TR52_rur" ,1,0)]
  pers[, D_rur_11 := ifelse(nur == "TR61_rur" ,1,0)]
  pers[, D_rur_12 := ifelse(nur == "TR62_rur" ,1,0)]
  pers[, D_rur_13 := ifelse(nur == "TR63_rur" ,1,0)]
  pers[, D_rur_14 := ifelse(nur == "TR71_rur" ,1,0)]
  pers[, D_rur_15 := ifelse(nur == "TR72_rur" ,1,0)]
  pers[, D_rur_16 := ifelse(nur == "TR81_rur" ,1,0)]
  pers[, D_rur_17 := ifelse(nur == "TR82_rur" ,1,0)]
  pers[, D_rur_18 := ifelse(nur == "TR83_rur" ,1,0)]
  pers[, D_rur_19 := ifelse(nur == "TR90_rur" ,1,0)]
  pers[, D_rur_20 := ifelse(nur == "TRA1_rur" ,1,0)]
  pers[, D_rur_21 := ifelse(nur == "TRA2_rur" ,1,0)]
  pers[, D_rur_22 := ifelse(nur == "TRB1_rur" ,1,0)]
  pers[, D_rur_23 := ifelse(nur == "TRB2_rur" ,1,0)]
  pers[, D_rur_24 := ifelse(nur == "TRC1_rur" ,1,0)]
  pers[, D_rur_25 := ifelse(nur == "TRC2_rur" ,1,0)]
  pers[, D_rur_26 := ifelse(nur == "TRC3_rur" ,1,0)]

  pers[, D_urb_01 := ifelse(nur == "TR10_urb" ,1,0)]
  pers[, D_urb_02 := ifelse(nur == "TR21_urb" ,1,0)]
  pers[, D_urb_03 := ifelse(nur == "TR22_urb" ,1,0)]
  pers[, D_urb_04 := ifelse(nur == "TR31_urb" ,1,0)]
  pers[, D_urb_05 := ifelse(nur == "TR32_urb" ,1,0)]
  pers[, D_urb_06 := ifelse(nur == "TR33_urb" ,1,0)]
  pers[, D_urb_07 := ifelse(nur == "TR41_urb" ,1,0)]
  pers[, D_urb_08 := ifelse(nur == "TR42_urb" ,1,0)]
  pers[, D_urb_09 := ifelse(nur == "TR51_urb" ,1,0)]
  pers[, D_urb_10 := ifelse(nur == "TR52_urb" ,1,0)]
  pers[, D_urb_11 := ifelse(nur == "TR61_urb" ,1,0)]
  pers[, D_urb_12 := ifelse(nur == "TR62_urb" ,1,0)]
  pers[, D_urb_13 := ifelse(nur == "TR63_urb" ,1,0)]
  pers[, D_urb_14 := ifelse(nur == "TR71_urb" ,1,0)]
  pers[, D_urb_15 := ifelse(nur == "TR72_urb" ,1,0)]
  pers[, D_urb_16 := ifelse(nur == "TR81_urb" ,1,0)]
  pers[, D_urb_17 := ifelse(nur == "TR82_urb" ,1,0)]
  pers[, D_urb_18 := ifelse(nur == "TR83_urb" ,1,0)]
  pers[, D_urb_19 := ifelse(nur == "TR90_urb" ,1,0)]
  pers[, D_urb_20 := ifelse(nur == "TRA1_urb" ,1,0)]
  pers[, D_urb_21 := ifelse(nur == "TRA2_urb" ,1,0)]
  pers[, D_urb_22 := ifelse(nur == "TRB1_urb" ,1,0)]
  pers[, D_urb_23 := ifelse(nur == "TRB2_urb" ,1,0)]
  pers[, D_urb_24 := ifelse(nur == "TRC1_urb" ,1,0)]
  pers[, D_urb_25 := ifelse(nur == "TRC2_urb" ,1,0)]
  pers[, D_urb_26 := ifelse(nur == "TRC3_urb" ,1,0)]



  pers[, D_il_01 := ifelse(ILKAYITNO == "01" ,1,0)]
  pers[, D_il_02 := ifelse(ILKAYITNO == "02" ,1,0)]
  pers[, D_il_03 := ifelse(ILKAYITNO == "03" ,1,0)]
  pers[, D_il_04 := ifelse(ILKAYITNO == "04" ,1,0)]
  pers[, D_il_05 := ifelse(ILKAYITNO == "05" ,1,0)]
  pers[, D_il_06 := ifelse(ILKAYITNO == "06" ,1,0)]
  pers[, D_il_07 := ifelse(ILKAYITNO == "07" ,1,0)]
  pers[, D_il_08 := ifelse(ILKAYITNO == "08" ,1,0)]
  pers[, D_il_09 := ifelse(ILKAYITNO == "09" ,1,0)]
  pers[, D_il_10 := ifelse(ILKAYITNO == "10" ,1,0)]
  pers[, D_il_11 := ifelse(ILKAYITNO == "11" ,1,0)]
  pers[, D_il_12 := ifelse(ILKAYITNO == "12" ,1,0)]
  pers[, D_il_13 := ifelse(ILKAYITNO == "13" ,1,0)]
  pers[, D_il_14 := ifelse(ILKAYITNO == "14" ,1,0)]
  pers[, D_il_15 := ifelse(ILKAYITNO == "15" ,1,0)]
  pers[, D_il_16 := ifelse(ILKAYITNO == "16" ,1,0)]
  pers[, D_il_17 := ifelse(ILKAYITNO == "17" ,1,0)]
  pers[, D_il_18 := ifelse(ILKAYITNO == "18" ,1,0)]
  pers[, D_il_19 := ifelse(ILKAYITNO == "19" ,1,0)]
  pers[, D_il_20 := ifelse(ILKAYITNO == "20" ,1,0)]
  pers[, D_il_21 := ifelse(ILKAYITNO == "21" ,1,0)]
  pers[, D_il_22 := ifelse(ILKAYITNO == "22" ,1,0)]
  pers[, D_il_23 := ifelse(ILKAYITNO == "23" ,1,0)]
  pers[, D_il_24 := ifelse(ILKAYITNO == "24" ,1,0)]
  pers[, D_il_25 := ifelse(ILKAYITNO == "25" ,1,0)]
  pers[, D_il_26 := ifelse(ILKAYITNO == "26" ,1,0)]
  pers[, D_il_27 := ifelse(ILKAYITNO == "27" ,1,0)]
  pers[, D_il_28 := ifelse(ILKAYITNO == "28" ,1,0)]
  pers[, D_il_29 := ifelse(ILKAYITNO == "29" ,1,0)]
  pers[, D_il_30 := ifelse(ILKAYITNO == "30" ,1,0)]
  pers[, D_il_31 := ifelse(ILKAYITNO == "31" ,1,0)]
  pers[, D_il_32 := ifelse(ILKAYITNO == "32" ,1,0)]
  pers[, D_il_33 := ifelse(ILKAYITNO == "33" ,1,0)]
  pers[, D_il_34 := ifelse(ILKAYITNO == "34" ,1,0)]
  pers[, D_il_35 := ifelse(ILKAYITNO == "35" ,1,0)]
  pers[, D_il_36 := ifelse(ILKAYITNO == "36" ,1,0)]
  pers[, D_il_37 := ifelse(ILKAYITNO == "37" ,1,0)]
  pers[, D_il_38 := ifelse(ILKAYITNO == "38" ,1,0)]
  pers[, D_il_39 := ifelse(ILKAYITNO == "39" ,1,0)]
  pers[, D_il_40 := ifelse(ILKAYITNO == "40" ,1,0)]
  pers[, D_il_41 := ifelse(ILKAYITNO == "41" ,1,0)]
  pers[, D_il_42 := ifelse(ILKAYITNO == "42" ,1,0)]
  pers[, D_il_43 := ifelse(ILKAYITNO == "43" ,1,0)]
  pers[, D_il_44 := ifelse(ILKAYITNO == "44" ,1,0)]
  pers[, D_il_45 := ifelse(ILKAYITNO == "45" ,1,0)]
  pers[, D_il_46 := ifelse(ILKAYITNO == "46" ,1,0)]
  pers[, D_il_47 := ifelse(ILKAYITNO == "47" ,1,0)]
  pers[, D_il_48 := ifelse(ILKAYITNO == "48" ,1,0)]
  pers[, D_il_49 := ifelse(ILKAYITNO == "49" ,1,0)]
  pers[, D_il_50 := ifelse(ILKAYITNO == "50" ,1,0)]
  pers[, D_il_51 := ifelse(ILKAYITNO == "51" ,1,0)]
  pers[, D_il_52 := ifelse(ILKAYITNO == "52" ,1,0)]
  pers[, D_il_53 := ifelse(ILKAYITNO == "53" ,1,0)]
  pers[, D_il_54 := ifelse(ILKAYITNO == "54" ,1,0)]
  pers[, D_il_55 := ifelse(ILKAYITNO == "55" ,1,0)]
  pers[, D_il_56 := ifelse(ILKAYITNO == "56" ,1,0)]
  pers[, D_il_57 := ifelse(ILKAYITNO == "57" ,1,0)]
  pers[, D_il_58 := ifelse(ILKAYITNO == "58" ,1,0)]
  pers[, D_il_59 := ifelse(ILKAYITNO == "59" ,1,0)]
  pers[, D_il_60 := ifelse(ILKAYITNO == "60" ,1,0)]
  pers[, D_il_61 := ifelse(ILKAYITNO == "61" ,1,0)]
  pers[, D_il_62 := ifelse(ILKAYITNO == "62" ,1,0)]
  pers[, D_il_63 := ifelse(ILKAYITNO == "63" ,1,0)]
  pers[, D_il_64 := ifelse(ILKAYITNO == "64" ,1,0)]
  pers[, D_il_65 := ifelse(ILKAYITNO == "65" ,1,0)]
  pers[, D_il_66 := ifelse(ILKAYITNO == "66" ,1,0)]
  pers[, D_il_67 := ifelse(ILKAYITNO == "67" ,1,0)]
  pers[, D_il_68 := ifelse(ILKAYITNO == "68" ,1,0)]
  pers[, D_il_69 := ifelse(ILKAYITNO == "69" ,1,0)]
  pers[, D_il_70 := ifelse(ILKAYITNO == "70" ,1,0)]
  pers[, D_il_71 := ifelse(ILKAYITNO == "71" ,1,0)]
  pers[, D_il_72 := ifelse(ILKAYITNO == "72" ,1,0)]
  pers[, D_il_73 := ifelse(ILKAYITNO == "73" ,1,0)]
  pers[, D_il_74 := ifelse(ILKAYITNO == "74" ,1,0)]
  pers[, D_il_75 := ifelse(ILKAYITNO == "75" ,1,0)]
  pers[, D_il_76 := ifelse(ILKAYITNO == "76" ,1,0)]
  pers[, D_il_77 := ifelse(ILKAYITNO == "77" ,1,0)]
  pers[, D_il_78 := ifelse(ILKAYITNO == "78" ,1,0)]
  pers[, D_il_79 := ifelse(ILKAYITNO == "79" ,1,0)]
  pers[, D_il_80 := ifelse(ILKAYITNO == "80" ,1,0)]
  pers[, D_il_81 := ifelse(ILKAYITNO == "81" ,1,0)]

  # "gk_arrange" fonksiyonunda "hhbsay" argümani var ve default olarak degeri 9.
  # Kullanici tarafindan girilen degere gore "hhb" sutununu guncelliyoruz.
  # Kullanici "hhbsay = 5" argumanini kullanirsa, veri setinde yer alacak "hhb"
  # sutunu 1-5 araliginda olacaktir.
  pers[, hhb := HHB]
  pers[hhb >= hhbsay, hhb := hhbsay]

  ### ONEMLI - INTERAKTIF DUMMY OLUSTURMA ###
  # Default olarak 9 HHB kategorisi X 2 KIRKENT kategorisi = 18 tane "D_hhb_kk_1_1", "D_hhb_kk_1_2", "..." isminde
  # Dummy degiskenleri for dongusu ile olusturuyoruz. Kodun ilerleyen bolumlerinde kullanici tarafindan
  # belirlenecek HHB kriterine gore kodun kendini guncelleyerek veri setinden bu dummyleri silip
  # kullanicinin belirledigi sayida dummy degiskenleri olusturmasini bekliyoruz. Bu yuzden programa "hhbsay"
  # referansiyla erisim saglayabildigimiz argumani ekledik. Bu objenin alabildigi degerler 2-9 ile
  # sinirli. Default olarak da 9 secilmis olarak program basliyor.
  # Ornegin; kullanici, "hhbsay = 4" sectiginde, program 1, 2, 3, ve 4+ hanehalki buyuklugune gore
  # kalibrasyon yapmak icin 4 X 2 = 8 tane dummy degisken olusturup kalibrasyonu ona gore yapacak.

  if(hhbkirkent == TRUE) {
    for(hhb_kategori in 1:hhbsay) {
      for(kirkent_kategori in 1:2) {
        eval(
          parse(
            text = paste0("pers[, D_hhb_kk_",
                          kirkent_kategori, "_", hhb_kategori,
                          ":= ifelse(hhb ==", hhb_kategori, "& KIR_KENT ==", kirkent_kategori,",1 , 0)]" )))
      }
    }
    pers[, hhb_kirkent := paste0(KIR_KENT,"-",hhb)]
  }

  if(hhbkirkent == FALSE) {
    for(hhb_kategori in 1:hhbsay) {
      eval(
        parse(
          text = paste0("pers[, D_hhb_", hhb_kategori, ":= ifelse(hhb ==", hhb_kategori, ",1 , 0)]" )))
    }
    pers[, hhb_kirkent := hhb]
  }


  pers_aktar <- pers
  hfile <- pers_aktar[, .(NUM_m_0_4 = sum(D_m_0_4), NUM_m_5_11 = sum(D_m_5_11), NUM_m_12_14 = sum(D_m_12_14),
                          NUM_m_15_17 = sum(D_m_15_17), NUM_m_18_20 = sum(D_m_18_20), NUM_m_21_24 = sum(D_m_21_24),
                          NUM_m_25_29 = sum(D_m_25_29), NUM_m_30_34 = sum(D_m_30_34), NUM_m_35_39 = sum(D_m_35_39),
                          NUM_m_40_44 = sum(D_m_40_44), NUM_m_45_49 = sum(D_m_45_49), NUM_m_50_54 = sum(D_m_50_54),
                          NUM_m_55_59 = sum(D_m_55_59), NUM_m_60_64 = sum(D_m_60_64), NUM_m_65_74 = sum(D_m_65_74),
                          NUM_m_over75 = sum(D_m_over75), NUM_f_0_4 = sum(D_f_0_4), NUM_f_5_11 = sum(D_f_5_11),
                          NUM_f_12_14 = sum(D_f_12_14), NUM_f_15_17 = sum(D_f_15_17), NUM_f_18_20 = sum(D_f_18_20),
                          NUM_f_21_24 = sum(D_f_21_24), NUM_f_25_29 = sum(D_f_25_29), NUM_f_30_34 = sum(D_f_30_34),
                          NUM_f_35_39 = sum(D_f_35_39), NUM_f_40_44 = sum(D_f_40_44), NUM_f_45_49 = sum(D_f_45_49),
                          NUM_f_50_54 = sum(D_f_50_54), NUM_f_55_59 = sum(D_f_55_59), NUM_f_60_64 = sum(D_f_60_64),
                          NUM_f_65_74 = sum(D_f_65_74), NUM_f_over75 = sum(D_f_over75)), by = bulten]
  hfile[, DH_m_0_4  := ifelse(NUM_m_0_4 == 0, 0,1)]
  hfile[, DH_m_5_11 := ifelse(NUM_m_5_11 == 0, 0,1)]
  hfile[, DH_m_12_14:= ifelse(NUM_m_12_14 == 0, 0,1)]
  hfile[, DH_m_15_17:= ifelse(NUM_m_15_17 == 0, 0,1)]
  hfile[, DH_m_18_20:= ifelse(NUM_m_18_20 == 0, 0,1)]
  hfile[, DH_m_21_24:= ifelse(NUM_m_21_24 == 0, 0,1)]
  hfile[, DH_m_25_29:= ifelse(NUM_m_25_29 == 0, 0,1)]
  hfile[, DH_m_30_34:= ifelse(NUM_m_30_34 == 0, 0,1)]
  hfile[, DH_m_35_39:= ifelse(NUM_m_35_39 == 0, 0,1)]
  hfile[, DH_m_40_44:= ifelse(NUM_m_40_44 == 0, 0,1)]
  hfile[, DH_m_45_49:= ifelse(NUM_m_45_49 == 0, 0,1)]
  hfile[, DH_m_50_54:= ifelse(NUM_m_50_54 == 0, 0,1)]
  hfile[, DH_m_55_59:= ifelse(NUM_m_55_59 == 0, 0,1)]
  hfile[, DH_m_60_64:= ifelse(NUM_m_60_64 == 0, 0,1)]
  hfile[, DH_m_65_74:= ifelse(NUM_m_65_74 == 0, 0,1)]
  hfile[, DH_m_over75:= ifelse(NUM_m_over75 == 0, 0,1)]
  hfile[, DH_f_0_4:= ifelse(NUM_f_0_4 == 0, 0,1)]
  hfile[, DH_f_5_11:= ifelse(NUM_f_5_11 == 0, 0,1)]
  hfile[, DH_f_12_14:= ifelse(NUM_f_12_14 == 0, 0,1)]
  hfile[, DH_f_15_17:= ifelse(NUM_f_15_17 == 0, 0,1)]
  hfile[, DH_f_18_20:= ifelse(NUM_f_18_20 == 0, 0,1)]
  hfile[, DH_f_21_24:= ifelse(NUM_f_21_24 == 0, 0,1)]
  hfile[, DH_f_25_29:= ifelse(NUM_f_25_29 == 0, 0,1)]
  hfile[, DH_f_30_34:= ifelse(NUM_f_30_34 == 0, 0,1)]
  hfile[, DH_f_35_39:= ifelse(NUM_f_35_39 == 0, 0,1)]
  hfile[, DH_f_40_44:= ifelse(NUM_f_40_44 == 0, 0,1)]
  hfile[, DH_f_45_49:= ifelse(NUM_f_45_49 == 0, 0,1)]
  hfile[, DH_f_50_54:= ifelse(NUM_f_50_54 == 0, 0,1)]
  hfile[, DH_f_55_59:= ifelse(NUM_f_55_59 == 0, 0,1)]
  hfile[, DH_f_60_64:= ifelse(NUM_f_60_64 == 0, 0,1)]
  hfile[, DH_f_65_74:= ifelse(NUM_f_65_74 == 0, 0,1)]
  hfile[, DH_f_over75:= ifelse(NUM_f_over75 == 0, 0,1)]
  hfile[, country := 1]

  setkey(hfile, bulten)
  setkey(pers_aktar, bulten)
  pers_h0_temel <- pers_aktar[hfile, nomatch = 0]

  pers_h_temel <- cbind(pers_h0_temel, proj_yascins, proj_nutskirkent, proj_il, pop_prop_hh_kirkent_adnks_satir)

  pers_h_temel[, w := duzfakto]
  pers_h_temel[, w_old := w]

  pers <- pers_h_temel

  # Istanbul'un kiri olmadigi icin PF_rur_01 sutununu siliyoruz.
  pers <- pers %>% select(-PF_rur_01)

  return(pers)
}
