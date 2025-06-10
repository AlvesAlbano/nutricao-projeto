(ns org.nutricao_projeto.data
  (:import (java.time LocalDate)
           (java.time.format DateTimeFormatter)))

(def data-formato (DateTimeFormatter/ofPattern "dd/MM/yyyy"))

(defn intervalo? [data data-inicio data-fim]
  (and (or (.isEqual data data-inicio) (.isAfter data data-inicio))
       (or (.isEqual data data-fim) (.isBefore data data-fim))))

(defn filtrar-data [item data-inicio data-fim]
  (let [data (LocalDate/parse (:data item) data-formato)
        data-inicio-formatada (LocalDate/parse data-inicio data-formato)
        data-fim-formatada (LocalDate/parse data-fim data-formato)]

    (intervalo? data data-inicio-formatada data-fim-formatada)
    )
  )

(defn entre-datas [lista data-inicial data-fim]
  (filter #(filtrar-data % data-inicial data-fim) lista)
  )

(defn -main []
  (let [dados [{:nome "João" :data "01/05/2024"}
               {:nome "Maria" :data "15/05/2024"}
               {:nome "Carlos" :data "01/06/2024"}
               {:nome "Ana" :data "10/06/2024"}
               {:nome "Pedro" :data "20/06/2024"}]

        data-inicio "10/05/2024"
        data-fim    "15/06/2024"

        resultado   (entre-datas dados data-inicio data-fim)]

    (println "Itens entre as datas:")
    (doseq [item resultado]
      (println item))))