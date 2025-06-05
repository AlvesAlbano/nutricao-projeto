(ns org.nutricao_projeto.saldo_calorico)

(defn calcular-saldo-calorico[calorias-ganhas calorias-perdidas ]
  (let [saldo (- calorias-ganhas calorias-perdidas)]
    (if (neg? saldo)
      (format "Você Perdeu %d calorias!" (Math/abs saldo))
      (format "Você Ganhou %d calorias!" saldo)
      )
    )
  )

(defn -main[]
  (println (calcular-saldo-calorico 200 300))
  )