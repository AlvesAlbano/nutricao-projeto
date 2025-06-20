(ns org.nutricao_projeto.saldo_calorico)

(defn calcular-saldo-calorico [calorias-ganhas calorias-perdidas]
  (let [saldo (- calorias-ganhas calorias-perdidas)]
    ;(println (type saldo))
    (if (neg? saldo)
      (format "Você Perdeu %.0f calorias!" (Math/abs saldo))
      (format "Você Ganhou %.0f calorias!" saldo))))

(defn -main []
  (println (calcular-saldo-calorico 200 300)))
