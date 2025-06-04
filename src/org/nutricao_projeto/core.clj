(ns org.nutricao_projeto.core
  (:require [org.nutricao_projeto.alimento.alimento_controller :as alimento]
            [org.nutricao_projeto.usuario.usuario_operacoes :as usuario]
            [org.nutricao_projeto.traducao.traduzir_frase :as trad]
            [org.nutricao_projeto.exercicio.exercicio_operacoes :as exercicio]))

(defn menu-usuario []
  (println "Digite sua altura: ")
  (let [altura (read)
        _ (println "Digite seu peso: ")
        peso (read)
        _ (println "Digite sua idade: ")
        idade (read)
        _ (println "Digite seu sexo: ")
        sexo (read)
        usuario-dados {:altura altura :peso peso :idade idade :sexo sexo}]
    (usuario/cadastrar-usuario usuario-dados)
    (println "Usuário cadastrado!" usuario-dados)))

(defn menu []
  (println "=== Menu Nutricional ===")
  (println "1. Adicionar refeição")
  (println "2. Mostrar refeições do dia")
  (println "3. Sair")
  (println "9. Adicionar exercício")
  (println "10. Mostrar calorias perdidas")
  (println "Escolha uma opção:")
  (let [entrada (read-line)]
    (if (alimento/inteiro? entrada)
      (Integer/parseInt entrada)
      -1)))

(defn imprimir-refeicoes [refeicoes]
  (println "----------------------------------------------------------------------------------------------------------")
  (println (format "%-25s %8s %10s %10s %10s %14s %20s"
                   "Alimento" "Qtd(g)" "Calorias" "Proteína" "Gordura" "Carboidrato" "Data/Hora"))
  (println "----------------------------------------------------------------------------------------------------------")
  (letfn [(imprimir-rec [lst acc]
            (if (empty? lst)
              (do
                (println "----------------------------------------------------------------------------------------------------------")
                (println (format "Total de calorias consumidas no dia: %.2f kcal" acc)))
              (let [r (first lst)
                    cal (:calorias r)]
                (println (format "%-25s %8.1f %10.2f %10.2f %10.2f %14.2f %20s"
                                 (:alimento r)
                                 (:quantidade r)
                                 (:calorias r)
                                 (:proteina r)
                                 (:gordura r)
                                 (:carboidrato r)
                                 (:data-hora r)))
                (recur (rest lst) (+ acc cal)))))]
    (imprimir-rec refeicoes 0.0)))

(defn adicionar-refeicoes [nomes refeicoes]
  (letfn [(loop-nomes [restantes refeicoes-atualizadas]
            (if (empty? restantes)
              refeicoes-atualizadas
              (let [nome (first restantes)
                    opcoes (alimento/traduzir-e-filtrar-alimentos nome)]
                (println (str "\nSelecione o alimento correspondente para: " nome))
                (letfn [(mostrar-opcoes [lst idx]
                          (if (empty? lst)
                            nil
                            (let [a (first lst)
                                  nome (:descricao-pt a)
                                  marca (or (:brandName a) "-")
                                  porcao (or (:servingSize a) "??")
                                  unidade (or (:servingSizeUnit a) "g")
                                  categoria (try (trad/ingles-portugues (or (:foodCategory a) "-")) (catch Exception _ "-"))]
                              (println (format "%d. %s (Marca: %s)\n   Porção: %s %s | Categoria: %s"
                                               idx nome marca porcao unidade categoria))
                              (recur (rest lst) (inc idx)))))]

                  (mostrar-opcoes opcoes 1)
                  (let [idx-str (read-line)
                        idx (try (Integer/parseInt idx-str) (catch Exception _ 0))]
                    (if (or (< idx 1) (> idx (count opcoes)))
                      (do
                        (println "Opção inválida. Pulando alimento.")
                        (recur (rest restantes) refeicoes-atualizadas))
                      (let [selecionado (nth opcoes (dec idx))]
                        (println (str "Digite a quantidade consumida em gramas de " (:descricao-pt selecionado) ":"))
                        (let [qtd-str (read-line)]
                          (if (alimento/double-str? qtd-str)
                            (let [qtd (Double/parseDouble qtd-str)
                                  nova-refeicao (alimento/montar-refeicao selecionado qtd)]
                              (alimento/enviar-refeicao nova-refeicao)
                              (println (str "Adicionado: " (:descricao-pt selecionado) " (" qtd "g)"))
                              (recur (rest restantes) (conj refeicoes-atualizadas nova-refeicao)))
                            (do
                              (println "Quantidade inválida. Pulando alimento.")
                              (recur (rest restantes) refeicoes-atualizadas)))))))))))]
    (loop-nomes nomes refeicoes)))

(defn executar [refeicoes]
  (let [opcao (menu)]
    (cond
      (= opcao 1)
      (do
        (println "Digite os alimentos consumidos separados por vírgula (ex: arroz, feijao, ovo):")
        (let [linha (read-line)
              nomes (clojure.string/split linha #",\s*")
              novas-refeicoes (adicionar-refeicoes nomes refeicoes)]
          (recur novas-refeicoes)))

      (= opcao 2)
      (do
        (imprimir-refeicoes refeicoes)
        (recur refeicoes))

      (= opcao 3)
      (println "Saindo...")

      ;; === OPÇÕES ADICIONADAS PARA EXERCÍCIO ===

      (= opcao 9)
      (do
        (println "Diga o nome do exercício: ")
        (let [nome-exercicio (read-line)
              peso-usuario (usuario/get-peso)
              _ (println "Digite a duração do exercício em minutos: ")
              duracao (read)
              _ (println (format "Perda calórica baseada no seu peso atual %.2f Kg e tempo gasto %d min" peso-usuario duracao))
              lista-exercicios (exercicio/listar-exercicios nome-exercicio peso-usuario duracao)
              _ (println (exercicio/enumerar-exercicios lista-exercicios))
              _ (println "Escolha o exercício realizado: ")
              indice (read)
              exercicio-selecionado (exercicio/selecionar-exercicio lista-exercicios indice)
              _ (println "Exercício escolhido: " exercicio-selecionado)
              _ (read-line)
              _ (println "Informe a data em que o exercício foi realizado: (Ex: dia/mês/ano)")
              data (read-line)
              exercicio-com-data (exercicio/adicionar-data exercicio-selecionado data)
              _ (exercicio/registrar-perda exercicio-com-data)
              _ (println (format "Exercício (%s) adicionado com sucesso!" exercicio-com-data))]
          (recur refeicoes)))

      (= opcao 10)
      (do
        (println "Perdas calóricas registradas: ")
        (let [lista-calorias-perdidas (exercicio/calorias-perdidas)
              calorias-perdidas (exercicio/soma-calorias-perdidas)]
          (println "" lista-calorias-perdidas)
          (println "Total de calorias perdidas:" calorias-perdidas))
        (recur refeicoes))

      :else
      (do
        (println "Opção inválida.")
        (recur refeicoes)))))

(defn -main []
  (menu-usuario)
  (executar []))
