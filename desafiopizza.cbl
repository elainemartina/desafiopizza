      *---- Divisão de Identificação do Programa
       identification division.
       program-id. "desafiopizza".
       author. "Elaine Martina André".
       Installation. "PC".
       date-written. 10/07/2020.
       date-compiled. 10/07/2020

      *---- Divisão Para Configuração do Ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *---- Declaração de Recursos Externos
       input-output section.
       file-control.
       I-O-Control.

      *---- Declaração de Variáveis
       data division.

      *---- Variaveis de Arquivos
       file section.

      *---- Variaveis de Trabalho
       working-storage section.

       01  relatorio occurs 20.
           05 nome                                 pic x(15)
                                                   value spaces.
           05 filler                               pic x(03)
                                                   value  " - ".
           05 diametro                             pic 9(03).
           05 filler                               pic x(03)
                                                   value  " - ".
           05 preco                                pic 9(03)v99.
           05 filler                               pic x(03)
                                                   value  " - ".
           05 preco_cm2                            pic 9(03)v99.
           05 filler                               pic x(03)
                                                   value  " - ".
           05 diferenca_rel                        pic 9(03)v99.

       01  aux.
           05 nome_aux                             pic x(15)
                                                   value spaces.
           05 filler                               pic x(03)
                                                   value  " - ".
           05 diametro_aux                         pic 9(03).
           05 filler                               pic x(03)
                                                   value  " - ".
           05 preco_aux                            pic 9(03)v99.
           05 filler                               pic x(03)
                                                   value  " - ".
           05 preco_cm2_aux                        pic 9(03)v99.
           05 filler                               pic x(03)
                                                   value  " - ".
           05 diferenca_rel_aux                    pic 9(03)v99.

       77  delta_preco_cm2                         pic 9(03)v99.
       77  raio                                    pic 9(03)v99.
       77  area_pizza                              pic 9(03)v99.
       77  pi                                      pic 9(01)v999999
                                                   value 3,141592.
       77  ind                                     pic 9(02).
       77  menu                                    pic x(01).
       77  controle                                pic x(10).
      * ---------------------------------------------------------------
      *---- Variaveis para comunicação entre programas
       linkage section.

      *---- Declaração de tela
       screen section.

      *---- Declaração do corpo do programa
       procedure division.

           perform inicializa.
           perform processamento.
           perform finaliza.

       inicializa section.

           move     "S"       to     menu
           move   "trocou"    to     controle
           .
       inicializa-exit.
           exit.

      * ----------------------------------------------------------------
      * Construcao do Laco Principal (menu) ou Regra de Negocio
       processamento section.

           move 0 to ind
           perform until menu <> "S"
               display erase

               add 1 to ind

               if ind > 20 then
                   display "Voce Atingiu o Limite de 20 Pizzas"
               else
                   display "Informe o Nome da Pizza "
                   accept nome(ind)

                   display "Informe o Diametro da Pizza "
                   accept diametro(ind)

                   display "Informe o Preco da Pizza "
                   accept preco(ind)

               end-if

               perform calculo-preco-cm2

               display "Deseja Cadastrar Mais Uma Pizza? ('S'/'N')"
               accept menu

           end-perform

           perform ordenar
           perform calculo-porcentagem

           perform varying ind from 1 by 1 until ind > 20
                                              or nome(ind) = space
               display relatorio(ind)

           end-perform

           .
       processamento-exit.
           exit.
      * ---------------------------------------------------------------
      * ------------------- Calculo do Preco em Cm2 -------------------
       calculo-preco-cm2 section.

           compute raio = diametro(ind) / 2

           compute area_pizza = pi * (raio * raio)

           compute preco_cm2(ind) = preco(ind) / area_pizza

           .
       calculo-preco-cm2-exit.
           exit.
      * ---------------------------------------------------------------
      * ------------------------- Ordenacao ---------------------------
       ordenar section.

           move "trocou" to controle
           perform until controle <> "trocou"

               move 1 to ind
               move "n_trocou" to controle

               perform until ind = 20
                          or nome(ind + 1) = space

                   if preco_cm2(ind) > preco_cm2(ind + 1) then
                       move relatorio(ind + 1) to aux
                       move relatorio(ind)     to relatorio(ind + 1)
                       move aux                to relatorio(ind)

                       move "trocou"           to controle
                   end-if

                   add 1 to ind

               end-perform

           end-perform

           .
       ordenar-exit.
           exit.
      * ---------------------------------------------------------------
      * -------------------- Calculo da Porcentagem --------------------
       calculo-porcentagem section.

           move 1 to ind

           perform until ind = 20
                      or nome(ind + 1) = space

      * ---- Diferenca de Precos (Delta)
               compute delta_preco_cm2
                       = preco_cm2(ind + 1) - preco_cm2(ind)

      * ---- Porcentagem
               compute diferenca_rel(ind + 1)
                       = (delta_preco_cm2 * 100) / preco_cm2(ind)

               add 1 to ind

           end-perform

           .
       calculo-porcentagem-exit.
           exit.
      * ---------------------------------------------------------------
      * ---------------------------- Fim ------------------------------
       finaliza section.

           stop run
           .
       finaliza-exit.
           exit.












