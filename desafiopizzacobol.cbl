      *Divisão de identificação do programa
       Identification Division.
       Program-id. "desafiopizzacobol".
       Author. "Julia Krüger".
       Installation. "PC".
       Date-written. 09/07/2020.
       Date-compiled. 09/07/2020.

      *Divisão para configuração do ambiente
       Environment Division.
       Configuration Section.
           special-names. decimal-point is comma.

      *   Declaração dos recursos externos
       Input-output Section.
       File-control.
       I-O-Control.


      *Declaração de variáveis
       Data Division.

      *----Variáveis de arquivos
       File Section.

      *----Variáveis de trabalho
       Working-storage Section.

      * declaração das variáveis
       01 relatorio occurs 50.
           05 nome                                 pic x(15).
           05 filler                               value space.
           05 diametro                             pic 9(03)V99
                                                   value 0.
           05 filler                               value space.
           05 filler                               value space.
           05 filler                               value space.
           05 preco                                pic 9(03)V99
                                                   value 0.
           05 filler                               value space.
           05 filler                               value space.
           05 preco_cm2                            pic 9(03)V99
                                                   value 0.
           05 filler                               value space.
           05 filler                               value space.
           05 filler                               value space.
           05 filler                               value space.
           05 filler                               value space.

           05 porcent                              pic S9(03)V99
                                                   value 0.

       01 relatorio_tab occurs 50.
           05 nome_tab                             pic x(15)
                                                   value "Nome".
           05 filler                               value space.
           05 diametro_tab                         pic x(08)
                                                   value "Tamanho".
           05 preco_tab                            pic x(06)
                                                   value "Preco".
           05 filler                               value space.
           05 precocm2_tab                         pic x(08)
                                                   value "R$ p/cm2".
           05 filler                               value space.
           05 filler                               value space.
           05 porcent_tab                          pic x(12)
                                                   value "Diferenca %".



       77 ind                                      pic 9(2)
                                                   value 0.
       77 auxprecocm2                              pic 9(03)V99.
       77 auxnome                                  pic x(15).
       77 auxdiametro                              pic 9(03)V99.
       77 auxpreco                                 pic 9(03)V99.
       77 aux                                      pic x(1).
       77 fimprograma                              pic x(03).
       77 controle                                 pic x(08).
       77 quantidade                               pic 9(3).
       01 varporcentagem occurs 50.
           05 diferenca                            pic S9(03)V99
                                                   value 0.

      *----Variáveis para comunicação entre programas
       .Linkage Section.

      *----Declaração de tela
       Screen Section.


      *Declaração do corpo do programa
       Procedure Division.

      * executando as sections
           perform inicializa.
           perform processamento.
           perform finaliza.

      * section inicializa(vazia)
           inicializa section.
               next sentence
               .
           inicializa-exit.
               exit.

      * section processamento, recebendo as informações
           processamento section.
               display erase
               perform until fimprograma = "Nao"
                   add 1 to ind
                   if ind <= 50
                       display "Informe o nome da pizza: "
                       accept nome(ind)
                       display "Informe o diametro da pizza: "
                       accept diametro(ind)
                       display "Informe o preco da pizza: "
                       accept preco(ind)

                       perform calculo_cm2

                       display "Deseja cadastrar outra pizza? (Sim/Nao)"
                       accept fimprograma
                   else
                       display "Voce atingiu o limite de pizzas!"
                       display "Tecle enter!"
                       accept aux
                       move "Nao" to fimprograma
                   end-if
                   display erase
               end-perform

               move ind to quantidade

      * executar a section ordenar
               perform ordenar
      * executar a section porcentagem
               perform porcentagem
      * executar a section imprimetab
               perform imprimetab
               .
           processamento-exit.
               exit.



      *cálculo do preço por centímetro quadrado
           calculo_cm2 section.
               compute preco_cm2(ind) = preco(ind) /
               (((diametro(ind) / 2)*(diametro(ind) / 2))* 3,14)
               .
           calculo_cm2-exit.
               exit.



      * perform para ordenar as informações
           ordenar section.
               move "trocou" to controle
               move 1 to ind
               perform until controle <> "trocou"
                   move "Ntrocou" to controle
                   perform until ind >= quantidade
                       if preco_cm2(ind) > preco_cm2(ind + 1) then
                           move preco_cm2(ind + 1) to auxprecocm2
                           move nome(ind + 1) to auxnome
                           move diametro(ind + 1) to auxdiametro
                           move preco(ind + 1) to auxpreco
                           move preco_cm2(ind) to preco_cm2(ind + 1)
                           move nome(ind) to nome(ind + 1)
                           move diametro(ind) to diametro(ind + 1)
                           move preco(ind) to preco(ind + 1)
                           move auxprecocm2 to preco_cm2(ind)
                           move auxnome to nome(ind)
                           move auxdiametro to diametro(ind)
                           move auxpreco to preco(ind)
                           move "trocou" to controle
                       end-if
                       add 1 to ind
                   end-perform
                   move 1 to ind
               end-perform
               .
           ordenar-exit.
               exit.
      * cálculo da diferença percentual
           porcentagem section.
               move 2 to ind
               perform until ind > quantidade
                   compute diferenca(ind) = preco_cm2(ind) -
                   preco_cm2(ind - 1)
                   compute porcent(ind)
                   = (diferenca(ind) * 100) / preco_cm2(ind - 1)
                   add 1 to ind
               end-perform
               .
           porcentagem-exit.
               exit.

      * section para exibir na tela as informações finais
           imprimetab section.
               display "Ordem de melhor custo beneficio: "
               perform varying ind from 1 by 1 until ind > quantidade
                   display relatorio_tab(ind)
                   display relatorio(ind)
               end-perform
               .
           imprimetab-exit.
               exit.

      * section para finalizar o programa
           finaliza section.
               stop run
               .
           finaliza-exit.
               exit.

























