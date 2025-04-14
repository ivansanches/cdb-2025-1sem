       IDENTIFICATION              DIVISION.
       PROGRAM-ID. MENU.
      *=================================================================
      *==  OBJETIVO: MENU PARA PROJETO DE GESTAO DE FILMES
      *==  AUTOR: GABRIEL CARVALHO
      *=================================================================
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
           SELECT FILMES ASSIGN TO "..\dados\FILMES.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS FILMES-STATUS
               RECORD KEY IS FILMES-CHAVE.

           SELECT RELATORIO ASSIGN TO "..\dados\FILMES.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FILMES2-STATUS
               RECORD KEY IS  FILMES2-CHAVE.

       DATA                        DIVISION.
       FILE                        SECTION.
       FD  FILMES.
       01  FILMES-REG.
           05  FILMES-CHAVE.
               10  CODFILME    PIC 9(005).
           05  TITULO          PIC X(030).
           05  GENERO          PIC X(008).
           05  DURACAO         PIC 9(003).
           05  DISTRIBUIDORA   PIC X(015).
           05  NOTA            PIC 9(002).

       FD  RELATORIO.
       01  FILMES2-REG.
           05  FILMES2-CHAVE.
               10  CODFILME2    PIC 9(005).
           05  TITULO2          PIC X(030).
           05  GENERO2          PIC X(008).
           05  DURACAO2         PIC 9(003).
           05  DISTRIBUIDORA2   PIC X(015).
           05  NOTA2            PIC 9(002).

       WORKING-STORAGE             SECTION.
       77  WRK-OPCAO           PIC X(001).
       77  WRK-TITULO          PIC X(020) VALUES "MENU".
       77  FILMES-STATUS       PIC 9(002).
       77  FILMES2-STATUS      PIC 9(002).
       77  WRK-MSG-ERRO        PIC X(050).
       77  WRK-TECLA           PIC X(001).
       77  WRK-LINHA           PIC 9(002) VALUES 0.

       77  WRK-LIDOS           PIC 9(005) VALUES ZEROS.
       77  WRK-LIDOS-ED        PIC ZZ.ZZ9 VALUES ZEROS.
       77  WRK-PAGINA          PIC 9(005) VALUES ZEROS.
       77  WRK-PAGINA-ED       PIC ZZ.ZZ9 VALUES ZEROS.
       77  WRK-NOTA-ERRO       PIC 9(001) VALUES ZEROS.

       SCREEN                      SECTION.
       01  TELA.
           05  LIMPA-TELA.
               10  BLANK SCREEN.
               10  LINE 01 COLUMN 01 PIC X(020) ERASE EOL
                   BACKGROUND-COLOR 4.
               10  LINE 01 COLUMN 01 PIC X(050)
                   BACKGROUND-COLOR 4 FROM
                       'GESTOR DE FILMES'.
               10  LINE 02 COLUMN 01 PIC X(020) ERASE EOL
                   BACKGROUND-COLOR 0 FROM WRK-TITULO
                   FOREGROUND-COLOR 9.

       01  AVISO-SAIR.
           15  LINE 02 COLUMN 22 PIC X(030)
               BACKGROUND-COLOR 0 FROM 'PRESSIONE F3 PARA SAIR'
               FOREGROUND-COLOR 9.

       01  FILLER.
           05  LIMPA-TELA-GERAL.
               10  BLANK SCREEN.

       01  MENU.
           05  LINE 07 COLUMN 15 VALUE '1 - INCLUIR'.
           05  LINE 08 COLUMN 15 VALUE '2 - CONSULTAR'.
           05  LINE 09 COLUMN 15 VALUE '3 - ALTERAR'.
           05  LINE 10 COLUMN 15 VALUE '4 - EXCLUIR'.
           05  LINE 11 COLUMN 15 VALUE '5 - RELATORIO'.
           05  LINE 12 COLUMN 15 VALUE 'X - ENCERRAR'.
           05  LINE 13 COLUMN 15 VALUE 'OPCAO...'.
           05  LINE 13 COLUMN 24 USING WRK-OPCAO.

       01  TELA-REGISTRO.
           05  CHAVE FOREGROUND-COLOR 2.
               10  LINE 10 COLUMN 10 VALUE 'CODIGO FILME..'.
               10  COLUMN PLUS 2 PIC 9(005) USING CODFILME
                   BLANK WHEN ZEROS.
           05  SS-DADOS.
               10  LINE 11 COLUMN 10 VALUE 'TITULO..'.
               10  COLUMN PLUS 2 PIC X(030) USING TITULO.
               10  LINE 12 COLUMN 10 VALUE 'GENERO..'.
               10  COLUMN PLUS 2 PIC X(008) USING GENERO.
               10  LINE 13 COLUMN 10 VALUE 'DURACAO..'.
               10  COLUMN PLUS 2 PIC 9(003) USING DURACAO
                   BLANK WHEN ZEROS.
               10  LINE 14 COLUMN 10 VALUE 'DISTRIBUIDORA..'.
               10  COLUMN PLUS 2 PIC X(015) USING DISTRIBUIDORA.
               10  LINE 15 COLUMN 10 VALUE 'NOTA..'.
               10  COLUMN PLUS 2 PIC 9(002) USING NOTA
                   BLANK WHEN ZEROS.

       01  MOSTRA-ERRO.
           05  MSG-ERRO.
               10  LINE 22 COLUMN 10 PIC X(020) ERASE EOL.
               10  LINE 22 COLUMN 10 PIC X(050)
                   FROM WRK-MSG-ERRO.
               10  COLUMN PLUS 2     PIC X(001)
                   USING WRK-TECLA.

       PROCEDURE                   DIVISION.
       0001-PRINCIPAL              SECTION.
           PERFORM 0100-INICIALIZAR.
           PERFORM 0200-PROCESSAR UNTIL
               WRK-OPCAO EQUAL 'X' OR WRK-OPCAO EQUAL 'x'.
           PERFORM 0300-FINALIZAR.
           STOP RUN.

       0100-INICIALIZAR            SECTION.
           OPEN I-O FILMES
           IF FILMES-STATUS EQUAL 35 THEN
               OPEN OUTPUT FILMES
               CLOSE FILMES
               OPEN I-O FILMES
           END-IF.

           MOVE SPACES TO WRK-OPCAO.
           MOVE 'MENU' TO WRK-TITULO.
           DISPLAY TELA.
           ACCEPT MENU.

       0200-PROCESSAR              SECTION.
           MOVE SPACES TO FILMES-REG WRK-TECLA WRK-MSG-ERRO.
           EVALUATE WRK-OPCAO
               WHEN 1
                   PERFORM 0400-INCLUIR
               WHEN 2
                   PERFORM 0500-CONSULTA
               WHEN 3
                   PERFORM 0600-ALTERAR
               WHEN 4
                   PERFORM 0700-EXCLUIR
               WHEN 5
                   PERFORM 0800-RELATORIO-TELA

               WHEN OTHER
                   IF WRK-OPCAO NOT EQUAL "X" AND NOT EQUAL "x"
                        MOVE 'OPCAO INVALIDA!' TO WRK-MSG-ERRO
                        ACCEPT MOSTRA-ERRO
                   END-IF
           END-EVALUATE.
           PERFORM 0100-INICIALIZAR.

       0300-FINALIZAR              SECTION.
           CLOSE FILMES.

       0400-INCLUIR                SECTION.
           MOVE 'INCLUIR' TO WRK-TITULO.

           PERFORM UNTIL WRK-TECLA EQUAL "N"
               DISPLAY TELA
               DISPLAY AVISO-SAIR
               ACCEPT TELA-REGISTRO

               READ FILMES
               INVALID KEY
                   PERFORM 0830-VALIDAR-NOTA
                   IF WRK-NOTA-ERRO EQUAL 1
                       MOVE 0 TO WRK-NOTA-ERRO
                       DISPLAY "TENTAR GRAVAR NOVAMENTE? (S/N)"
                           LINE 21 COLUMN 10
                   ELSE
                       DISPLAY "DESEJA GRAVAR? (S/N)" LINE 20 COLUMN 10
                       ACCEPT WRK-TECLA               LINE 20 COLUMN 60

                       IF WRK-TECLA NOT EQUAL "S" AND
                           WRK-TECLA NOT EQUAL "s"
                           PERFORM 0820-CANCELAR
                           EXIT SECTION
                       END-IF

                       WRITE FILMES-REG
                           INVALID KEY
                               MOVE 'FILME JA EXISTE' TO WRK-MSG-ERRO
                               ACCEPT MOSTRA-ERRO
                           NOT INVALID KEY
                               MOVE 'FILME CRIADO' TO WRK-MSG-ERRO
                               DISPLAY MOSTRA-ERRO
                       END-WRITE

                       DISPLAY "DESEJA GRAVAR MAIS UM REGISTRO? (S/N)"
                               LINE 21 COLUMN 10
                   END-IF
               NOT INVALID KEY
                   MOVE 'FILME JA EXISTE' TO WRK-MSG-ERRO
                   DISPLAY MOSTRA-ERRO
                   DISPLAY "TENTAR GRAVAR NOVAMENTE? (S/N)"
                           LINE 21 COLUMN 10
               END-READ

               ACCEPT  WRK-TECLA               LINE 21 COLUMN 60
               INITIALIZE FILMES-REG
           END-PERFORM.

       0500-CONSULTA               SECTION.
           MOVE 'CONSULTA' TO WRK-TITULO.
           MOVE SPACES     TO WRK-TECLA.

           PERFORM UNTIL WRK-TECLA EQUAL "N" OR WRK-TECLA EQUAL "n"
               DISPLAY TELA
               DISPLAY AVISO-SAIR
               DISPLAY TELA-REGISTRO
               ACCEPT CHAVE
               READ FILMES
                   INVALID KEY
                       MOVE 'FILME NAO ENCONTRADO' TO WRK-MSG-ERRO
                   NOT INVALID KEY
                       MOVE 'FILME ENCONTRADO' TO WRK-MSG-ERRO
                       DISPLAY SS-DADOS
               END-READ

               DISPLAY MOSTRA-ERRO


               DISPLAY "DESEJA REALIZAR UMA NOVA CONSULTA? (S/N)"
                                               LINE 20 COLUMN 10
               ACCEPT  WRK-TECLA               LINE 20 COLUMN 60
               INITIALIZE FILMES-REG
           END-PERFORM.

       0600-ALTERAR               SECTION.
           MOVE 'ALTERAR' TO WRK-TITULO.

           PERFORM UNTIL WRK-TECLA EQUAL "N" OR WRK-TECLA EQUAL "n"
               MOVE SPACES TO WRK-TECLA
               DISPLAY TELA
               DISPLAY AVISO-SAIR
               DISPLAY TELA-REGISTRO
               ACCEPT CHAVE
               READ FILMES
                   INVALID KEY
                       MOVE 'FILME NAO ENCONTRADO' TO WRK-MSG-ERRO
                       DISPLAY MOSTRA-ERRO
                       DISPLAY "DESEJA TENTAR OUTRO FILME? (S/N)"
                                LINE 21 COLUMN 10
                   NOT INVALID KEY
                       MOVE 'FILME ENCONTRADO' TO WRK-MSG-ERRO
                       DISPLAY MOSTRA-ERRO
                       ACCEPT SS-DADOS

                       PERFORM 0830-VALIDAR-NOTA
                       IF WRK-NOTA-ERRO EQUAL 1
                           MOVE 0 TO WRK-NOTA-ERRO
                           DISPLAY "TENTAR GRAVAR NOVAMENTE? (S/N)"
                               LINE 21 COLUMN 10
                       ELSE
                           DISPLAY "CONFIRMA A ALTERACAO? (S/N)"
                                   LINE 20 COLUMN 10
                           ACCEPT  WRK-TECLA
                                   LINE 20 COLUMN 60

                           IF WRK-TECLA NOT EQUAL "S" AND
                              WRK-TECLA NOT EQUAL "s"
                               PERFORM 0820-CANCELAR
                               EXIT SECTION
                           END-IF

                           REWRITE FILMES-REG

                           MOVE SPACES TO WRK-TECLA
                           IF FILMES-STATUS EQUAL 0
                               MOVE 'REGISTRO ALTERADO' TO WRK-MSG-ERRO
                               DISPLAY MOSTRA-ERRO
                           ELSE
                               MOVE 'REGISTRO NAO ALTERADO'
                                   TO WRK-MSG-ERRO
                               DISPLAY MOSTRA-ERRO
                           END-IF

                            DISPLAY "DESEJA ALTERAR OUTRO FILME? (S/N)"
                                     LINE 21 COLUMN 10
                       END-IF
               END-READ

               ACCEPT  WRK-TECLA               LINE 21 COLUMN 60
               INITIALIZE FILMES-REG
           END-PERFORM.

       0700-EXCLUIR               SECTION.
           MOVE 'EXCLUIR' TO WRK-TITULO.
           PERFORM UNTIL WRK-TECLA EQUAL "N" OR WRK-TECLA EQUAL "n"
               MOVE SPACES TO WRK-TECLA
               DISPLAY TELA
               DISPLAY AVISO-SAIR
               DISPLAY TELA-REGISTRO
               ACCEPT CHAVE
               READ FILMES
                   INVALID KEY
                       MOVE 'FILME NAO ENCONTRADO' TO WRK-MSG-ERRO
                       DISPLAY MOSTRA-ERRO
                       DISPLAY "DESEJA TENTAR OUTRO FILME? (S/N)"
                                LINE 21 COLUMN 10
                   NOT INVALID KEY
                       MOVE 'FILME ENCONTRADO' TO WRK-MSG-ERRO
                       DISPLAY MOSTRA-ERRO
                       DISPLAY SS-DADOS

                       DISPLAY "DESEJA EXCLUIR? (S/N)" LINE 20 COLUMN 10
                       ACCEPT  WRK-TECLA               LINE 20 COLUMN 60

                       IF WRK-TECLA NOT EQUAL "S" AND
                          WRK-TECLA NOT EQUAL "s"
                           PERFORM 0820-CANCELAR
                           EXIT SECTION
                       END-IF

                       DELETE FILMES
                           INVALID KEY
                               MOVE 'NAO EXCLUIDO' TO WRK-MSG-ERRO
                               ACCEPT MOSTRA-ERRO
                           NOT INVALID KEY
                               MOVE "FILME EXCLUIDO" TO WRK-MSG-ERRO
                               DISPLAY MOSTRA-ERRO
                       END-DELETE

                       DISPLAY "DESEJA EXCLUIR OUTRO FILME? (S/N)"
                                LINE 21 COLUMN 10
               END-READ

               ACCEPT  WRK-TECLA               LINE 21 COLUMN 60
               INITIALIZE FILMES-REG
           END-PERFORM.

       0800-RELATORIO-TELA             SECTION.
           OPEN INPUT RELATORIO.
           MOVE 1 TO WRK-PAGINA.
           MOVE 0 TO WRK-LIDOS.
           PERFORM 0810-CABEC.

           READ RELATORIO
           PERFORM UNTIL FILMES2-STATUS EQUAL 10
               ADD 1 TO WRK-LIDOS

               DISPLAY CODFILME2       LINE WRK-LINHA COLUMN 01
               DISPLAY TITULO2         LINE WRK-LINHA COLUMN 12
               DISPLAY GENERO2         LINE WRK-LINHA COLUMN 44
               DISPLAY DURACAO2        LINE WRK-LINHA COLUMN 54
               DISPLAY DISTRIBUIDORA2  LINE WRK-LINHA COLUMN 64
               DISPLAY NOTA2           LINE WRK-LINHA COLUMN 80

               READ RELATORIO

               ADD 1 TO WRK-LINHA

               IF FILMES2-STATUS NOT EQUAL 10 AND WRK-LINHA > 6
                   ADD 1 TO WRK-PAGINA
                   DISPLAY "PRESSIONE ENTER PARA CONTINUAR"
                       LINE 20 COLUMN 10
                   ACCEPT  WRK-TECLA
                       LINE 20 COLUMN 60
                   PERFORM 0810-CABEC
               END-IF

               IF FILMES2-STATUS EQUAL 10
                   MOVE WRK-LIDOS TO WRK-LIDOS-ED

                   DISPLAY "REGISTROS LIDOS: " LINE 19 COLUMN 10
                   DISPLAY WRK-LIDOS-ED        LINE 19 COLUMN 27
                   DISPLAY "PRESSIONE ENTER PARA SAIR "
                       LINE 20 COLUMN 10
                   ACCEPT  WRK-TECLA
                       LINE 20 COLUMN 45
               END-IF

           END-PERFORM.

           CLOSE RELATORIO.

       0810-CABEC                      SECTION.
                DISPLAY LIMPA-TELA-GERAL.
                MOVE 1 TO WRK-LINHA.
                DISPLAY 'RELATORIO DE FILMES' LINE WRK-LINHA COLUMN 01.
                DISPLAY 'PAGINA: '            LINE WRK-LINHA COLUMN 80.
                MOVE WRK-PAGINA TO WRK-PAGINA-ED.
                DISPLAY WRK-PAGINA-ED         LINE WRK-LINHA COLUMN 88.
                ADD 2 TO WRK-LINHA.

                DISPLAY
                   'CODFILME | TITULO                        | GENERO  |
      -            ' DURACAO | DISTRIBUIDORA | NOTA |'
                   LINE WRK-LINHA COLUMN 01.
                ADD 1 TO WRK-LINHA.


       0820-CANCELAR                   SECTION.
           MOVE SPACES TO WRK-TECLA.
           MOVE 'OPERACAO CANCELADA! ENTER PARA CONTINUAR'
               TO WRK-MSG-ERRO.
           ACCEPT MOSTRA-ERRO.

       0830-VALIDAR-NOTA               SECTION.
           IF NOTA GREATER THAN 10 OR NOTA LESS THAN 0
               MOVE 'NOTA INVALIDA, DEVE ESTAR ENTRE 0 E 10'
                   TO WRK-MSG-ERRO
               DISPLAY MSG-ERRO
               MOVE 1 TO WRK-NOTA-ERRO
           END-IF.
