      ******************************************************************
      *== AUTOR: THIAGO
      *== OBJETO: SISTEMA DE CADASTRO DE FILMES
      *== DATA: 29/03/2025
      *== OBSERVACOES:
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILMES.
       ENVIRONMENT                                             DIVISION.
       INPUT-OUTPUT                                            SECTION.
       FILE-CONTROL.
           SELECT FILMES ASSIGN TO '../FILMES.DAT'
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS FILMES-STATUS
           RECORD KEY   IS FILMES-CODFILME.

       DATA                                                    DIVISION.
       FILE                                                    SECTION.
       FD FILMES.
       01 FILMES-REG.
           05 FILMES-CHAVE.
               10 FILMES-CODFILME      PIC 9(005).
           05 FILMES-TITULO            PIC X(030).
           05 FILMES-GENERO            PIC X(008).
           05 FILMES-DURACAO           PIC 9(003).
           05 FILMES-DISTRIBUIDORA     PIC X(015).
           05 FILMES-NOTA              PIC 9(002).

       WORKING-STORAGE                                         SECTION.
       77 FILMES-STATUS                PIC 9(002).
       77 WRK-OPCAO                    PIC X(001).
       77 WRK-TECLA                    PIC X(001).
       77 WRK-MSGERRO                  PIC X(040).
       77 WRK-CONTALINHA               PIC 9(003) VALUE 0.
       77 WRK-QTREGISTROS              PIC 9(005) VALUE 0.
       77 WRK-LINHA                    PIC 9(002) VALUE 4.
       77 WRK-MODULO                   PIC X(020).

       SCREEN                                                  SECTION.
       01 TELA.
           05 LIMPA-TELA.
               10 BLANK SCREEN.
               10 LINE 01 COLUMN 01    PIC X(020) ERASE EOL
                   BACKGROUND-COLOR 0.
               10 LINE 01 COLUMN 25    PIC X(020)
                   BACKGROUND-COLOR 0 FROM 'CATALOGO DE FILMES '.
               10 LINE 02 COLUMN 01    PIC X(030) ERASE EOL
                   BACKGROUND-COLOR 1 FROM WRK-MODULO.

       01 MENU.
           05 LINE 07 COLUMN 15 VALUE '1- INCLUIR'.
           05 LINE 08 COLUMN 15 VALUE '2- CONSULTAR'.
           05 LINE 09 COLUMN 15 VALUE '3- ALTERAR'.
           05 LINE 10 COLUMN 15 VALUE '4- EXCLUIR'.
           05 LINE 11 COLUMN 15 VALUE '5- RELATORIO'.
           05 LINE 12 COLUMN 15 VALUE 'X- SAIDA'.
           05 LINE 14 COLUMN 15 VALUE 'OPCAO...... '.
           05 LINE 14 COLUMN 28 USING WRK-OPCAO.

       01 TELA-REGISTRO.
           05 CHAVE FOREGROUND-COLOR 2.
               10 LINE 10 COLUMN 10 VALUE 'CODFILME '.
               10 COLUMN PLUS 2        PIC 9(005) USING FILMES-CODFILME
                   BLANK WHEN ZEROS.

       05 SS-DADOS.
           10 LINE 11 COLUMN 10 VALUE 'TITULO '.
           10 COLUMN PLUS 2            PIC X(030) USING FILMES-TITULO.
           10 LINE 12 COLUMN 10 VALUE 'GENERO '.
           10 COLUMN PLUS 2            PIC X(020) USING FILMES-GENERO.
           10 LINE 13 COLUMN 10 VALUE 'DURACAO '.
           10 COLUMN PLUS 2            PIC 9(003) USING FILMES-DURACAO.
           10 LINE 14 COLUMN 10 VALUE 'DISTRIBUIDORA '.
           10 COLUMN PLUS 2            PIC X(025) USING
               FILMES-DISTRIBUIDORA.
           10 LINE 15 COLUMN 10 VALUE 'NOTA '.
           10 COLUMN PLUS 2            PIC 9(001) USING FILMES-NOTA.

       01 MOSTRA-ERRO.
           02 MSG-ERRO.
               10 LINE 17 COLUMN 01    PIC X(020) ERASE EOL
                   BACKGROUND-COLOR 3.
               10 LINE 17 COLUMN 10    PIC X(030)
                   BACKGROUND-COLOR 3
                   FROM WRK-MSGERRO.
               10 COLUMN PLUS 2        PIC X(001)
                   BACKGROUND-COLOR 3
                   USING WRK-TECLA.

       PROCEDURE                                               DIVISION.
       0001-PRINCIPAL                                          SECTION.
           PERFORM 1000-INICIAR THRU 1100-MONTATELA
           PERFORM 2000-PROCESSAR UNTIL WRK-OPCAO = 'X'.
           PERFORM 3000-FINALIZAR.
               DISPLAY TELA.
           STOP RUN.

           1000-INICIAR.
               OPEN I-O FILMES
                   IF FILMES-STATUS = 35 THEN
                       OPEN OUTPUT FILMES
                       CLOSE FILMES
                       OPEN I-O FILMES
                   END-IF.

           1100-MONTATELA.
               MOVE 0 TO WRK-QTREGISTROS.
               DISPLAY TELA.
               ACCEPT MENU.

           2000-PROCESSAR.
               MOVE SPACES TO FILMES-TITULO FILMES-GENERO
               FILMES-DISTRIBUIDORA.
               MOVE ZEROS TO FILMES-CODFILME FILMES-DURACAO
               FILMES-NOTA.
               EVALUATE WRK-OPCAO
                   WHEN 1
                       PERFORM 5000-INCLUIR UNTIL WRK-TECLA = 'N'
                       MOVE ' ' TO WRK-TECLA
                   WHEN 2
                       PERFORM 6000-CONSULTAR UNTIL WRK-TECLA = 'N'
                       MOVE ' ' TO WRK-TECLA
                   WHEN 3
                       PERFORM 7000-ALTERAR UNTIL WRK-TECLA = 'N'
                       MOVE ' ' TO WRK-TECLA
                   WHEN 4
                       PERFORM 8000-EXCLUIR UNTIL WRK-TECLA = 'N'
                       MOVE ' ' TO WRK-TECLA
                   WHEN 5
                       PERFORM 9000-RELATORIO
                   WHEN OTHER
                       CLOSE FILMES
                       IF WRK-OPCAO NOT EQUAL 'X'
                       DISPLAY 'ENTRE COM A OPCAO CORRETA'
               END-EVALUATE.
                   PERFORM 1100-MONTATELA.

           3000-FINALIZAR.
           CONTINUE.

           5000-INCLUIR.
               MOVE 'MODULO - INCLUSAO ' TO WRK-MODULO.
               DISPLAY TELA.
               ACCEPT TELA-REGISTRO.
               WRITE FILMES-REG
                 INVALID KEY
                   MOVE 'JA EXISTE ' TO WRK-MSGERRO
                   ACCEPT MOSTRA-ERRO
               END-WRITE.
               MOVE 'N' TO WRK-TECLA.
               MOVE 'DESEJA INCLUIR OUTRO REGISTRO? (S/N) '
                   TO WRK-MSGERRO.
               ACCEPT MOSTRA-ERRO.
               IF WRK-TECLA = 'S'
                   MOVE SPACES TO FILMES-TITULO FILMES-GENERO
                   FILMES-DISTRIBUIDORA.
                   MOVE ZEROS TO FILMES-CODFILME FILMES-DURACAO
                   FILMES-NOTA.
                   DISPLAY WRK-TECLA.

           6000-CONSULTAR.
               MOVE 'MODULO - CONSULTA ' TO WRK-MODULO.
               DISPLAY TELA.
               DISPLAY TELA-REGISTRO.
               ACCEPT CHAVE.
               READ FILMES
                   INVALID KEY
                     MOVE 'NAO ENCONTRADO ' TO WRK-MSGERRO
                   NOT INVALID KEY
                     MOVE '-- ENCONTRADO '  TO WRK-MSGERRO
                     DISPLAY SS-DADOS
               END-READ.
               ACCEPT MOSTRA-ERRO.
               MOVE 'N' TO WRK-TECLA.
               MOVE 'DESEJA CONSULTAR OUTRO REGISTRO? (S/N) '
                   TO WRK-MSGERRO.
               ACCEPT MOSTRA-ERRO.
               IF WRK-TECLA = 'S'
                   MOVE SPACES TO FILMES-TITULO FILMES-GENERO
                   FILMES-DISTRIBUIDORA.
                   MOVE ZEROS TO FILMES-CODFILME FILMES-DURACAO
                   FILMES-NOTA.
                   DISPLAY WRK-TECLA.

           7000-ALTERAR.
               MOVE 'MODULO - ALTERACAO ' TO WRK-MODULO.
               DISPLAY TELA
               DISPLAY TELA-REGISTRO
               ACCEPT CHAVE
               READ FILMES
               IF FILMES-STATUS = 0
                   ACCEPT SS-DADOS
                   REWRITE FILMES-REG
                   IF FILMES-STATUS = 0
                       MOVE 'REGISTRO ALTERADO ' TO WRK-MSGERRO
                   ELSE
                       MOVE 'REGISTRO NAO ALTERADO ' TO WRK-MSGERRO
                       ACCEPT MOSTRA-ERRO
                   END-IF
               ELSE
                   MOVE 'REGISTRO NAO ENCONTRADO ' TO WRK-MSGERRO
                   ACCEPT MOSTRA-ERRO
               END-IF.
               MOVE 'N' TO WRK-TECLA.
               MOVE 'DESEJA ALTERAR OUTRO REGISTRO? (S/N) '
                   TO WRK-MSGERRO.
               ACCEPT MOSTRA-ERRO.
               IF WRK-TECLA = 'S'
                   MOVE SPACES TO FILMES-TITULO FILMES-GENERO
                   FILMES-DISTRIBUIDORA.
                   MOVE ZEROS TO FILMES-CODFILME FILMES-DURACAO
                   FILMES-NOTA.
                   DISPLAY WRK-TECLA.

           8000-EXCLUIR.
               MOVE 'MODULO - EXCLUIR ' TO WRK-MODULO.
               DISPLAY TELA.
               DISPLAY TELA-REGISTRO.
               ACCEPT CHAVE.
                   READ FILMES
                       INVALID KEY
                         MOVE 'NAO ENCONTRADO ' TO WRK-MSGERRO
                       NOT INVALID KEY
                         MOVE 'EXCLUIR (S/N)' TO WRK-MSGERRO
                         DISPLAY SS-DADOS
                   END-READ
                   ACCEPT MOSTRA-ERRO.
                   IF WRK-TECLA = 'S' AND FILMES-STATUS = 0
                       DELETE FILMES
                         INVALID KEY
                           MOVE 'NAO EXCLUIDO ' TO WRK-MSGERRO
                           ACCEPT MOSTRA-ERRO
                       END-DELETE
                   END-IF.
                   MOVE 'N' TO WRK-TECLA.
               MOVE 'DESEJA EXCLUIR OUTRO REGISTRO? (S/N) '
                   TO WRK-MSGERRO.
               ACCEPT MOSTRA-ERRO.
               IF WRK-TECLA = 'S'
                   MOVE SPACES TO FILMES-TITULO FILMES-GENERO
                   FILMES-DISTRIBUIDORA.
                   MOVE ZEROS TO FILMES-CODFILME FILMES-DURACAO
                   FILMES-NOTA.
                   DISPLAY WRK-TECLA.

           9000-RELATORIO.
               MOVE 'MODULO - RELATORIO ' TO WRK-MODULO.
               DISPLAY TELA.
               MOVE 00001 TO FILMES-CODFILME.
               MOVE 0 TO WRK-CONTALINHA.
               START FILMES KEY IS EQUAL FILMES-CODFILME.
               READ FILMES
                   INVALID KEY
                       MOVE 'NENHUM REGISTRO ENCONTRADO ' TO WRK-MSGERRO
                       DISPLAY WRK-MSGERRO LINE 11
                       ACCEPT WRK-TECLA
                   NOT INVALID KEY
                       DISPLAY 'RELATORIO DE FILMES ' LINE 2 COLUMN 24
                           BACKGROUND-COLOR 3
                       DISPLAY '-------------------'  LINE 3 COLUMN 24
                       PERFORM UNTIL FILMES-STATUS = 10
                           ADD 1 TO WRK-QTREGISTROS
                           DISPLAY ' ' LINE WRK-LINHA
                                   FILMES-CODFILME      ' '
                                   FILMES-TITULO        ' '
                                   FILMES-GENERO        ' '
                                   FILMES-DURACAO       ' '
                                   FILMES-DISTRIBUIDORA ' '
                                   FILMES-NOTA          ' '
                                   ADD 1 TO WRK-LINHA
                            READ FILMES NEXT
                            ADD 1 TO WRK-CONTALINHA
                            IF WRK-CONTALINHA = 5
                                MOVE ' PRESSIONE ALGUMA TECLA'
                                   TO WRK-MSGERRO
                                ACCEPT MOSTRA-ERRO
                                MOVE 'MODULO - RELATORIO ' TO WRK-MODULO
                                DISPLAY TELA
                                DISPLAY 'RELATORIO DE FILMES '
                                   LINE 2 COLUMN 24
                                   BACKGROUND-COLOR 3
                                DISPLAY '-------------------'
                                   LINE 3 COLUMN 24
                                MOVE 0 TO WRK-CONTALINHA
                                MOVE 4 TO WRK-LINHA
                            END-IF
                       END-PERFORM
               END-READ.
                   MOVE 'REGISTROS LIDOS ' TO WRK-MSGERRO.
                   MOVE WRK-QTREGISTROS TO WRK-MSGERRO(17:05).
                   ACCEPT MOSTRA-ERRO.
