       IDENTIFICATION                  DIVISION.
       PROGRAM-ID. FILMES.
      *===========================================
      *== AUTOR: YAGO             EMPRESA: XPTO
      *== OBJETIVO: GESTOR DE FILMES
      *== DATA: 06/04/2025
      *== OBSERVACOES:
      *===========================================

       ENVIRONMENT                     DIVISION.  
       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN TO "..\CLIENTES.DAT"
               FILE STATUS IS FS-CLIENTES.

       DATA                            DIVISION.

       FILE                            SECTION.
       FD CLIENTES.
       01 REG-CLIENTES.
           05 REG-ID       PIC 9(4).  
           05 REG-NOME     PIC X(20).
           05 REG-TELEFONE PIC X(11).

       WORKING-STORAGE                 SECTION.
       01 WRK-OPCAO        PIC X(1).  
       01 WRK-TITULO       PIC X(20).
       01 WRK-MSG-ERRO     PIC X(30).
       01 WRK-TECLA        PIC X(1).  

       SCREEN                          SECTION.
       01 TELA.
           05 LIMPA-TELA.
               10 BLANK SCREEN.
               10 LINE 01 COLUMN 01 PIC X(20) ERASE EOL
                   BACKGROUND-COLOR 3.
               10 LINE 01 COLUMN 50 PIC X(20)
                   BACKGROUND-COLOR 3 FROM 'GESTOR DE FILMES'.
               10 LINE 02 COLUMN 01 PIC X(20) ERASE EOL
                   BACKGROUND-COLOR 4 FROM WRK-TITULO
                   FOREGROUND-COLOR 9.

       01 MENU.
           05 LINE 07 COLUMN 15 VALUE '1 - INCLUIR'.
           05 LINE 08 COLUMN 15 VALUE '2 - CONSULTAR'.
           05 LINE 09 COLUMN 15 VALUE '3 - ALTERAR'.
           05 LINE 10 COLUMN 15 VALUE '4 - EXCLUIR'.
           05 LINE 11 COLUMN 15 VALUE '5 - RELATORIO'.
           05 LINE 12 COLUMN 15 VALUE 'X - ENCERRAR'.
           05 LINE 13 COLUMN 15 VALUE 'OPCAO... '.
           05 LINE 13 COLUMN 24 PIC X(1) FROM WRK-OPCAO.

       01 TELA-REGISTRO.
           05 CHAVE FOREGROUND-COLOR 2.
               10 LINE 10 COLUMN 10 VALUE 'CODIGO FILME..'.
               10 COLUMN PLUS 2 PIC 9(5) USING CODFILME
                   BLANK WHEN ZEROS.
           05 SS-DADOS.
               10 LINE 11 COLUMN 10 VALUE 'TITULO..'.
               10 COLUMN PLUS 2 PIC X(30) USING TITULO.
               10 LINE 12 COLUMN 10 VALUE 'GENERO..'.
               10 COLUMN PLUS 2 PIC X(8) USING GENERO.
               10 LINE 13 COLUMN 10 VALUE 'DURACAO..'.
               10 COLUMN PLUS 2 PIC 9(3) USING DURACAO
                   BLANK WHEN ZEROS.
               10 LINE 14 COLUMN 10 VALUE 'DISTRIBUIDORA..'.
               10 COLUMN PLUS 2 PIC X(15) USING DISTRIBUIDORA.
               10 LINE 15 COLUMN 10 VALUE 'NOTA..'.
               10 COLUMN PLUS 2 PIC 9(2) USING NOTA
                   BLANK WHEN ZEROS.

       01 MOSTRA-ERRO.
           05 MSG-ERRO.
               10 LINE 16 COLUMN 01 PIC X(20) ERASE EOL
                   BACKGROUND-COLOR 3.
               10 LINE 16 COLUMN 10 PIC X(30)
                   BACKGROUND-COLOR 3 FROM WRK-MSG-ERRO.
               10 COLUMN PLUS 2 PIC X(1)
                   BACKGROUND-COLOR 3 USING WRK-TECLA.

       PROCEDURE                       DIVISION.
       0100-PRINCIPAL                  SECTION.

           PERFORM 0100-INICIALIZAR.
           PERFORM 0200-PROCESSAR UNTIL WRK-OPCAO EQUAL 'X'.
           PERFORM 0300-FINALIZAR.
           STOP RUN.

       0100-INICIALIZAR                SECTION.
           OPEN I-O CLIENTES
           IF FS-CLIENTES EQUAL 35 THEN
               OPEN OUTPUT CLIENTES
               CLOSE CLIENTES
               OPEN I-O CLIENTES
           END-IF.

           MOVE SPACES TO WRK-OPCAO.
           MOVE 'MENU' TO WRK-TITULO.
           DISPLAY TELA.
           ACCEPT MENU.

       0200-PROCESSAR                  SECTION.
           MOVE SPACES TO REG-CLIENTES WRK-TECLA WRK-MSG-ERRO.
           EVALUATE WRK-OPCAO
               WHEN '1'
                   PERFORM 0400-INCLUIR
               WHEN '2'
                   PERFORM 0500-CONSULTA
               WHEN '3'
                   PERFORM 0600-ALTERAR
               WHEN '4'
                   PERFORM 0700-EXCLUIR
               WHEN '5'
                   PERFORM 0800-RELATORIO-TELA
               WHEN OTHER
                   IF WRK-OPCAO NOT EQUAL 'X'
                       DISPLAY 'OPÃ‡ÃƒO INVALIDA!' AT 1324
                   END-IF
           END-EVALUATE.
           PERFORM 0100-INICIALIZAR.

       0300-FINALIZAR                  SECTION.
           CLOSE CLIENTES.  

       0400-INCLUIR SECTION.
           MOVE 'INCLUIR' TO WRK-TITULO.
           DISPLAY TELA.
           DISPLAY 'INSIRA OS DADOS DO FILME:' LINE 03 COLUMN 15.
           DISPLAY 'CODIGO FILME (5 dígitos):' LINE 05 COLUMN 15.
           ACCEPT CODFILME.
           DISPLAY 'TITULO (até 30 caracteres):' LINE 07 COLUMN 15.
           ACCEPT TITULO.
           DISPLAY 'GENERO (até 8 caracteres):' LINE 09 COLUMN 15.
           ACCEPT GENERO.
           DISPLAY 'DURACAO (em minutos, até 3 dígitos):' LINE 11 COLUMN 15.
           ACCEPT DURACAO.
           DISPLAY 'DISTRIBUIDORA (até 15 caracteres):' LINE 13 COLUMN 15.
           ACCEPT DISTRIBUIDORA.
           DISPLAY 'NOTA (0 a 99):' LINE 15 COLUMN 15.
           ACCEPT NOTA.

           DISPLAY 'DESEJA GRAVAR ? (S/N)' LINE 20 COLUMN 15.
           ACCEPT WRK-TECLA LINE 20 COLUMN 45.
               IF WRK-TECLA EQUAL 'S' OR WRK-TECLA EQUAL 's'
                   WRITE REG-CLIENTES  
                       INVALID KEY
                           MOVE 'FILME JA EXISTE' TO WRK-MSG-ERRO
                           ACCEPT MOSTRA-ERRO
                   END-WRITE
               ELSE
                   DISPLAY 'GRAVAÇÃO CANCELADA'
                   PERFORM 0100-INICIALIZAR
               END-IF.

       0500-CONSULTA                   SECTION.
           MOVE 'CONSULTA' TO WRK-TITULO.
           DISPLAY TELA.
           DISPLAY 'INSIRA O CODIGO DO FILME PARA CONSULTA (5 dígitos):' LINE 03 COLUMN 15.
           ACCEPT CHAVE.
           READ CLIENTES
               INVALID KEY
                   DISPLAY 'CLIENTE NÃO ENCONTRADO'
                   PERFORM 0100-INICIALIZAR
               END-READ.
           DISPLAY 'DESEJA CONSULTAR OUTRO? (S/N)' LINE 20 COLUMN 15.
           ACCEPT WRK-TECLA LINE 20 COLUMN 45.
           IF WRK-TECLA EQUAL 'S' OR WRK-TECLA EQUAL 's'
               PERFORM 0500-CONSULTA
           ELSE
               PERFORM 0100-INICIALIZAR
           END-IF.

       0600-ALTERAR                    SECTION.
           MOVE 'ALTERAR' TO WRK-TITULO.
           DISPLAY TELA.
           DISPLAY 'INSIRA O CODIGO DO FILME PARA ALTERAÇÃO (5 dígitos):' LINE 03 COLUMN 15.
           ACCEPT CHAVE.
           READ CLIENTES
               INVALID KEY
                   DISPLAY 'CLIENTE NÃO ENCONTRADO'
                   PERFORM 0100-INICIALIZAR
               END-READ.
           DISPLAY 'DESEJA ALTERAR ? (S/N)' LINE 20 COLUMN 15.
           ACCEPT WRK-TECLA LINE 20 COLUMN 45.
           IF WRK-TECLA EQUAL 'S' OR WRK-TECLA EQUAL 's'
               DISPLAY 'ALTERANDO CLIENTE...'
               PERFORM 0100-INICIALIZAR
           ELSE
               DISPLAY 'ALTERAÇÃO CANCELADA'
               PERFORM 0100-INICIALIZAR
           END-IF.
       
       0700-EXCLUIR                    SECTION.
           MOVE 'EXCLUIR' TO WRK-TITULO.
           DISPLAY TELA.
           DISPLAY 'INSIRA O CODIGO DO FILME PARA EXCLUSÃO (5 dígitos):'
            LINE 03 COLUMN 15.
           ACCEPT CHAVE.
           READ CLIENTES
               INVALID KEY
                   DISPLAY 'CLIENTE NÃO ENCONTRADO'
                   PERFORM 0100-INICIALIZAR
               END-READ.
           DISPLAY 'DESEJA EXCLUIR ? (S/N)' LINE 20 COLUMN 15.
           ACCEPT WRK-TECLA LINE 20 COLUMN 45.
           IF WRK-TECLA EQUAL 'S' OR WRK-TECLA EQUAL 's'
               DELETE CLIENTES
                   INVALID KEY
                       DISPLAY 'ERRO AO EXCLUIR'
                       PERFORM 0100-INICIALIZAR
                   END-DELETE
               ELSE
                   DISPLAY 'EXCLUSÃO CANCELADA'
                   PERFORM 0100-INICIALIZAR
           END-IF.

       0800-RELATORIO-TELA             SECTION.
           MOVE 'RELATORIO' TO WRK-TITULO.
           DISPLAY TELA.
           DISPLAY 'GERANDO RELATÓRIO...' LINE 03 COLUMN 15.
           PERFORM UNTIL END-OF-FILE
               READ CLIENTES
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       DISPLAY REG-CLIENTES
                       IF LINE-NUMBER > MAX-LINES
                           DISPLAY 'PRESSIONE ENTER PARA CONTINUAR' LINE 20 COLUMN 15
                           ACCEPT WRK-TECLA
                           MOVE 1 TO LINE-NUMBER
                       END-IF
               END-READ
           END-PERFORM.
