>>SOURCE FORMAT FREE
*> ================================================================
*> BANCO COBOL ELITE - SISTEMA BANCARIO PROFESIONAL v4.0
*> Compilar: cobc -x -free bcpro.cbl -o bcpro && ./bcpro
*> ================================================================
IDENTIFICATION DIVISION.
PROGRAM-ID. BANCO-ELITE.
AUTHOR. BancoCobol Dev Team.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. GNU-COBOL.
OBJECT-COMPUTER. GNU-COBOL.

DATA DIVISION.
WORKING-STORAGE SECTION.

*> ---------------------------------------------------------------
*> CONFIGURACION DEL BANCO
*> ---------------------------------------------------------------
01 CFG-NOMBRE-BANCO      PIC X(30)    VALUE "BANCO COBOL ELITE".
01 CFG-SUCURSAL          PIC X(20)    VALUE "SUCURSAL CENTRAL".
01 CFG-VERSION           PIC X(12)    VALUE "v4.0 - 2026".
01 CFG-MAX-CUENTAS       PIC 9(3)     VALUE 20.
01 CFG-MAX-MOVS          PIC 9(4)     VALUE 200.
01 CFG-MAX-PRESTAMOS     PIC 9(3)     VALUE 15.
01 CFG-MAX-TARJETAS      PIC 9(3)     VALUE 20.
01 CFG-RETIRO-MAX-DIA    PIC 9(8)V99  VALUE 3000.00.
01 CFG-TRANSF-MAX-DIA    PIC 9(8)V99  VALUE 10000.00.
01 CFG-COMISION-TRANSF   PIC 9V9(4)   VALUE 0.0020.
01 CFG-TASA-AHORRO       PIC 9V9(4)   VALUE 0.0350.
01 CFG-TASA-CORRIENTE    PIC 9V9(4)   VALUE 0.0150.
01 CFG-TASA-PLAZO        PIC 9V9(4)   VALUE 0.0800.
01 CFG-TASA-JUVENIL      PIC 9V9(4)   VALUE 0.0400.
01 CFG-TASA-EMPRESA      PIC 9V9(4)   VALUE 0.0100.
01 CFG-TASA-PRESTAMO     PIC 9V9(4)   VALUE 0.0650.

*> ---------------------------------------------------------------
*> TABLA DE CUENTAS (20 max)
*> ---------------------------------------------------------------
01 TBL-CUENTAS.
   05 CTA OCCURS 20 TIMES.
      10 CTA-NUM          PIC 9(8)      VALUE ZEROS.
      10 CTA-TITULAR      PIC X(35)     VALUE SPACES.
      10 CTA-DNI          PIC X(10)     VALUE SPACES.
      10 CTA-TEL          PIC X(12)     VALUE SPACES.
      10 CTA-EMAIL        PIC X(35)     VALUE SPACES.
      10 CTA-TIPO         PIC X(1)      VALUE SPACES.
      10 CTA-SALDO        PIC 9(10)V99  VALUE ZEROS.
      10 CTA-SALDO-MAX    PIC 9(10)V99  VALUE ZEROS.
      10 CTA-SALDO-MIN    PIC 9(10)V99  VALUE ZEROS.
      10 CTA-TOT-ING      PIC 9(12)V99  VALUE ZEROS.
      10 CTA-TOT-SAL      PIC 9(12)V99  VALUE ZEROS.
      10 CTA-PIN          PIC 9(4)      VALUE ZEROS.
      10 CTA-ACTIVA       PIC X(1)      VALUE "N".
      10 CTA-BLOQUEADA    PIC X(1)      VALUE "N".
      10 CTA-FECHA-APT    PIC X(10)     VALUE SPACES.
      10 CTA-INTENTOS     PIC 9(1)      VALUE ZEROS.
      10 CTA-NUM-MOVS     PIC 9(4)      VALUE ZEROS.

*> ---------------------------------------------------------------
*> TABLA DE MOVIMIENTOS (200 max)
*> ---------------------------------------------------------------
01 TBL-MOVIMIENTOS.
   05 MOV OCCURS 200 TIMES.
      10 MOV-ID           PIC 9(6)      VALUE ZEROS.
      10 MOV-CTA          PIC 9(8)      VALUE ZEROS.
      10 MOV-TIPO         PIC X(2)      VALUE SPACES.
      10 MOV-IMPORTE      PIC S9(10)V99 VALUE ZEROS.
      10 MOV-SALDO-TR     PIC 9(10)V99  VALUE ZEROS.
      10 MOV-DESC         PIC X(40)     VALUE SPACES.
      10 MOV-FECHA        PIC X(10)     VALUE SPACES.
      10 MOV-HORA         PIC X(8)      VALUE SPACES.
      10 MOV-REF          PIC X(12)     VALUE SPACES.
      10 MOV-ESTADO       PIC X(1)      VALUE "C".

*> ---------------------------------------------------------------
*> TABLA DE PRESTAMOS (15 max)
*> ---------------------------------------------------------------
01 TBL-PRESTAMOS.
   05 PRE OCCURS 15 TIMES.
      10 PRE-ID           PIC 9(6)      VALUE ZEROS.
      10 PRE-CTA          PIC 9(8)      VALUE ZEROS.
      10 PRE-TITULAR      PIC X(35)     VALUE SPACES.
      10 PRE-CAPITAL      PIC 9(10)V99  VALUE ZEROS.
      10 PRE-SALDO-PDT    PIC 9(10)V99  VALUE ZEROS.
      10 PRE-CUOTA        PIC 9(8)V99   VALUE ZEROS.
      10 PRE-PLAZO        PIC 9(3)      VALUE ZEROS.
      10 PRE-CUOTAS-PDT   PIC 9(3)      VALUE ZEROS.
      10 PRE-TASA         PIC 9V9(4)    VALUE ZEROS.
      10 PRE-ACTIVO       PIC X(1)      VALUE "N".
      10 PRE-FECHA        PIC X(10)     VALUE SPACES.
      10 PRE-PROX-CUOTA   PIC X(10)     VALUE SPACES.

*> ---------------------------------------------------------------
*> TABLA DE TARJETAS (20 max)
*> ---------------------------------------------------------------
01 TBL-TARJETAS.
   05 TAR OCCURS 20 TIMES.
      10 TAR-NUM          PIC X(19)     VALUE SPACES.
      10 TAR-CTA          PIC 9(8)      VALUE ZEROS.
      10 TAR-TITULAR      PIC X(35)     VALUE SPACES.
      10 TAR-TIPO         PIC X(1)      VALUE SPACES.
      10 TAR-LIMITE       PIC 9(8)V99   VALUE ZEROS.
      10 TAR-USADO        PIC 9(8)V99   VALUE ZEROS.
      10 TAR-CVV          PIC 9(3)      VALUE ZEROS.
      10 TAR-CADUCIDAD    PIC X(5)      VALUE SPACES.
      10 TAR-ACTIVA       PIC X(1)      VALUE "N".
      10 TAR-BLOQUEADA    PIC X(1)      VALUE "N".
      10 TAR-FECHA-EMI    PIC X(10)     VALUE SPACES.

*> ---------------------------------------------------------------
*> TABLA DE DIVISAS (tipos de cambio)
*> ---------------------------------------------------------------
01 TBL-DIVISAS.
   05 DIV OCCURS 6 TIMES.
      10 DIV-CODIGO       PIC X(3)      VALUE SPACES.
      10 DIV-NOMBRE       PIC X(20)     VALUE SPACES.
      10 DIV-CAMBIO       PIC 9(4)V9(4) VALUE ZEROS.
      10 DIV-SIMBOLO      PIC X(2)      VALUE SPACES.

*> ---------------------------------------------------------------
*> CONTADORES GLOBALES
*> ---------------------------------------------------------------
01 G-NUM-CUENTAS         PIC 9(3)     VALUE ZEROS.
01 G-NUM-MOVS            PIC 9(6)     VALUE ZEROS.
01 G-NUM-PRESTAMOS       PIC 9(3)     VALUE ZEROS.
01 G-NUM-TARJETAS        PIC 9(3)     VALUE ZEROS.
01 G-ULT-CTA             PIC 9(8)     VALUE 30000000.
01 G-ULT-PRE             PIC 9(6)     VALUE 800000.
01 G-ULT-TAR-SEQ         PIC 9(6)     VALUE 100000.
01 G-REF-SEQ             PIC 9(6)     VALUE 200000.

*> ---------------------------------------------------------------
*> VARIABLES DE SESION
*> ---------------------------------------------------------------
01 SES-FECHA             PIC X(10)    VALUE "2026-03-13".
01 SES-HORA              PIC X(8)     VALUE "09:00:00".
01 SES-USUARIO           PIC X(20)    VALUE "CAJERO01".
01 SES-OPERACIONES       PIC 9(4)     VALUE ZEROS.

*> ---------------------------------------------------------------
*> VARIABLES DE TRABAJO
*> ---------------------------------------------------------------
01 W-OPCION              PIC 9(1)     VALUE ZEROS.
01 W-SUBOP               PIC 9(1)     VALUE ZEROS.
01 W-SUBOP2              PIC 9(1)     VALUE ZEROS.
01 W-BUSCAR-NRO          PIC 9(8)     VALUE ZEROS.
01 W-IDX-A               PIC 9(3)     VALUE ZEROS.
01 W-IDX-B               PIC 9(3)     VALUE ZEROS.
01 W-IDX-K               PIC 9(3)     VALUE ZEROS.
01 W-ENCONTRADO          PIC X(1)     VALUE "N".
01 W-IMPORTE             PIC 9(10)V99 VALUE ZEROS.
01 W-DESC                PIC X(40)    VALUE SPACES.
01 W-RESP                PIC X(1)     VALUE SPACES.
01 W-TIPO-MOV            PIC X(2)     VALUE SPACES.
01 W-ERROR               PIC X(1)     VALUE "N".
01 W-PIN-INPUT           PIC 9(4)     VALUE ZEROS.
01 W-PIN-OK              PIC X(1)     VALUE "N".
01 W-COMISION            PIC 9(8)V99  VALUE ZEROS.
01 W-INTERES             PIC 9(10)V99 VALUE ZEROS.
01 W-TEMP                PIC 9(12)V99 VALUE ZEROS.
01 W-TEMP2               PIC 9(12)V99 VALUE ZEROS.
01 W-REF-STR             PIC X(12)    VALUE SPACES.
01 W-I                   PIC 9(3)     VALUE ZEROS.
01 W-J                   PIC 9(3)     VALUE ZEROS.
01 W-K                   PIC 9(3)     VALUE ZEROS.
01 W-CNT                 PIC 9(4)     VALUE ZEROS.
01 W-CNT2                PIC 9(4)     VALUE ZEROS.
01 W-ACUM                PIC 9(14)V99 VALUE ZEROS.
01 W-ACUM2               PIC 9(14)V99 VALUE ZEROS.
01 W-MEDIA               PIC 9(12)V99 VALUE ZEROS.
01 W-CONFIRMAR           PIC X(1)     VALUE "N".

*> Inputs para nueva cuenta
01 NC-TITULAR            PIC X(35)    VALUE SPACES.
01 NC-DNI                PIC X(10)    VALUE SPACES.
01 NC-TEL                PIC X(12)    VALUE SPACES.
01 NC-EMAIL              PIC X(35)    VALUE SPACES.
01 NC-TIPO               PIC X(1)     VALUE SPACES.
01 NC-DEPOSITO           PIC 9(10)V99 VALUE ZEROS.
01 NC-PIN                PIC 9(4)     VALUE ZEROS.

*> Inputs para prestamo
01 NP-IMPORTE            PIC 9(10)V99 VALUE ZEROS.
01 NP-PLAZO              PIC 9(3)     VALUE ZEROS.
01 NP-CUOTA              PIC 9(8)V99  VALUE ZEROS.
01 NP-TOTAL              PIC 9(12)V99 VALUE ZEROS.
01 NP-INTERESES          PIC 9(10)V99 VALUE ZEROS.

*> ---------------------------------------------------------------
*> DISPLAY
*> ---------------------------------------------------------------
01 D-SALDO               PIC ZZ,ZZZ,ZZ9.99.
01 D-IMP                 PIC ZZ,ZZZ,ZZ9.99.
01 D-IMP-S               PIC -ZZ,ZZZ,ZZ9.99.
01 D-CNT                 PIC ZZZ9.
01 D-CNT2                PIC ZZ9.
01 D-PCT                 PIC Z9.99.
01 D-TMP                 PIC ZZZ9.

01 LN-SEP1               PIC X(68)
   VALUE "====================================================================".
01 LN-SEP2               PIC X(68)
   VALUE "--------------------------------------------------------------------".
01 LN-SEP3               PIC X(68)
   VALUE "####################################################################".
01 LN-BLANK              PIC X(68)    VALUE SPACES.

PROCEDURE DIVISION.

*> ================================================================
INICIO.
    PERFORM CARGAR-DATOS-DEMO
    PERFORM PANTALLA-BIENVENIDA
    PERFORM MENU-PRINCIPAL UNTIL W-OPCION = 9
    PERFORM PANTALLA-DESPEDIDA
    STOP RUN
    .

*> ================================================================
*> PANTALLAS DE INICIO Y FIN
*> ================================================================
PANTALLA-BIENVENIDA.
    DISPLAY " "
    DISPLAY LN-SEP3
    DISPLAY "##                                                                ##"
    DISPLAY "##          BANCO COBOL ELITE  -  Sistema Bancario               ##"
    DISPLAY "##                    v4.0  |  2026                              ##"
    DISPLAY "##                                                                ##"
    DISPLAY LN-SEP3
    DISPLAY " "
    DISPLAY "  Sucursal : " CFG-SUCURSAL
    DISPLAY "  Usuario  : " SES-USUARIO
    DISPLAY "  Fecha    : " SES-FECHA "   Hora: " SES-HORA
    DISPLAY " "
    DISPLAY "  Cuentas activas : " G-NUM-CUENTAS
    DISPLAY "  Prestamos activos: " G-NUM-PRESTAMOS
    DISPLAY "  Tarjetas emitidas: " G-NUM-TARJETAS
    DISPLAY " "
    DISPLAY "  Presione Enter para continuar..."
    ACCEPT W-DESC
    .

PANTALLA-DESPEDIDA.
    DISPLAY " "
    DISPLAY LN-SEP3
    DISPLAY "##                                                                ##"
    DISPLAY "##           BANCO COBOL ELITE - SESION CERRADA                  ##"
    DISPLAY "##       Gracias por utilizar nuestros servicios.                ##"
    DISPLAY "##                                                                ##"
    DISPLAY LN-SEP3
    MOVE SES-OPERACIONES TO D-CNT
    DISPLAY "  Operaciones realizadas en esta sesion: " D-CNT
    DISPLAY " "
    .

*> ================================================================
*> MENU PRINCIPAL
*> ================================================================
MENU-PRINCIPAL.
    DISPLAY " "
    DISPLAY LN-SEP1
    DISPLAY "   ** " CFG-NOMBRE-BANCO " **   " CFG-VERSION
    DISPLAY LN-SEP1
    MOVE G-NUM-CUENTAS TO D-CNT
    DISPLAY "  [" SES-FECHA " " SES-HORA "]  Cuentas: " D-CNT
            "  Usuario: " SES-USUARIO
    DISPLAY LN-SEP2
    DISPLAY "  1. Gestion de Cuentas        5. Tarjetas de Credito/Debito"
    DISPLAY "  2. Operaciones Bancarias      6. Divisas y Cambio de Moneda"
    DISPLAY "  3. Consultas y Extractos      7. Buscar Cliente"
    DISPLAY "  4. Prestamos y Creditos       8. Informes y Estadisticas"
    DISPLAY "                                9. Salir del Sistema"
    DISPLAY LN-SEP2
    DISPLAY "  Opcion: " WITH NO ADVANCING
    ACCEPT W-OPCION
    EVALUATE W-OPCION
        WHEN 1 PERFORM MENU-CUENTAS
        WHEN 2 PERFORM MENU-OPERACIONES
        WHEN 3 PERFORM MENU-CONSULTAS
        WHEN 4 PERFORM MENU-PRESTAMOS
        WHEN 5 PERFORM MENU-TARJETAS
        WHEN 6 PERFORM MENU-DIVISAS
        WHEN 7 PERFORM BUSCAR-CLIENTE
        WHEN 8 PERFORM MENU-INFORMES
        WHEN 9 CONTINUE
        WHEN OTHER
            DISPLAY "  *** Opcion no valida. Elija 1-8 o 9 ***"
    END-EVALUATE
    .

*> ================================================================
*> 1. GESTION DE CUENTAS
*> ================================================================
MENU-CUENTAS.
    DISPLAY " "
    DISPLAY LN-SEP2
    DISPLAY "  *** GESTION DE CUENTAS ***"
    DISPLAY LN-SEP2
    DISPLAY "  1. Abrir nueva cuenta         5. Cambiar PIN"
    DISPLAY "  2. Ficha completa             6. Bloquear/Desbloquear"
    DISPLAY "  3. Modificar datos contacto   7. Cancelar cuenta"
    DISPLAY "  4. Listar todas las cuentas   0. Volver"
    DISPLAY LN-SEP2
    DISPLAY "  Opcion: " WITH NO ADVANCING
    ACCEPT W-SUBOP
    EVALUATE W-SUBOP
        WHEN 1 PERFORM ABRIR-CUENTA
        WHEN 2 PERFORM FICHA-COMPLETA
        WHEN 3 PERFORM MOD-CONTACTO
        WHEN 4 PERFORM LISTAR-CUENTAS
        WHEN 5 PERFORM CAMBIAR-PIN
        WHEN 6 PERFORM BLOQUEAR-DESBLOQUEAR
        WHEN 7 PERFORM CANCELAR-CUENTA
        WHEN 0 CONTINUE
        WHEN OTHER DISPLAY "  Opcion invalida"
    END-EVALUATE
    .

ABRIR-CUENTA.
    MOVE "N" TO W-ERROR
    IF G-NUM-CUENTAS >= CFG-MAX-CUENTAS
        DISPLAY "  ERROR: Capacidad maxima de cuentas alcanzada"
        MOVE "S" TO W-ERROR
    END-IF
    IF W-ERROR = "N"
        DISPLAY " "
        DISPLAY "  === APERTURA DE NUEVA CUENTA ==="
        DISPLAY "  Tipos disponibles:"
        DISPLAY "    A = Ahorro        (interes 3.50%)"
        DISPLAY "    C = Corriente     (interes 1.50%)"
        DISPLAY "    P = Plazo Fijo    (interes 8.00%)"
        DISPLAY "    J = Juvenil       (interes 4.00%)"
        DISPLAY "    E = Empresa       (interes 1.00%)"
        DISPLAY LN-SEP2
        DISPLAY "  Nombre completo         : " WITH NO ADVANCING
        ACCEPT NC-TITULAR
        DISPLAY "  DNI/NIF/CIF             : " WITH NO ADVANCING
        ACCEPT NC-DNI
        DISPLAY "  Telefono                : " WITH NO ADVANCING
        ACCEPT NC-TEL
        DISPLAY "  Email                   : " WITH NO ADVANCING
        ACCEPT NC-EMAIL
        DISPLAY "  Tipo de cuenta (A/C/P/J/E): " WITH NO ADVANCING
        ACCEPT NC-TIPO
        MOVE FUNCTION UPPER-CASE(NC-TIPO) TO NC-TIPO
        IF NC-TIPO NOT = "A" AND NC-TIPO NOT = "C" AND
           NC-TIPO NOT = "P" AND NC-TIPO NOT = "J" AND
           NC-TIPO NOT = "E"
            DISPLAY "  ERROR: Tipo invalido"
            MOVE "S" TO W-ERROR
        END-IF
    END-IF
    IF W-ERROR = "N"
        DISPLAY "  PIN de 4 digitos        : " WITH NO ADVANCING
        ACCEPT NC-PIN
        DISPLAY "  Deposito inicial (EUR)  : " WITH NO ADVANCING
        ACCEPT NC-DEPOSITO
        ADD 1 TO G-NUM-CUENTAS
        ADD 1 TO G-ULT-CTA
        MOVE G-NUM-CUENTAS TO W-I
        MOVE G-ULT-CTA     TO CTA-NUM(W-I)
        MOVE NC-TITULAR    TO CTA-TITULAR(W-I)
        MOVE NC-DNI        TO CTA-DNI(W-I)
        MOVE NC-TEL        TO CTA-TEL(W-I)
        MOVE NC-EMAIL      TO CTA-EMAIL(W-I)
        MOVE NC-TIPO       TO CTA-TIPO(W-I)
        MOVE NC-DEPOSITO   TO CTA-SALDO(W-I)
        MOVE NC-DEPOSITO   TO CTA-SALDO-MAX(W-I)
        MOVE NC-DEPOSITO   TO CTA-SALDO-MIN(W-I)
        MOVE NC-DEPOSITO   TO CTA-TOT-ING(W-I)
        MOVE NC-PIN        TO CTA-PIN(W-I)
        MOVE "S"           TO CTA-ACTIVA(W-I)
        MOVE "N"           TO CTA-BLOQUEADA(W-I)
        MOVE SES-FECHA     TO CTA-FECHA-APT(W-I)
        MOVE ZEROS         TO CTA-INTENTOS(W-I)
        IF NC-DEPOSITO > ZEROS
            MOVE NC-DEPOSITO  TO W-IMPORTE
            MOVE "Ingreso inicial apertura" TO W-DESC
            MOVE "DE" TO W-TIPO-MOV
            PERFORM REG-MOV
        END-IF
        DISPLAY " "
        DISPLAY "  +-------------------------------------------------+"
        DISPLAY "  |    *** CUENTA ABIERTA CORRECTAMENTE ***          |"
        DISPLAY "  +-------------------------------------------------+"
        DISPLAY "  Numero de cuenta : " G-ULT-CTA
        DISPLAY "  Titular          : " NC-TITULAR
        MOVE NC-DEPOSITO TO D-SALDO
        DISPLAY "  Saldo inicial    : " D-SALDO " EUR"
        DISPLAY "  Tipo             : " NC-TIPO
        DISPLAY "  Conserve su PIN en lugar seguro."
        ADD 1 TO SES-OPERACIONES
    END-IF
    .

FICHA-COMPLETA.
    DISPLAY " "
    DISPLAY "  Numero de cuenta: " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
    ELSE
        MOVE W-IDX-A TO W-I
        DISPLAY " "
        DISPLAY LN-SEP2
        DISPLAY "  FICHA COMPLETA DE CUENTA"
        DISPLAY LN-SEP2
        DISPLAY "  Numero cuenta  : " CTA-NUM(W-I)
        DISPLAY "  Titular        : " CTA-TITULAR(W-I)
        DISPLAY "  DNI/NIF        : " CTA-DNI(W-I)
        DISPLAY "  Telefono       : " CTA-TEL(W-I)
        DISPLAY "  Email          : " CTA-EMAIL(W-I)
        EVALUATE CTA-TIPO(W-I)
            WHEN "A" DISPLAY "  Tipo           : AHORRO (3.50% anual)"
            WHEN "C" DISPLAY "  Tipo           : CORRIENTE (1.50% anual)"
            WHEN "P" DISPLAY "  Tipo           : PLAZO FIJO (8.00% anual)"
            WHEN "J" DISPLAY "  Tipo           : JUVENIL (4.00% anual)"
            WHEN "E" DISPLAY "  Tipo           : EMPRESA (1.00% anual)"
        END-EVALUATE
        DISPLAY LN-SEP2
        MOVE CTA-SALDO(W-I)     TO D-SALDO
        DISPLAY "  Saldo actual   : " D-SALDO " EUR"
        MOVE CTA-SALDO-MAX(W-I) TO D-IMP
        DISPLAY "  Saldo maximo   : " D-IMP " EUR"
        MOVE CTA-SALDO-MIN(W-I) TO D-IMP
        DISPLAY "  Saldo minimo   : " D-IMP " EUR"
        MOVE CTA-TOT-ING(W-I)   TO D-IMP
        DISPLAY "  Total ingresos : " D-IMP " EUR"
        MOVE CTA-TOT-SAL(W-I)   TO D-IMP
        DISPLAY "  Total salidas  : " D-IMP " EUR"
        MOVE CTA-NUM-MOVS(W-I)  TO D-CNT
        DISPLAY "  N. movimientos : " D-CNT
        DISPLAY LN-SEP2
        EVALUATE CTA-ACTIVA(W-I)
            WHEN "S" DISPLAY "  Estado         : ACTIVA"
            WHEN "N" DISPLAY "  Estado         : CANCELADA"
        END-EVALUATE
        IF CTA-BLOQUEADA(W-I) = "S"
            DISPLAY "  Bloqueo        : *** BLOQUEADA ***"
        ELSE
            DISPLAY "  Bloqueo        : Sin bloqueo"
        END-IF
        DISPLAY "  Fecha apertura : " CTA-FECHA-APT(W-I)
        DISPLAY LN-SEP2
    END-IF
    .

MOD-CONTACTO.
    DISPLAY " "
    DISPLAY "  Numero de cuenta: " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
    ELSE
        MOVE W-IDX-A TO W-I
        PERFORM PEDIR-PIN
        IF W-PIN-OK = "S"
            DISPLAY "  Nuevo telefono (Enter=mantener): "
                    WITH NO ADVANCING
            ACCEPT NC-TEL
            IF NC-TEL NOT = SPACES
                MOVE NC-TEL TO CTA-TEL(W-I)
            END-IF
            DISPLAY "  Nuevo email (Enter=mantener): "
                    WITH NO ADVANCING
            ACCEPT NC-EMAIL
            IF NC-EMAIL NOT = SPACES
                MOVE NC-EMAIL TO CTA-EMAIL(W-I)
            END-IF
            DISPLAY "  *** CONTACTO ACTUALIZADO ***"
            ADD 1 TO SES-OPERACIONES
        END-IF
    END-IF
    .

LISTAR-CUENTAS.
    DISPLAY " "
    DISPLAY LN-SEP2
    DISPLAY "  LISTA DE CUENTAS ACTIVAS"
    DISPLAY LN-SEP2
    DISPLAY "  NUMERO    TITULAR                          T  SALDO EUR      ESTADO"
    DISPLAY LN-SEP2
    MOVE ZEROS TO W-CNT
    PERFORM VARYING W-I FROM 1 BY 1
        UNTIL W-I > G-NUM-CUENTAS
        IF CTA-ACTIVA(W-I) = "S"
            ADD 1 TO W-CNT
            MOVE CTA-SALDO(W-I) TO D-SALDO
            MOVE SPACES TO W-DESC
            IF CTA-BLOQUEADA(W-I) = "S"
                MOVE "BLOQ" TO W-DESC(1:4)
            ELSE
                MOVE "OK  " TO W-DESC(1:4)
            END-IF
            DISPLAY "  " CTA-NUM(W-I)
                    "  " CTA-TITULAR(W-I)(1:30)
                    "  " CTA-TIPO(W-I)
                    "  " D-SALDO
                    "  " W-DESC(1:4)
        END-IF
    END-PERFORM
    DISPLAY LN-SEP2
    MOVE W-CNT TO D-CNT
    DISPLAY "  Total cuentas activas: " D-CNT
    .

CAMBIAR-PIN.
    DISPLAY " "
    DISPLAY "  Numero de cuenta: " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
    ELSE
        MOVE W-IDX-A TO W-I
        PERFORM PEDIR-PIN
        IF W-PIN-OK = "S"
            DISPLAY "  Nuevo PIN (4 digitos): " WITH NO ADVANCING
            ACCEPT NC-PIN
            MOVE NC-PIN TO CTA-PIN(W-I)
            MOVE ZEROS TO CTA-INTENTOS(W-I)
            DISPLAY "  *** PIN CAMBIADO CORRECTAMENTE ***"
            ADD 1 TO SES-OPERACIONES
        END-IF
    END-IF
    .

BLOQUEAR-DESBLOQUEAR.
    DISPLAY " "
    DISPLAY "  Numero de cuenta: " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
    ELSE
        MOVE W-IDX-A TO W-I
        IF CTA-BLOQUEADA(W-I) = "S"
            DISPLAY "  La cuenta esta BLOQUEADA."
            DISPLAY "  Desbloquear? (S/N): " WITH NO ADVANCING
            ACCEPT W-RESP
            MOVE FUNCTION UPPER-CASE(W-RESP) TO W-RESP
            IF W-RESP = "S"
                MOVE "N" TO CTA-BLOQUEADA(W-I)
                MOVE ZEROS TO CTA-INTENTOS(W-I)
                DISPLAY "  *** CUENTA DESBLOQUEADA ***"
                ADD 1 TO SES-OPERACIONES
            END-IF
        ELSE
            DISPLAY "  Bloquear cuenta? (S/N): " WITH NO ADVANCING
            ACCEPT W-RESP
            MOVE FUNCTION UPPER-CASE(W-RESP) TO W-RESP
            IF W-RESP = "S"
                MOVE "S" TO CTA-BLOQUEADA(W-I)
                DISPLAY "  *** CUENTA BLOQUEADA ***"
                ADD 1 TO SES-OPERACIONES
            END-IF
        END-IF
    END-IF
    .

CANCELAR-CUENTA.
    DISPLAY " "
    DISPLAY "  Numero de cuenta: " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
    ELSE
        MOVE W-IDX-A TO W-I
        IF CTA-ACTIVA(W-I) = "N"
            DISPLAY "  ERROR: Cuenta ya cancelada"
        ELSE
            PERFORM PEDIR-PIN
            IF W-PIN-OK = "S"
                IF CTA-SALDO(W-I) > ZEROS
                    MOVE CTA-SALDO(W-I) TO D-SALDO
                    DISPLAY "  AVISO: Saldo pendiente "
                            D-SALDO " EUR (se devuelve)"
                END-IF
                DISPLAY "  ATENCION: Accion irreversible."
                DISPLAY "  Cancelar definitivamente? (S/N): "
                        WITH NO ADVANCING
                ACCEPT W-RESP
                MOVE FUNCTION UPPER-CASE(W-RESP) TO W-RESP
                IF W-RESP = "S"
                    MOVE "N"   TO CTA-ACTIVA(W-I)
                    MOVE ZEROS TO CTA-SALDO(W-I)
                    DISPLAY "  *** CUENTA " W-BUSCAR-NRO " CANCELADA ***"
                    ADD 1 TO SES-OPERACIONES
                ELSE
                    DISPLAY "  Operacion cancelada."
                END-IF
            END-IF
        END-IF
    END-IF
    .

*> ================================================================
*> 2. OPERACIONES BANCARIAS
*> ================================================================
MENU-OPERACIONES.
    DISPLAY " "
    DISPLAY LN-SEP2
    DISPLAY "  *** OPERACIONES BANCARIAS ***"
    DISPLAY LN-SEP2
    DISPLAY "  1. Ingreso / Deposito"
    DISPLAY "  2. Reintegro / Retiro"
    DISPLAY "  3. Transferencia (comision 0.20%)"
    DISPLAY "  4. Bizum (hasta 1.000 EUR, sin comision)"
    DISPLAY "  5. Pago de recibo/factura"
    DISPLAY "  6. Domiciliacion bancaria"
    DISPLAY "  0. Volver"
    DISPLAY LN-SEP2
    DISPLAY "  Opcion: " WITH NO ADVANCING
    ACCEPT W-SUBOP
    EVALUATE W-SUBOP
        WHEN 1 PERFORM OP-INGRESO
        WHEN 2 PERFORM OP-RETIRO
        WHEN 3 PERFORM OP-TRANSFERENCIA
        WHEN 4 PERFORM OP-BIZUM
        WHEN 5 PERFORM OP-PAGO-RECIBO
        WHEN 6 PERFORM OP-DOMICILIACION
        WHEN 0 CONTINUE
        WHEN OTHER DISPLAY "  Opcion invalida"
    END-EVALUATE
    .

OP-INGRESO.
    DISPLAY " "
    DISPLAY "  === INGRESO / DEPOSITO ==="
    DISPLAY "  Numero de cuenta: " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
    ELSE
        MOVE W-IDX-A TO W-I
        PERFORM VERIFICAR-OPERABLE
        IF W-ERROR = "N"
            MOVE CTA-SALDO(W-I) TO D-SALDO
            DISPLAY "  Titular  : " CTA-TITULAR(W-I)
            DISPLAY "  Saldo    : " D-SALDO " EUR"
            DISPLAY "  Importe a ingresar (EUR): " WITH NO ADVANCING
            ACCEPT W-IMPORTE
            IF W-IMPORTE <= ZEROS
                DISPLAY "  ERROR: Importe debe ser positivo"
            ELSE
                DISPLAY "  Concepto [Enter=Ingreso efectivo]: "
                        WITH NO ADVANCING
                ACCEPT W-DESC
                IF W-DESC = SPACES
                    MOVE "Ingreso en efectivo" TO W-DESC
                END-IF
                ADD W-IMPORTE TO CTA-SALDO(W-I)
                ADD W-IMPORTE TO CTA-TOT-ING(W-I)
                IF CTA-SALDO(W-I) > CTA-SALDO-MAX(W-I)
                    MOVE CTA-SALDO(W-I) TO CTA-SALDO-MAX(W-I)
                END-IF
                MOVE "DE" TO W-TIPO-MOV
                PERFORM REG-MOV
                MOVE CTA-SALDO(W-I) TO D-SALDO
                MOVE W-IMPORTE TO D-IMP
                DISPLAY " "
                DISPLAY "  +--------------------------------------+"
                DISPLAY "  |   *** INGRESO REALIZADO ***          |"
                DISPLAY "  +--------------------------------------+"
                DISPLAY "  Importe     : +" D-IMP " EUR"
                DISPLAY "  Nuevo saldo : " D-SALDO " EUR"
                DISPLAY "  Referencia  : " W-REF-STR
                ADD 1 TO SES-OPERACIONES
            END-IF
        END-IF
    END-IF
    .

OP-RETIRO.
    DISPLAY " "
    DISPLAY "  === REINTEGRO / RETIRO ==="
    DISPLAY "  Numero de cuenta: " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
    ELSE
        MOVE W-IDX-A TO W-I
        PERFORM VERIFICAR-OPERABLE
        IF W-ERROR = "N"
            PERFORM PEDIR-PIN
            IF W-PIN-OK = "S"
                MOVE CTA-SALDO(W-I) TO D-SALDO
                MOVE CFG-RETIRO-MAX-DIA TO D-IMP
                DISPLAY "  Titular    : " CTA-TITULAR(W-I)
                DISPLAY "  Saldo      : " D-SALDO " EUR"
                DISPLAY "  Limite/dia : " D-IMP " EUR"
                DISPLAY "  Importe a retirar (EUR): " WITH NO ADVANCING
                ACCEPT W-IMPORTE
                IF W-IMPORTE <= ZEROS
                    DISPLAY "  ERROR: Importe debe ser positivo"
                ELSE
                    IF W-IMPORTE > CFG-RETIRO-MAX-DIA
                        DISPLAY "  ERROR: Supera el limite diario"
                    ELSE
                        IF W-IMPORTE > CTA-SALDO(W-I)
                            DISPLAY "  ERROR: Saldo insuficiente"
                        ELSE
                            DISPLAY "  Concepto [Enter=Reintegro]: "
                                    WITH NO ADVANCING
                            ACCEPT W-DESC
                            IF W-DESC = SPACES
                                MOVE "Reintegro en efectivo" TO W-DESC
                            END-IF
                            SUBTRACT W-IMPORTE FROM CTA-SALDO(W-I)
                            ADD W-IMPORTE TO CTA-TOT-SAL(W-I)
                            IF CTA-SALDO(W-I) < CTA-SALDO-MIN(W-I)
                                MOVE CTA-SALDO(W-I)
                                    TO CTA-SALDO-MIN(W-I)
                            END-IF
                            MOVE "RE" TO W-TIPO-MOV
                            PERFORM REG-MOV
                            MOVE CTA-SALDO(W-I) TO D-SALDO
                            MOVE W-IMPORTE TO D-IMP
                            DISPLAY " "
                            DISPLAY "  +------------------------------------+"
                            DISPLAY "  |   *** RETIRO REALIZADO ***         |"
                            DISPLAY "  +------------------------------------+"
                            DISPLAY "  Importe     : -" D-IMP " EUR"
                            DISPLAY "  Nuevo saldo : " D-SALDO " EUR"
                            DISPLAY "  Referencia  : " W-REF-STR
                            ADD 1 TO SES-OPERACIONES
                        END-IF
                    END-IF
                END-IF
            END-IF
        END-IF
    END-IF
    .

OP-TRANSFERENCIA.
    DISPLAY " "
    DISPLAY "  === TRANSFERENCIA BANCARIA ==="
    DISPLAY "  Comision del 0.20% sobre el importe"
    MOVE "N" TO W-ERROR
    DISPLAY "  Cuenta ORIGEN  : " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta origen no encontrada"
        MOVE "S" TO W-ERROR
    END-IF
    IF W-ERROR = "N"
        MOVE W-IDX-A TO W-I
        PERFORM VERIFICAR-OPERABLE
    END-IF
    IF W-ERROR = "N"
        PERFORM PEDIR-PIN
        IF W-PIN-OK = "N"
            MOVE "S" TO W-ERROR
        END-IF
    END-IF
    IF W-ERROR = "N"
        DISPLAY "  Cuenta DESTINO : " WITH NO ADVANCING
        ACCEPT W-BUSCAR-NRO
        PERFORM BUSCAR-CTA
        IF W-ENCONTRADO = "N"
            DISPLAY "  ERROR: Cuenta destino no encontrada"
            MOVE "S" TO W-ERROR
        ELSE
            MOVE W-IDX-A TO W-IDX-B
            IF W-I = W-IDX-B
                DISPLAY "  ERROR: Origen y destino iguales"
                MOVE "S" TO W-ERROR
            END-IF
            IF CTA-ACTIVA(W-IDX-B) = "N"
                DISPLAY "  ERROR: Cuenta destino cancelada"
                MOVE "S" TO W-ERROR
            END-IF
            IF CTA-BLOQUEADA(W-IDX-B) = "S"
                DISPLAY "  ERROR: Cuenta destino bloqueada"
                MOVE "S" TO W-ERROR
            END-IF
        END-IF
    END-IF
    IF W-ERROR = "N"
        MOVE CTA-SALDO(W-I) TO D-SALDO
        DISPLAY "  Origen  : " CTA-TITULAR(W-I)
        DISPLAY "  Destino : " CTA-TITULAR(W-IDX-B)
        DISPLAY "  Saldo disponible: " D-SALDO " EUR"
        DISPLAY "  Importe (EUR): " WITH NO ADVANCING
        ACCEPT W-IMPORTE
        IF W-IMPORTE <= ZEROS
            DISPLAY "  ERROR: Importe debe ser positivo"
        ELSE
            MULTIPLY W-IMPORTE BY CFG-COMISION-TRANSF
                GIVING W-COMISION
            ADD W-IMPORTE W-COMISION GIVING W-TEMP
            IF W-TEMP > CTA-SALDO(W-I)
                DISPLAY "  ERROR: Saldo insuficiente (incl. comision)"
            ELSE
                MOVE W-COMISION TO D-IMP
                DISPLAY "  Comision 0.20% : " D-IMP " EUR"
                DISPLAY "  Concepto [Enter=Transferencia]: "
                        WITH NO ADVANCING
                ACCEPT W-DESC
                IF W-DESC = SPACES
                    MOVE "Transferencia bancaria" TO W-DESC
                END-IF
                SUBTRACT W-IMPORTE FROM CTA-SALDO(W-I)
                SUBTRACT W-COMISION FROM CTA-SALDO(W-I)
                ADD W-IMPORTE TO CTA-SALDO(W-IDX-B)
                ADD W-IMPORTE TO CTA-TOT-ING(W-IDX-B)
                ADD W-TEMP    TO CTA-TOT-SAL(W-I)
                MOVE "TR" TO W-TIPO-MOV
                PERFORM REG-MOV
                MOVE CTA-SALDO(W-I) TO D-SALDO
                MOVE W-IMPORTE TO D-IMP
                DISPLAY " "
                DISPLAY "  +--------------------------------------+"
                DISPLAY "  | *** TRANSFERENCIA REALIZADA ***      |"
                DISPLAY "  +--------------------------------------+"
                DISPLAY "  Importe         : " D-IMP " EUR"
                MOVE W-COMISION TO D-IMP
                DISPLAY "  Comision cobrada: " D-IMP " EUR"
                DISPLAY "  Nuevo saldo org.: " D-SALDO " EUR"
                MOVE CTA-SALDO(W-IDX-B) TO D-SALDO
                DISPLAY "  Nuevo saldo dst.: " D-SALDO " EUR"
                DISPLAY "  Referencia      : " W-REF-STR
                ADD 1 TO SES-OPERACIONES
            END-IF
        END-IF
    END-IF
    .

OP-BIZUM.
    DISPLAY " "
    DISPLAY "  === BIZUM (Pago Inmediato) ==="
    DISPLAY "  Sin comision. Limite: 1.000 EUR"
    MOVE "N" TO W-ERROR
    DISPLAY "  Su cuenta  : " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
        MOVE "S" TO W-ERROR
    END-IF
    IF W-ERROR = "N"
        MOVE W-IDX-A TO W-I
        PERFORM VERIFICAR-OPERABLE
    END-IF
    IF W-ERROR = "N"
        PERFORM PEDIR-PIN
        IF W-PIN-OK = "N"
            MOVE "S" TO W-ERROR
        END-IF
    END-IF
    IF W-ERROR = "N"
        DISPLAY "  Destinatario: " WITH NO ADVANCING
        ACCEPT W-BUSCAR-NRO
        PERFORM BUSCAR-CTA
        IF W-ENCONTRADO = "N"
            DISPLAY "  ERROR: Destinatario no encontrado"
            MOVE "S" TO W-ERROR
        ELSE
            MOVE W-IDX-A TO W-IDX-B
            IF W-I = W-IDX-B
                DISPLAY "  ERROR: No puede enviarse a si mismo"
                MOVE "S" TO W-ERROR
            END-IF
        END-IF
    END-IF
    IF W-ERROR = "N"
        DISPLAY "  Enviar a  : " CTA-TITULAR(W-IDX-B)
        DISPLAY "  Importe (1-1000 EUR): " WITH NO ADVANCING
        ACCEPT W-IMPORTE
        IF W-IMPORTE <= ZEROS OR W-IMPORTE > 1000
            DISPLAY "  ERROR: Importe invalido (1 - 1.000 EUR)"
        ELSE
            IF W-IMPORTE > CTA-SALDO(W-I)
                DISPLAY "  ERROR: Saldo insuficiente"
            ELSE
                DISPLAY "  Concepto: " WITH NO ADVANCING
                ACCEPT W-DESC
                IF W-DESC = SPACES
                    MOVE "Envio Bizum" TO W-DESC
                END-IF
                SUBTRACT W-IMPORTE FROM CTA-SALDO(W-I)
                ADD W-IMPORTE TO CTA-SALDO(W-IDX-B)
                ADD W-IMPORTE TO CTA-TOT-ING(W-IDX-B)
                ADD W-IMPORTE TO CTA-TOT-SAL(W-I)
                MOVE "BZ" TO W-TIPO-MOV
                PERFORM REG-MOV
                MOVE CTA-SALDO(W-I) TO D-SALDO
                MOVE W-IMPORTE TO D-IMP
                DISPLAY " "
                DISPLAY "  *** BIZUM ENVIADO ***"
                DISPLAY "  Importe    : " D-IMP " EUR"
                DISPLAY "  Nuevo saldo: " D-SALDO " EUR"
                ADD 1 TO SES-OPERACIONES
            END-IF
        END-IF
    END-IF
    .

OP-PAGO-RECIBO.
    DISPLAY " "
    DISPLAY "  === PAGO DE RECIBO/FACTURA ==="
    DISPLAY "  Numero de cuenta: " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
    ELSE
        MOVE W-IDX-A TO W-I
        PERFORM VERIFICAR-OPERABLE
        IF W-ERROR = "N"
            PERFORM PEDIR-PIN
            IF W-PIN-OK = "S"
                DISPLAY "  Empresa/Emisor : " WITH NO ADVANCING
                ACCEPT W-DESC
                DISPLAY "  Importe (EUR)  : " WITH NO ADVANCING
                ACCEPT W-IMPORTE
                IF W-IMPORTE <= ZEROS
                    DISPLAY "  ERROR: Importe invalido"
                ELSE
                    IF W-IMPORTE > CTA-SALDO(W-I)
                        DISPLAY "  ERROR: Saldo insuficiente"
                    ELSE
                        SUBTRACT W-IMPORTE FROM CTA-SALDO(W-I)
                        ADD W-IMPORTE TO CTA-TOT-SAL(W-I)
                        MOVE "RC" TO W-TIPO-MOV
                        PERFORM REG-MOV
                        MOVE CTA-SALDO(W-I) TO D-SALDO
                        MOVE W-IMPORTE TO D-IMP
                        DISPLAY " "
                        DISPLAY "  *** RECIBO PAGADO ***"
                        DISPLAY "  Importe    : " D-IMP " EUR"
                        DISPLAY "  Nuevo saldo: " D-SALDO " EUR"
                        DISPLAY "  Referencia : " W-REF-STR
                        ADD 1 TO SES-OPERACIONES
                    END-IF
                END-IF
            END-IF
        END-IF
    END-IF
    .

OP-DOMICILIACION.
    DISPLAY " "
    DISPLAY "  === DOMICILIACION BANCARIA ==="
    DISPLAY "  (Pago automatico periodico)"
    DISPLAY "  Numero de cuenta: " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
    ELSE
        MOVE W-IDX-A TO W-I
        PERFORM VERIFICAR-OPERABLE
        IF W-ERROR = "N"
            PERFORM PEDIR-PIN
            IF W-PIN-OK = "S"
                DISPLAY "  Empresa beneficiaria   : " WITH NO ADVANCING
                ACCEPT W-DESC
                DISPLAY "  Importe mensual (EUR)  : " WITH NO ADVANCING
                ACCEPT W-IMPORTE
                IF W-IMPORTE <= ZEROS
                    DISPLAY "  ERROR: Importe invalido"
                ELSE
                    IF W-IMPORTE > CTA-SALDO(W-I)
                        DISPLAY "  AVISO: Saldo actual insuficiente"
                        DISPLAY "  Se domiciliara igualmente."
                    END-IF
                    DISPLAY " "
                    MOVE W-IMPORTE TO D-IMP
                    DISPLAY "  *** DOMICILIACION REGISTRADA ***"
                    DISPLAY "  Empresa : " W-DESC
                    DISPLAY "  Importe : " D-IMP " EUR/mes"
                    DISPLAY "  Se cobrara el dia 1 de cada mes."
                    ADD 1 TO SES-OPERACIONES
                END-IF
            END-IF
        END-IF
    END-IF
    .

*> ================================================================
*> 3. CONSULTAS Y EXTRACTOS
*> ================================================================
MENU-CONSULTAS.
    DISPLAY " "
    DISPLAY LN-SEP2
    DISPLAY "  *** CONSULTAS Y EXTRACTOS ***"
    DISPLAY LN-SEP2
    DISPLAY "  1. Consultar saldo"
    DISPLAY "  2. Extracto completo"
    DISPLAY "  3. Ultimos movimientos"
    DISPLAY "  4. Resumen analitico"
    DISPLAY "  5. Movimientos por tipo"
    DISPLAY "  0. Volver"
    DISPLAY LN-SEP2
    DISPLAY "  Opcion: " WITH NO ADVANCING
    ACCEPT W-SUBOP
    EVALUATE W-SUBOP
        WHEN 1 PERFORM CON-SALDO
        WHEN 2 PERFORM CON-EXTRACTO
        WHEN 3 PERFORM CON-ULTIMOS
        WHEN 4 PERFORM CON-RESUMEN
        WHEN 5 PERFORM CON-POR-TIPO-MOV
        WHEN 0 CONTINUE
        WHEN OTHER DISPLAY "  Opcion invalida"
    END-EVALUATE
    .

CON-SALDO.
    DISPLAY " "
    DISPLAY "  Numero de cuenta: " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
    ELSE
        MOVE W-IDX-A TO W-I
        PERFORM PEDIR-PIN
        IF W-PIN-OK = "S"
            MOVE CTA-SALDO(W-I) TO D-SALDO
            DISPLAY " "
            DISPLAY LN-SEP2
            DISPLAY "  CONSULTA DE SALDO"
            DISPLAY LN-SEP2
            DISPLAY "  Cuenta      : " CTA-NUM(W-I)
            DISPLAY "  Titular     : " CTA-TITULAR(W-I)
            DISPLAY "  *** SALDO : " D-SALDO " EUR ***"
            DISPLAY "  Fecha/Hora  : " SES-FECHA " " SES-HORA
            DISPLAY LN-SEP2
        END-IF
    END-IF
    .

CON-EXTRACTO.
    DISPLAY " "
    DISPLAY "  Numero de cuenta: " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
    ELSE
        MOVE W-IDX-A TO W-I
        DISPLAY " "
        DISPLAY LN-SEP1
        DISPLAY "  EXTRACTO BANCARIO  -  " CFG-NOMBRE-BANCO
        DISPLAY LN-SEP1
        DISPLAY "  Cuenta   : " CTA-NUM(W-I)
        DISPLAY "  Titular  : " CTA-TITULAR(W-I)
        DISPLAY "  Apertura : " CTA-FECHA-APT(W-I)
        DISPLAY "  Emitido  : " SES-FECHA " " SES-HORA
        DISPLAY LN-SEP2
        DISPLAY "  FECHA       HORA     TP  IMPORTE          SALDO          REF"
        DISPLAY LN-SEP2
        MOVE ZEROS TO W-CNT
        PERFORM VARYING W-J FROM 1 BY 1
            UNTIL W-J > G-NUM-MOVS
            IF MOV-CTA(W-J) = CTA-NUM(W-I)
                ADD 1 TO W-CNT
                MOVE MOV-IMPORTE(W-J) TO D-IMP-S
                MOVE MOV-SALDO-TR(W-J) TO D-SALDO
                DISPLAY "  " MOV-FECHA(W-J)
                        " " MOV-HORA(W-J)
                        " " MOV-TIPO(W-J)
                        " " D-IMP-S
                        " " D-SALDO
                        " " MOV-REF(W-J)
            END-IF
        END-PERFORM
        DISPLAY LN-SEP2
        MOVE W-CNT TO D-CNT
        MOVE CTA-SALDO(W-I) TO D-SALDO
        DISPLAY "  Total movimientos: " D-CNT
        DISPLAY "  Saldo actual     : " D-SALDO " EUR"
        DISPLAY LN-SEP1
    END-IF
    .

CON-ULTIMOS.
    DISPLAY " "
    DISPLAY "  Numero de cuenta: " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
    ELSE
        MOVE W-IDX-A TO W-I
        DISPLAY " "
        DISPLAY "  === ULTIMOS 5 MOVIMIENTOS: " CTA-NUM(W-I) " ==="
        DISPLAY LN-SEP2
        MOVE ZEROS TO W-CNT
        MOVE ZEROS TO W-CNT2
        PERFORM VARYING W-J FROM 1 BY 1
            UNTIL W-J > G-NUM-MOVS
            IF MOV-CTA(W-J) = CTA-NUM(W-I)
                ADD 1 TO W-CNT
            END-IF
        END-PERFORM
        PERFORM VARYING W-J FROM 1 BY 1
            UNTIL W-J > G-NUM-MOVS
            IF MOV-CTA(W-J) = CTA-NUM(W-I)
                ADD 1 TO W-CNT2
                IF W-CNT2 > W-CNT - 5
                    MOVE MOV-IMPORTE(W-J) TO D-IMP-S
                    MOVE MOV-SALDO-TR(W-J) TO D-SALDO
                    DISPLAY "  " MOV-FECHA(W-J)
                            " " MOV-TIPO(W-J)
                            " " D-IMP-S " EUR"
                    DISPLAY "  Ref: " MOV-REF(W-J)
                            "  Nota: " MOV-DESC(W-J)
                    DISPLAY " "
                END-IF
            END-IF
        END-PERFORM
        DISPLAY LN-SEP2
    END-IF
    .

CON-RESUMEN.
    DISPLAY " "
    DISPLAY "  Numero de cuenta: " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
    ELSE
        MOVE W-IDX-A TO W-I
        DISPLAY " "
        DISPLAY LN-SEP2
        DISPLAY "  RESUMEN ANALITICO: " CTA-NUM(W-I)
        DISPLAY LN-SEP2
        MOVE CTA-SALDO(W-I)     TO D-SALDO
        DISPLAY "  Saldo actual   : " D-SALDO " EUR"
        MOVE CTA-SALDO-MAX(W-I) TO D-IMP
        DISPLAY "  Saldo maximo   : " D-IMP " EUR"
        MOVE CTA-SALDO-MIN(W-I) TO D-IMP
        DISPLAY "  Saldo minimo   : " D-IMP " EUR"
        MOVE CTA-TOT-ING(W-I)   TO D-IMP
        DISPLAY "  Total ingresos : " D-IMP " EUR"
        MOVE CTA-TOT-SAL(W-I)   TO D-IMP
        DISPLAY "  Total salidas  : " D-IMP " EUR"
        MOVE ZEROS TO W-CNT
        MOVE ZEROS TO W-ACUM
        MOVE ZEROS TO W-ACUM2
        PERFORM VARYING W-J FROM 1 BY 1
            UNTIL W-J > G-NUM-MOVS
            IF MOV-CTA(W-J) = CTA-NUM(W-I)
                ADD 1 TO W-CNT
                IF MOV-TIPO(W-J) = "DE"
                    ADD MOV-IMPORTE(W-J) TO W-ACUM
                END-IF
                IF MOV-TIPO(W-J) = "RE"
                    ADD MOV-IMPORTE(W-J) TO W-ACUM2
                END-IF
            END-IF
        END-PERFORM
        MOVE W-CNT TO D-CNT
        DISPLAY "  N. movimientos : " D-CNT
        EVALUATE CTA-TIPO(W-I)
            WHEN "A"
                MULTIPLY CTA-SALDO(W-I) BY CFG-TASA-AHORRO
                    GIVING W-TEMP
            WHEN "C"
                MULTIPLY CTA-SALDO(W-I) BY CFG-TASA-CORRIENTE
                    GIVING W-TEMP
            WHEN "P"
                MULTIPLY CTA-SALDO(W-I) BY CFG-TASA-PLAZO
                    GIVING W-TEMP
            WHEN "J"
                MULTIPLY CTA-SALDO(W-I) BY CFG-TASA-JUVENIL
                    GIVING W-TEMP
            WHEN "E"
                MULTIPLY CTA-SALDO(W-I) BY CFG-TASA-EMPRESA
                    GIVING W-TEMP
        END-EVALUATE
        MOVE W-TEMP TO D-IMP
        DISPLAY "  Interes anual  : +" D-IMP " EUR"
        DISPLAY LN-SEP2
    END-IF
    .

CON-POR-TIPO-MOV.
    DISPLAY " "
    DISPLAY "  Numero de cuenta: " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
    ELSE
        MOVE W-IDX-A TO W-I
        DISPLAY " "
        DISPLAY "  Tipos: DE=Ingreso RE=Retiro TR=Transfer BZ=Bizum"
        DISPLAY "         RC=Recibo PR=Prestamo PQ=Cuota IN=Interes"
        DISPLAY "  Tipo a filtrar (2 letras, Enter=todos): "
                WITH NO ADVANCING
        ACCEPT W-DESC
        MOVE FUNCTION UPPER-CASE(W-DESC) TO W-DESC
        DISPLAY " "
        DISPLAY "  FECHA       TP  IMPORTE          SALDO"
        DISPLAY LN-SEP2
        MOVE ZEROS TO W-CNT
        MOVE ZEROS TO W-ACUM
        PERFORM VARYING W-J FROM 1 BY 1
            UNTIL W-J > G-NUM-MOVS
            IF MOV-CTA(W-J) = CTA-NUM(W-I)
                IF W-DESC(1:2) = SPACES OR
                   MOV-TIPO(W-J) = W-DESC(1:2)
                    ADD 1 TO W-CNT
                    MOVE MOV-IMPORTE(W-J) TO D-IMP-S
                    MOVE MOV-SALDO-TR(W-J) TO D-SALDO
                    ADD MOV-IMPORTE(W-J) TO W-ACUM
                    DISPLAY "  " MOV-FECHA(W-J)
                            "  " MOV-TIPO(W-J)
                            " " D-IMP-S
                            " " D-SALDO
                END-IF
            END-IF
        END-PERFORM
        DISPLAY LN-SEP2
        MOVE W-CNT TO D-CNT
        MOVE W-ACUM TO D-IMP
        DISPLAY "  Registros encontrados: " D-CNT
        DISPLAY "  Suma importes        : " D-IMP " EUR"
    END-IF
    .

*> ================================================================
*> 4. PRESTAMOS
*> ================================================================
MENU-PRESTAMOS.
    DISPLAY " "
    DISPLAY LN-SEP2
    DISPLAY "  *** PRESTAMOS Y CREDITOS ***"
    DISPLAY LN-SEP2
    DISPLAY "  1. Simular prestamo"
    DISPLAY "  2. Solicitar prestamo"
    DISPLAY "  3. Ver mis prestamos"
    DISPLAY "  4. Pagar cuota mensual"
    DISPLAY "  5. Amortizacion anticipada"
    DISPLAY "  0. Volver"
    DISPLAY LN-SEP2
    DISPLAY "  Opcion: " WITH NO ADVANCING
    ACCEPT W-SUBOP
    EVALUATE W-SUBOP
        WHEN 1 PERFORM PRE-SIMULAR
        WHEN 2 PERFORM PRE-SOLICITAR
        WHEN 3 PERFORM PRE-VER
        WHEN 4 PERFORM PRE-PAGAR-CUOTA
        WHEN 5 PERFORM PRE-AMORTIZAR
        WHEN 0 CONTINUE
        WHEN OTHER DISPLAY "  Opcion invalida"
    END-EVALUATE
    .

PRE-SIMULAR.
    DISPLAY " "
    DISPLAY "  === SIMULADOR DE PRESTAMO ==="
    DISPLAY "  Tasa anual aplicada: 6.50%"
    DISPLAY " "
    DISPLAY "  Importe a solicitar (EUR): " WITH NO ADVANCING
    ACCEPT NP-IMPORTE
    IF NP-IMPORTE <= ZEROS
        DISPLAY "  ERROR: Importe invalido"
    ELSE
        DISPLAY "  Plazo en meses (12-120)  : " WITH NO ADVANCING
        ACCEPT NP-PLAZO
        IF NP-PLAZO < 12 OR NP-PLAZO > 120
            DISPLAY "  ERROR: Plazo entre 12 y 120 meses"
        ELSE
            MULTIPLY NP-IMPORTE BY CFG-TASA-PRESTAMO
                GIVING NP-INTERESES
            ADD NP-IMPORTE NP-INTERESES GIVING NP-TOTAL
            DIVIDE NP-PLAZO INTO NP-TOTAL
                GIVING NP-CUOTA
            DISPLAY " "
            DISPLAY LN-SEP2
            DISPLAY "  === RESULTADO DE LA SIMULACION ==="
            DISPLAY LN-SEP2
            MOVE NP-IMPORTE    TO D-IMP
            DISPLAY "  Capital solicitado  : " D-IMP " EUR"
            MOVE NP-INTERESES  TO D-IMP
            DISPLAY "  Total intereses     : " D-IMP " EUR"
            MOVE NP-TOTAL      TO D-IMP
            DISPLAY "  Total a devolver    : " D-IMP " EUR"
            MOVE NP-CUOTA      TO D-IMP
            DISPLAY "  Cuota mensual aprox.: " D-IMP " EUR"
            MOVE NP-PLAZO      TO D-CNT2
            DISPLAY "  Plazo               : " D-CNT2 " meses"
            DISPLAY "  Tasa anual          : 6.50%"
            DISPLAY LN-SEP2
            DISPLAY "  (Esta es una simulacion, no una solicitud)"
        END-IF
    END-IF
    .

PRE-SOLICITAR.
    MOVE "N" TO W-ERROR
    IF G-NUM-PRESTAMOS >= CFG-MAX-PRESTAMOS
        DISPLAY "  ERROR: Capacidad maxima de prestamos alcanzada"
        MOVE "S" TO W-ERROR
    END-IF
    IF W-ERROR = "N"
        DISPLAY " "
        DISPLAY "  === SOLICITUD DE PRESTAMO ==="
        DISPLAY "  Tasa anual: 6.50%"
        DISPLAY "  Cuenta vinculada: " WITH NO ADVANCING
        ACCEPT W-BUSCAR-NRO
        PERFORM BUSCAR-CTA
        IF W-ENCONTRADO = "N"
            DISPLAY "  ERROR: Cuenta no encontrada"
            MOVE "S" TO W-ERROR
        END-IF
    END-IF
    IF W-ERROR = "N"
        MOVE W-IDX-A TO W-I
        PERFORM VERIFICAR-OPERABLE
    END-IF
    IF W-ERROR = "N"
        PERFORM PEDIR-PIN
        IF W-PIN-OK = "N"
            MOVE "S" TO W-ERROR
        END-IF
    END-IF
    IF W-ERROR = "N"
        DISPLAY "  Importe solicitado (EUR): " WITH NO ADVANCING
        ACCEPT NP-IMPORTE
        IF NP-IMPORTE <= ZEROS
            DISPLAY "  ERROR: Importe invalido"
            MOVE "S" TO W-ERROR
        END-IF
    END-IF
    IF W-ERROR = "N"
        DISPLAY "  Plazo en meses (12-120) : " WITH NO ADVANCING
        ACCEPT NP-PLAZO
        IF NP-PLAZO < 12 OR NP-PLAZO > 120
            DISPLAY "  ERROR: Plazo invalido"
            MOVE "S" TO W-ERROR
        END-IF
    END-IF
    IF W-ERROR = "N"
        MULTIPLY NP-IMPORTE BY CFG-TASA-PRESTAMO
            GIVING NP-INTERESES
        ADD NP-IMPORTE NP-INTERESES GIVING NP-TOTAL
        DIVIDE NP-PLAZO INTO NP-TOTAL GIVING NP-CUOTA
        DISPLAY " "
        DISPLAY LN-SEP2
        DISPLAY "  CONDICIONES DEL PRESTAMO"
        DISPLAY LN-SEP2
        MOVE NP-IMPORTE   TO D-IMP
        DISPLAY "  Capital           : " D-IMP " EUR"
        MOVE NP-INTERESES TO D-IMP
        DISPLAY "  Total intereses   : " D-IMP " EUR"
        MOVE NP-TOTAL     TO D-IMP
        DISPLAY "  Total a devolver  : " D-IMP " EUR"
        MOVE NP-CUOTA     TO D-IMP
        DISPLAY "  Cuota mensual     : " D-IMP " EUR"
        MOVE NP-PLAZO     TO D-CNT2
        DISPLAY "  Plazo             : " D-CNT2 " meses"
        DISPLAY LN-SEP2
        DISPLAY "  Acepta las condiciones? (S/N): " WITH NO ADVANCING
        ACCEPT W-RESP
        MOVE FUNCTION UPPER-CASE(W-RESP) TO W-RESP
        IF W-RESP = "S"
            ADD 1 TO G-NUM-PRESTAMOS
            ADD 1 TO G-ULT-PRE
            MOVE G-NUM-PRESTAMOS TO W-K
            MOVE G-ULT-PRE          TO PRE-ID(W-K)
            MOVE CTA-NUM(W-I)       TO PRE-CTA(W-K)
            MOVE CTA-TITULAR(W-I)   TO PRE-TITULAR(W-K)
            MOVE NP-IMPORTE         TO PRE-CAPITAL(W-K)
            MOVE NP-IMPORTE         TO PRE-SALDO-PDT(W-K)
            MOVE NP-CUOTA           TO PRE-CUOTA(W-K)
            MOVE NP-PLAZO           TO PRE-PLAZO(W-K)
            MOVE NP-PLAZO           TO PRE-CUOTAS-PDT(W-K)
            MOVE CFG-TASA-PRESTAMO  TO PRE-TASA(W-K)
            MOVE "S"                TO PRE-ACTIVO(W-K)
            MOVE SES-FECHA          TO PRE-FECHA(W-K)
            ADD NP-IMPORTE TO CTA-SALDO(W-I)
            ADD NP-IMPORTE TO CTA-TOT-ING(W-I)
            MOVE NP-IMPORTE     TO W-IMPORTE
            MOVE "Ingreso prestamo concedido" TO W-DESC
            MOVE "PR" TO W-TIPO-MOV
            PERFORM REG-MOV
            DISPLAY " "
            DISPLAY "  *** PRESTAMO CONCEDIDO Y ABONADO ***"
            MOVE G-ULT-PRE TO D-TMP
            DISPLAY "  ID Prestamo: " D-TMP
            MOVE CTA-SALDO(W-I) TO D-SALDO
            DISPLAY "  Nuevo saldo cuenta: " D-SALDO " EUR"
            ADD 1 TO SES-OPERACIONES
        ELSE
            DISPLAY "  Solicitud cancelada."
        END-IF
    END-IF
    .

PRE-VER.
    DISPLAY " "
    DISPLAY LN-SEP2
    DISPLAY "  PRESTAMOS ACTIVOS"
    DISPLAY LN-SEP2
    MOVE ZEROS TO W-CNT
    PERFORM VARYING W-K FROM 1 BY 1
        UNTIL W-K > G-NUM-PRESTAMOS
        IF PRE-ACTIVO(W-K) = "S"
            ADD 1 TO W-CNT
            DISPLAY "  ID Prestamo  : " PRE-ID(W-K)
            DISPLAY "  Cuenta       : " PRE-CTA(W-K)
            DISPLAY "  Titular      : " PRE-TITULAR(W-K)
            MOVE PRE-CAPITAL(W-K) TO D-IMP
            DISPLAY "  Capital orig.: " D-IMP " EUR"
            MOVE PRE-SALDO-PDT(W-K) TO D-IMP
            DISPLAY "  Saldo pdte.  : " D-IMP " EUR"
            MOVE PRE-CUOTA(W-K) TO D-IMP
            DISPLAY "  Cuota mens.  : " D-IMP " EUR"
            MOVE PRE-CUOTAS-PDT(W-K) TO D-CNT2
            DISPLAY "  Cuotas rest. : " D-CNT2
            DISPLAY "  Concedido    : " PRE-FECHA(W-K)
            DISPLAY LN-SEP2
        END-IF
    END-PERFORM
    IF W-CNT = ZEROS
        DISPLAY "  No hay prestamos activos."
    END-IF
    .

PRE-PAGAR-CUOTA.
    DISPLAY " "
    DISPLAY "  Cuenta vinculada: " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
    ELSE
        MOVE W-IDX-A TO W-I
        PERFORM PEDIR-PIN
        IF W-PIN-OK = "S"
            MOVE "N" TO W-ENCONTRADO
            PERFORM VARYING W-K FROM 1 BY 1
                UNTIL W-K > G-NUM-PRESTAMOS
                IF PRE-CTA(W-K) = CTA-NUM(W-I) AND
                   PRE-ACTIVO(W-K) = "S"
                    MOVE "S" TO W-ENCONTRADO
                    MOVE PRE-CUOTA(W-K) TO D-IMP
                    DISPLAY " "
                    DISPLAY "  Prestamo ID  : " PRE-ID(W-K)
                    DISPLAY "  Cuota mensual: " D-IMP " EUR"
                    MOVE PRE-SALDO-PDT(W-K) TO D-IMP
                    DISPLAY "  Saldo pdte.  : " D-IMP " EUR"
                    MOVE PRE-CUOTAS-PDT(W-K) TO D-CNT2
                    DISPLAY "  Cuotas rest. : " D-CNT2
                    IF PRE-CUOTA(W-K) > CTA-SALDO(W-I)
                        DISPLAY "  ERROR: Saldo insuficiente"
                    ELSE
                        DISPLAY "  Confirmar pago? (S/N): "
                                WITH NO ADVANCING
                        ACCEPT W-RESP
                        MOVE FUNCTION UPPER-CASE(W-RESP) TO W-RESP
                        IF W-RESP = "S"
                            SUBTRACT PRE-CUOTA(W-K)
                                FROM CTA-SALDO(W-I)
                            SUBTRACT PRE-CUOTA(W-K)
                                FROM PRE-SALDO-PDT(W-K)
                            ADD PRE-CUOTA(W-K) TO CTA-TOT-SAL(W-I)
                            SUBTRACT 1 FROM PRE-CUOTAS-PDT(W-K)
                            MOVE PRE-CUOTA(W-K) TO W-IMPORTE
                            MOVE "Pago cuota prestamo" TO W-DESC
                            MOVE "PQ" TO W-TIPO-MOV
                            PERFORM REG-MOV
                            IF PRE-CUOTAS-PDT(W-K) = ZEROS
                                MOVE "N" TO PRE-ACTIVO(W-K)
                                DISPLAY "  *** PRESTAMO LIQUIDADO COMPLETAMENTE ***"
                            ELSE
                                MOVE CTA-SALDO(W-I) TO D-SALDO
                                DISPLAY "  *** CUOTA PAGADA ***"
                                DISPLAY "  Nuevo saldo  : " D-SALDO " EUR"
                                MOVE PRE-CUOTAS-PDT(W-K) TO D-CNT2
                                DISPLAY "  Cuotas rest. : " D-CNT2
                            END-IF
                            ADD 1 TO SES-OPERACIONES
                        END-IF
                    END-IF
                END-IF
            END-PERFORM
            IF W-ENCONTRADO = "N"
                DISPLAY "  No hay prestamos activos para esta cuenta."
            END-IF
        END-IF
    END-IF
    .

PRE-AMORTIZAR.
    DISPLAY " "
    DISPLAY "  Cuenta vinculada: " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
    ELSE
        MOVE W-IDX-A TO W-I
        PERFORM PEDIR-PIN
        IF W-PIN-OK = "S"
            MOVE "N" TO W-ENCONTRADO
            PERFORM VARYING W-K FROM 1 BY 1
                UNTIL W-K > G-NUM-PRESTAMOS
                IF PRE-CTA(W-K) = CTA-NUM(W-I) AND
                   PRE-ACTIVO(W-K) = "S"
                    MOVE "S" TO W-ENCONTRADO
                    MOVE PRE-SALDO-PDT(W-K) TO D-IMP
                    DISPLAY " "
                    DISPLAY "  Prestamo ID   : " PRE-ID(W-K)
                    DISPLAY "  Saldo pdte.   : " D-IMP " EUR"
                    DISPLAY "  Importe a amortizar: " WITH NO ADVANCING
                    ACCEPT W-IMPORTE
                    IF W-IMPORTE <= ZEROS
                        DISPLAY "  ERROR: Importe invalido"
                    ELSE
                        IF W-IMPORTE > CTA-SALDO(W-I)
                            DISPLAY "  ERROR: Saldo insuficiente"
                        ELSE
                            IF W-IMPORTE >= PRE-SALDO-PDT(W-K)
                                MOVE PRE-SALDO-PDT(W-K) TO W-IMPORTE
                                SUBTRACT W-IMPORTE FROM CTA-SALDO(W-I)
                                ADD W-IMPORTE TO CTA-TOT-SAL(W-I)
                                MOVE ZEROS TO PRE-SALDO-PDT(W-K)
                                MOVE ZEROS TO PRE-CUOTAS-PDT(W-K)
                                MOVE "N" TO PRE-ACTIVO(W-K)
                                MOVE "Amortizacion anticipada total" TO W-DESC
                                MOVE "AM" TO W-TIPO-MOV
                                PERFORM REG-MOV
                                DISPLAY "  *** PRESTAMO CANCELADO ANTICIPADAMENTE ***"
                            ELSE
                                SUBTRACT W-IMPORTE FROM CTA-SALDO(W-I)
                                SUBTRACT W-IMPORTE FROM PRE-SALDO-PDT(W-K)
                                ADD W-IMPORTE TO CTA-TOT-SAL(W-I)
                                MOVE "Amortizacion anticipada parcial" TO W-DESC
                                MOVE "AM" TO W-TIPO-MOV
                                PERFORM REG-MOV
                                MOVE PRE-SALDO-PDT(W-K) TO D-IMP
                                DISPLAY "  *** AMORTIZACION REGISTRADA ***"
                                DISPLAY "  Nuevo saldo prestamo: " D-IMP " EUR"
                            END-IF
                            ADD 1 TO SES-OPERACIONES
                        END-IF
                    END-IF
                END-IF
            END-PERFORM
            IF W-ENCONTRADO = "N"
                DISPLAY "  No hay prestamos activos para esta cuenta."
            END-IF
        END-IF
    END-IF
    .

*> ================================================================
*> 5. TARJETAS
*> ================================================================
MENU-TARJETAS.
    DISPLAY " "
    DISPLAY LN-SEP2
    DISPLAY "  *** TARJETAS DE CREDITO / DEBITO ***"
    DISPLAY LN-SEP2
    DISPLAY "  1. Solicitar nueva tarjeta"
    DISPLAY "  2. Ver mis tarjetas"
    DISPLAY "  3. Bloquear/Desbloquear tarjeta"
    DISPLAY "  4. Consultar limite disponible"
    DISPLAY "  0. Volver"
    DISPLAY LN-SEP2
    DISPLAY "  Opcion: " WITH NO ADVANCING
    ACCEPT W-SUBOP
    EVALUATE W-SUBOP
        WHEN 1 PERFORM TAR-SOLICITAR
        WHEN 2 PERFORM TAR-VER
        WHEN 3 PERFORM TAR-BLOQUEAR
        WHEN 4 PERFORM TAR-CONSULTAR-LIMITE
        WHEN 0 CONTINUE
        WHEN OTHER DISPLAY "  Opcion invalida"
    END-EVALUATE
    .

TAR-SOLICITAR.
    MOVE "N" TO W-ERROR
    IF G-NUM-TARJETAS >= CFG-MAX-TARJETAS
        DISPLAY "  ERROR: Capacidad maxima de tarjetas alcanzada"
        MOVE "S" TO W-ERROR
    END-IF
    IF W-ERROR = "N"
        DISPLAY " "
        DISPLAY "  === SOLICITUD DE TARJETA ==="
        DISPLAY "  D = Debito  C = Credito"
        DISPLAY "  Tipo de tarjeta (D/C): " WITH NO ADVANCING
        ACCEPT NC-TIPO
        MOVE FUNCTION UPPER-CASE(NC-TIPO) TO NC-TIPO
        IF NC-TIPO NOT = "D" AND NC-TIPO NOT = "C"
            DISPLAY "  ERROR: Tipo invalido (D o C)"
            MOVE "S" TO W-ERROR
        END-IF
    END-IF
    IF W-ERROR = "N"
        DISPLAY "  Cuenta vinculada    : " WITH NO ADVANCING
        ACCEPT W-BUSCAR-NRO
        PERFORM BUSCAR-CTA
        IF W-ENCONTRADO = "N"
            DISPLAY "  ERROR: Cuenta no encontrada"
            MOVE "S" TO W-ERROR
        END-IF
    END-IF
    IF W-ERROR = "N"
        MOVE W-IDX-A TO W-I
        PERFORM PEDIR-PIN
        IF W-PIN-OK = "N"
            MOVE "S" TO W-ERROR
        END-IF
    END-IF
    IF W-ERROR = "N"
        ADD 1 TO G-NUM-TARJETAS
        ADD 1 TO G-ULT-TAR-SEQ
        MOVE G-NUM-TARJETAS TO W-K
        MOVE NC-TIPO TO TAR-TIPO(W-K)
        MOVE CTA-NUM(W-I) TO TAR-CTA(W-K)
        MOVE CTA-TITULAR(W-I) TO TAR-TITULAR(W-K)
        MOVE "S" TO TAR-ACTIVA(W-K)
        MOVE "N" TO TAR-BLOQUEADA(W-K)
        MOVE SES-FECHA TO TAR-FECHA-EMI(W-K)
        MOVE "03/29" TO TAR-CADUCIDAD(W-K)
        MOVE 321 TO TAR-CVV(W-K)
        IF NC-TIPO = "C"
            MOVE 3000.00 TO TAR-LIMITE(W-K)
        ELSE
            MOVE 0 TO TAR-LIMITE(W-K)
        END-IF
        MOVE ZEROS TO TAR-USADO(W-K)
        MOVE "4532-1234-5678-0000" TO TAR-NUM(W-K)
        DISPLAY " "
        DISPLAY "  +------------------------------------------+"
        DISPLAY "  |   *** TARJETA EMITIDA CORRECTAMENTE ***  |"
        DISPLAY "  +------------------------------------------+"
        DISPLAY "  Numero    : " TAR-NUM(W-K)
        DISPLAY "  Titular   : " TAR-TITULAR(W-K)
        DISPLAY "  Tipo      : " TAR-TIPO(W-K)
        DISPLAY "  Caducidad : " TAR-CADUCIDAD(W-K)
        DISPLAY "  CVV       : " TAR-CVV(W-K)
        IF NC-TIPO = "C"
            MOVE TAR-LIMITE(W-K) TO D-IMP
            DISPLAY "  Limite    : " D-IMP " EUR"
        END-IF
        DISPLAY "  Conserve estos datos de forma segura."
        ADD 1 TO SES-OPERACIONES
    END-IF
    .

TAR-VER.
    DISPLAY " "
    DISPLAY LN-SEP2
    DISPLAY "  MIS TARJETAS"
    DISPLAY LN-SEP2
    DISPLAY "  Numero de cuenta: " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
    ELSE
        MOVE W-IDX-A TO W-I
        MOVE ZEROS TO W-CNT
        PERFORM VARYING W-K FROM 1 BY 1
            UNTIL W-K > G-NUM-TARJETAS
            IF TAR-CTA(W-K) = CTA-NUM(W-I) AND
               TAR-ACTIVA(W-K) = "S"
                ADD 1 TO W-CNT
                DISPLAY "  Numero    : " TAR-NUM(W-K)
                DISPLAY "  Tipo      : " TAR-TIPO(W-K)
                DISPLAY "  Caducidad : " TAR-CADUCIDAD(W-K)
                IF TAR-TIPO(W-K) = "C"
                    MOVE TAR-LIMITE(W-K) TO D-IMP
                    DISPLAY "  Limite    : " D-IMP " EUR"
                    MOVE TAR-USADO(W-K) TO D-IMP
                    DISPLAY "  Usado     : " D-IMP " EUR"
                    SUBTRACT TAR-USADO(W-K) FROM TAR-LIMITE(W-K)
                        GIVING W-TEMP
                    MOVE W-TEMP TO D-IMP
                    DISPLAY "  Disponible: " D-IMP " EUR"
                END-IF
                IF TAR-BLOQUEADA(W-K) = "S"
                    DISPLAY "  Estado    : *** BLOQUEADA ***"
                ELSE
                    DISPLAY "  Estado    : Activa"
                END-IF
                DISPLAY LN-SEP2
            END-IF
        END-PERFORM
        IF W-CNT = ZEROS
            DISPLAY "  No hay tarjetas activas para esta cuenta."
        END-IF
    END-IF
    .

TAR-BLOQUEAR.
    DISPLAY " "
    DISPLAY "  Numero de cuenta: " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
    ELSE
        MOVE W-IDX-A TO W-I
        MOVE "N" TO W-ENCONTRADO
        PERFORM VARYING W-K FROM 1 BY 1
            UNTIL W-K > G-NUM-TARJETAS
            IF TAR-CTA(W-K) = CTA-NUM(W-I) AND
               TAR-ACTIVA(W-K) = "S"
                MOVE "S" TO W-ENCONTRADO
                DISPLAY "  Tarjeta: " TAR-NUM(W-K)
                IF TAR-BLOQUEADA(W-K) = "S"
                    DISPLAY "  Estado: BLOQUEADA"
                    DISPLAY "  Desbloquear? (S/N): " WITH NO ADVANCING
                    ACCEPT W-RESP
                    MOVE FUNCTION UPPER-CASE(W-RESP) TO W-RESP
                    IF W-RESP = "S"
                        MOVE "N" TO TAR-BLOQUEADA(W-K)
                        DISPLAY "  *** TARJETA DESBLOQUEADA ***"
                        ADD 1 TO SES-OPERACIONES
                    END-IF
                ELSE
                    DISPLAY "  Estado: Activa"
                    DISPLAY "  Bloquear? (S/N): " WITH NO ADVANCING
                    ACCEPT W-RESP
                    MOVE FUNCTION UPPER-CASE(W-RESP) TO W-RESP
                    IF W-RESP = "S"
                        MOVE "S" TO TAR-BLOQUEADA(W-K)
                        DISPLAY "  *** TARJETA BLOQUEADA ***"
                        ADD 1 TO SES-OPERACIONES
                    END-IF
                END-IF
            END-IF
        END-PERFORM
        IF W-ENCONTRADO = "N"
            DISPLAY "  No hay tarjetas para esta cuenta."
        END-IF
    END-IF
    .

TAR-CONSULTAR-LIMITE.
    DISPLAY " "
    DISPLAY "  Numero de cuenta: " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
    ELSE
        MOVE W-IDX-A TO W-I
        MOVE "N" TO W-ENCONTRADO
        PERFORM VARYING W-K FROM 1 BY 1
            UNTIL W-K > G-NUM-TARJETAS
            IF TAR-CTA(W-K) = CTA-NUM(W-I) AND
               TAR-TIPO(W-K) = "C" AND
               TAR-ACTIVA(W-K) = "S"
                MOVE "S" TO W-ENCONTRADO
                MOVE TAR-LIMITE(W-K) TO D-IMP
                DISPLAY "  Tarjeta     : " TAR-NUM(W-K)
                DISPLAY "  Limite total: " D-IMP " EUR"
                MOVE TAR-USADO(W-K) TO D-IMP
                DISPLAY "  Usado       : " D-IMP " EUR"
                MOVE TAR-LIMITE(W-K) TO W-TEMP
                SUBTRACT TAR-USADO(W-K) FROM W-TEMP
                MOVE W-TEMP TO D-IMP
                DISPLAY "  Disponible  : " D-IMP " EUR"
            END-IF
        END-PERFORM
        IF W-ENCONTRADO = "N"
            DISPLAY "  No hay tarjetas de credito para esta cuenta."
        END-IF
    END-IF
    .

*> ================================================================
*> 6. DIVISAS
*> ================================================================
MENU-DIVISAS.
    DISPLAY " "
    DISPLAY LN-SEP2
    DISPLAY "  *** DIVISAS Y CAMBIO DE MONEDA ***"
    DISPLAY LN-SEP2
    DISPLAY "  1. Ver tipos de cambio"
    DISPLAY "  2. Convertir importe"
    DISPLAY "  3. Comprar divisas (cargo en cuenta)"
    DISPLAY "  0. Volver"
    DISPLAY LN-SEP2
    DISPLAY "  Opcion: " WITH NO ADVANCING
    ACCEPT W-SUBOP
    EVALUATE W-SUBOP
        WHEN 1 PERFORM DIV-VER-CAMBIOS
        WHEN 2 PERFORM DIV-CONVERTIR
        WHEN 3 PERFORM DIV-COMPRAR
        WHEN 0 CONTINUE
        WHEN OTHER DISPLAY "  Opcion invalida"
    END-EVALUATE
    .

DIV-VER-CAMBIOS.
    DISPLAY " "
    DISPLAY LN-SEP2
    DISPLAY "  TIPOS DE CAMBIO - " SES-FECHA
    DISPLAY "  (referencia: 1 EUR = X unidades de divisa)"
    DISPLAY LN-SEP2
    DISPLAY "  COD  DIVISA                CAMBIO   SIMBOLO"
    DISPLAY LN-SEP2
    PERFORM VARYING W-I FROM 1 BY 1
        UNTIL W-I > 6
        MOVE DIV-CAMBIO(W-I) TO D-IMP
        DISPLAY "  " DIV-CODIGO(W-I)
                "  " DIV-NOMBRE(W-I)
                "  " D-IMP
                "  " DIV-SIMBOLO(W-I)
    END-PERFORM
    DISPLAY LN-SEP2
    .

DIV-CONVERTIR.
    DISPLAY " "
    DISPLAY "  === CONVERSOR DE DIVISAS ==="
    DISPLAY "  Codigos: USD GBP JPY CHF MXN BRL"
    DISPLAY "  Divisa destino: " WITH NO ADVANCING
    ACCEPT W-DESC
    MOVE FUNCTION UPPER-CASE(W-DESC) TO W-DESC
    MOVE "N" TO W-ENCONTRADO
    PERFORM VARYING W-I FROM 1 BY 1
        UNTIL W-I > 6
        IF DIV-CODIGO(W-I) = W-DESC(1:3)
            MOVE "S" TO W-ENCONTRADO
            DISPLAY "  Importe en EUR a convertir: " WITH NO ADVANCING
            ACCEPT W-IMPORTE
            IF W-IMPORTE <= ZEROS
                DISPLAY "  ERROR: Importe invalido"
            ELSE
                MULTIPLY W-IMPORTE BY DIV-CAMBIO(W-I)
                    GIVING W-TEMP
                MOVE W-IMPORTE TO D-IMP
                MOVE W-TEMP TO D-SALDO
                DISPLAY " "
                DISPLAY "  " D-IMP " EUR  =  "
                        D-SALDO " " DIV-SIMBOLO(W-I)
                DISPLAY "  Tipo de cambio: 1 EUR = "
                        D-IMP " " DIV-CODIGO(W-I)
            END-IF
        END-IF
    END-PERFORM
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Divisa no encontrada"
    END-IF
    .

DIV-COMPRAR.
    DISPLAY " "
    DISPLAY "  === COMPRA DE DIVISAS ==="
    DISPLAY "  (Se cargara en su cuenta en EUR)"
    DISPLAY "  Numero de cuenta: " WITH NO ADVANCING
    ACCEPT W-BUSCAR-NRO
    PERFORM BUSCAR-CTA
    IF W-ENCONTRADO = "N"
        DISPLAY "  ERROR: Cuenta no encontrada"
    ELSE
        MOVE W-IDX-A TO W-I
        PERFORM VERIFICAR-OPERABLE
        IF W-ERROR = "N"
            PERFORM PEDIR-PIN
            IF W-PIN-OK = "S"
                DISPLAY "  Codigos: USD GBP JPY CHF MXN BRL"
                DISPLAY "  Divisa a comprar: " WITH NO ADVANCING
                ACCEPT W-DESC
                MOVE FUNCTION UPPER-CASE(W-DESC) TO W-DESC
                MOVE "N" TO W-ENCONTRADO
                PERFORM VARYING W-J FROM 1 BY 1
                    UNTIL W-J > 6
                    IF DIV-CODIGO(W-J) = W-DESC(1:3)
                        MOVE "S" TO W-ENCONTRADO
                        DISPLAY "  Importe en EUR: " WITH NO ADVANCING
                        ACCEPT W-IMPORTE
                        IF W-IMPORTE <= ZEROS
                            DISPLAY "  ERROR: Importe invalido"
                        ELSE
                            IF W-IMPORTE > CTA-SALDO(W-I)
                                DISPLAY "  ERROR: Saldo insuficiente"
                            ELSE
                                MULTIPLY W-IMPORTE BY DIV-CAMBIO(W-J)
                                    GIVING W-TEMP
                                MOVE W-TEMP TO D-SALDO
                                MOVE W-IMPORTE TO D-IMP
                                DISPLAY "  Recibe: " D-SALDO
                                        " " DIV-SIMBOLO(W-J)
                                DISPLAY "  Cargo en cuenta: "
                                        D-IMP " EUR"
                                DISPLAY "  Confirmar? (S/N): "
                                        WITH NO ADVANCING
                                ACCEPT W-RESP
                                MOVE FUNCTION UPPER-CASE(W-RESP)
                                    TO W-RESP
                                IF W-RESP = "S"
                                    SUBTRACT W-IMPORTE
                                        FROM CTA-SALDO(W-I)
                                    ADD W-IMPORTE TO CTA-TOT-SAL(W-I)
                                    MOVE "FX" TO W-TIPO-MOV
                                    MOVE "Compra de divisas" TO W-DESC
                                    PERFORM REG-MOV
                                    MOVE CTA-SALDO(W-I) TO D-SALDO
                                    DISPLAY "  *** COMPRA REALIZADA ***"
                                    DISPLAY "  Nuevo saldo: "
                                            D-SALDO " EUR"
                                    ADD 1 TO SES-OPERACIONES
                                END-IF
                            END-IF
                        END-IF
                    END-IF
                END-PERFORM
                IF W-ENCONTRADO = "N"
                    DISPLAY "  ERROR: Divisa no encontrada"
                END-IF
            END-IF
        END-IF
    END-IF
    .

*> ================================================================
*> 7. BUSCAR CLIENTE
*> ================================================================
BUSCAR-CLIENTE.
    DISPLAY " "
    DISPLAY LN-SEP2
    DISPLAY "  *** BUSCAR CLIENTE ***"
    DISPLAY LN-SEP2
    DISPLAY "  1. Por DNI/NIF/CIF"
    DISPLAY "  2. Por primeras letras del nombre"
    DISPLAY "  3. Por email"
    DISPLAY "  0. Volver"
    DISPLAY LN-SEP2
    DISPLAY "  Opcion: " WITH NO ADVANCING
    ACCEPT W-SUBOP
    EVALUATE W-SUBOP
        WHEN 1 PERFORM BUSCAR-POR-DNI
        WHEN 2 PERFORM BUSCAR-POR-NOMBRE
        WHEN 3 PERFORM BUSCAR-POR-EMAIL
        WHEN 0 CONTINUE
        WHEN OTHER DISPLAY "  Opcion invalida"
    END-EVALUATE
    .

BUSCAR-POR-DNI.
    DISPLAY " "
    DISPLAY "  DNI/NIF/CIF: " WITH NO ADVANCING
    ACCEPT W-DESC
    MOVE ZEROS TO W-CNT
    PERFORM VARYING W-I FROM 1 BY 1
        UNTIL W-I > G-NUM-CUENTAS
        IF CTA-DNI(W-I) = W-DESC(1:10) AND CTA-ACTIVA(W-I) = "S"
            ADD 1 TO W-CNT
            MOVE CTA-SALDO(W-I) TO D-SALDO
            DISPLAY "  Cuenta: " CTA-NUM(W-I)
                    "  " CTA-TITULAR(W-I)(1:25)
                    "  " D-SALDO " EUR"
        END-IF
    END-PERFORM
    MOVE W-CNT TO D-CNT
    IF W-CNT = ZEROS
        DISPLAY "  No se encontraron resultados."
    ELSE
        DISPLAY "  Resultados: " D-CNT
    END-IF
    .

BUSCAR-POR-NOMBRE.
    DISPLAY " "
    DISPLAY "  Texto a buscar (primeros caracteres): " WITH NO ADVANCING
    ACCEPT W-DESC
    MOVE FUNCTION UPPER-CASE(W-DESC) TO W-DESC
    MOVE ZEROS TO W-CNT
    MOVE ZEROS TO W-J
    PERFORM VARYING W-I FROM 1 BY 1
        UNTIL W-I > G-NUM-CUENTAS
        MOVE FUNCTION UPPER-CASE(CTA-TITULAR(W-I)) TO NC-TITULAR
        IF CTA-ACTIVA(W-I) = "S"
            MOVE ZEROS TO W-J
            PERFORM VARYING W-J FROM 1 BY 1
                UNTIL W-J > 6
                CONTINUE
            END-PERFORM
            IF NC-TITULAR(1:6) = W-DESC(1:6) OR
               NC-TITULAR(1:5) = W-DESC(1:5) OR
               NC-TITULAR(1:4) = W-DESC(1:4) OR
               NC-TITULAR(1:3) = W-DESC(1:3)
                ADD 1 TO W-CNT
                MOVE CTA-SALDO(W-I) TO D-SALDO
                DISPLAY "  Cuenta: " CTA-NUM(W-I)
                        "  " CTA-TITULAR(W-I)(1:25)
                        "  Tel: " CTA-TEL(W-I)
            END-IF
        END-IF
    END-PERFORM
    MOVE W-CNT TO D-CNT
    IF W-CNT = ZEROS
        DISPLAY "  No se encontraron resultados."
    ELSE
        DISPLAY "  Resultados: " D-CNT
    END-IF
    .

BUSCAR-POR-EMAIL.
    DISPLAY " "
    DISPLAY "  Email (primeros 15 caracteres): " WITH NO ADVANCING
    ACCEPT W-DESC
    MOVE ZEROS TO W-CNT
    PERFORM VARYING W-I FROM 1 BY 1
        UNTIL W-I > G-NUM-CUENTAS
        IF CTA-EMAIL(W-I)(1:15) = W-DESC(1:15) AND
           CTA-ACTIVA(W-I) = "S"
            ADD 1 TO W-CNT
            MOVE CTA-SALDO(W-I) TO D-SALDO
            DISPLAY "  Cuenta: " CTA-NUM(W-I)
                    "  " CTA-TITULAR(W-I)(1:25)
        END-IF
    END-PERFORM
    IF W-CNT = ZEROS
        DISPLAY "  No se encontraron resultados."
    END-IF
    .

*> ================================================================
*> 8. INFORMES
*> ================================================================
MENU-INFORMES.
    DISPLAY " "
    DISPLAY LN-SEP2
    DISPLAY "  *** INFORMES Y ESTADISTICAS ***"
    DISPLAY LN-SEP2
    DISPLAY "  1. Informe ejecutivo del banco"
    DISPLAY "  2. Ranking de saldos"
    DISPLAY "  3. Estadisticas por tipo de cuenta"
    DISPLAY "  4. Informe de prestamos"
    DISPLAY "  5. Liquidacion de intereses"
    DISPLAY "  0. Volver"
    DISPLAY LN-SEP2
    DISPLAY "  Opcion: " WITH NO ADVANCING
    ACCEPT W-SUBOP
    EVALUATE W-SUBOP
        WHEN 1 PERFORM INF-EJECUTIVO
        WHEN 2 PERFORM INF-RANKING
        WHEN 3 PERFORM INF-ESTADISTICAS
        WHEN 4 PERFORM INF-PRESTAMOS
        WHEN 5 PERFORM INF-INTERESES
        WHEN 0 CONTINUE
        WHEN OTHER DISPLAY "  Opcion invalida"
    END-EVALUATE
    .

INF-EJECUTIVO.
    MOVE ZEROS TO W-ACUM
    MOVE ZEROS TO W-ACUM2
    MOVE ZEROS TO W-CNT
    MOVE ZEROS TO W-TEMP
    MOVE ZEROS TO W-TEMP2

    PERFORM VARYING W-I FROM 1 BY 1
        UNTIL W-I > G-NUM-CUENTAS
        IF CTA-ACTIVA(W-I) = "S"
            ADD CTA-SALDO(W-I) TO W-ACUM
            ADD CTA-TOT-ING(W-I) TO W-ACUM2
            ADD 1 TO W-CNT
        END-IF
    END-PERFORM

    MOVE ZEROS TO W-TEMP2
    PERFORM VARYING W-K FROM 1 BY 1
        UNTIL W-K > G-NUM-PRESTAMOS
        IF PRE-ACTIVO(W-K) = "S"
            ADD PRE-SALDO-PDT(W-K) TO W-TEMP2
        END-IF
    END-PERFORM

    IF W-CNT > ZEROS
        DIVIDE W-CNT INTO W-ACUM GIVING W-MEDIA
    END-IF

    DISPLAY " "
    DISPLAY LN-SEP3
    DISPLAY "##                                                                ##"
    DISPLAY "##         INFORME EJECUTIVO - " CFG-NOMBRE-BANCO "   ##"
    DISPLAY "##                  Fecha: " SES-FECHA "                          ##"
    DISPLAY "##                                                                ##"
    DISPLAY LN-SEP3
    MOVE G-NUM-CUENTAS    TO D-CNT
    DISPLAY "  Cuentas registradas    : " D-CNT
    MOVE W-CNT            TO D-CNT
    DISPLAY "  Cuentas activas        : " D-CNT
    MOVE G-NUM-MOVS       TO D-CNT
    DISPLAY "  Total movimientos      : " D-CNT
    MOVE G-NUM-PRESTAMOS  TO D-CNT
    DISPLAY "  Prestamos activos      : " D-CNT
    MOVE G-NUM-TARJETAS   TO D-CNT
    DISPLAY "  Tarjetas emitidas      : " D-CNT
    DISPLAY LN-SEP2
    MOVE W-ACUM           TO D-SALDO
    DISPLAY "  SALDO TOTAL BANCO      : " D-SALDO " EUR"
    MOVE W-MEDIA          TO D-IMP
    DISPLAY "  Saldo medio por cuenta : " D-IMP " EUR"
    MOVE W-ACUM2          TO D-IMP
    DISPLAY "  Total ingresos hist.   : " D-IMP " EUR"
    MOVE W-TEMP2          TO D-IMP
    DISPLAY "  Prestamos pendientes   : " D-IMP " EUR"
    MOVE SES-OPERACIONES  TO D-CNT
    DISPLAY "  Operaciones en sesion  : " D-CNT
    DISPLAY LN-SEP3
    .

INF-RANKING.
    DISPLAY " "
    DISPLAY LN-SEP2
    DISPLAY "  RANKING DE CUENTAS POR SALDO"
    DISPLAY LN-SEP2
    DISPLAY "  POS  CUENTA    TITULAR                    T  SALDO EUR"
    DISPLAY LN-SEP2
    MOVE ZEROS TO W-CNT
    PERFORM VARYING W-I FROM 1 BY 1
        UNTIL W-I > G-NUM-CUENTAS
        IF CTA-ACTIVA(W-I) = "S"
            ADD 1 TO W-CNT
            MOVE W-CNT TO D-CNT2
            MOVE CTA-SALDO(W-I) TO D-SALDO
            DISPLAY "   " D-CNT2
                    "  " CTA-NUM(W-I)
                    "  " CTA-TITULAR(W-I)(1:24)
                    "  " CTA-TIPO(W-I)
                    "  " D-SALDO
        END-IF
    END-PERFORM
    DISPLAY LN-SEP2
    .

INF-ESTADISTICAS.
    DISPLAY " "
    DISPLAY LN-SEP1
    DISPLAY "  ESTADISTICAS POR TIPO DE CUENTA"
    DISPLAY LN-SEP1
    MOVE "A" TO NC-TIPO
    PERFORM MOSTRAR-STATS-TIPO
    MOVE "C" TO NC-TIPO
    PERFORM MOSTRAR-STATS-TIPO
    MOVE "P" TO NC-TIPO
    PERFORM MOSTRAR-STATS-TIPO
    MOVE "J" TO NC-TIPO
    PERFORM MOSTRAR-STATS-TIPO
    MOVE "E" TO NC-TIPO
    PERFORM MOSTRAR-STATS-TIPO
    DISPLAY LN-SEP1
    .

MOSTRAR-STATS-TIPO.
    MOVE ZEROS TO W-CNT
    MOVE ZEROS TO W-ACUM
    MOVE ZEROS TO W-ACUM2
    PERFORM VARYING W-I FROM 1 BY 1
        UNTIL W-I > G-NUM-CUENTAS
        IF CTA-TIPO(W-I) = NC-TIPO AND CTA-ACTIVA(W-I) = "S"
            ADD 1 TO W-CNT
            ADD CTA-SALDO(W-I) TO W-ACUM
            ADD CTA-TOT-ING(W-I) TO W-ACUM2
        END-IF
    END-PERFORM
    EVALUATE NC-TIPO
        WHEN "A" DISPLAY "  [ AHORRO       3.50% ]"
        WHEN "C" DISPLAY "  [ CORRIENTE    1.50% ]"
        WHEN "P" DISPLAY "  [ PLAZO FIJO   8.00% ]"
        WHEN "J" DISPLAY "  [ JUVENIL      4.00% ]"
        WHEN "E" DISPLAY "  [ EMPRESA      1.00% ]"
    END-EVALUATE
    MOVE W-CNT TO D-CNT
    DISPLAY "    Num. cuentas : " D-CNT
    MOVE W-ACUM TO D-IMP
    DISPLAY "    Saldo total  : " D-IMP " EUR"
    IF W-CNT > ZEROS
        DIVIDE W-CNT INTO W-ACUM GIVING W-MEDIA
        MOVE W-MEDIA TO D-IMP
        DISPLAY "    Saldo medio  : " D-IMP " EUR"
    END-IF
    DISPLAY LN-SEP2
    .

INF-PRESTAMOS.
    DISPLAY " "
    DISPLAY LN-SEP2
    DISPLAY "  INFORME DE PRESTAMOS"
    DISPLAY LN-SEP2
    MOVE ZEROS TO W-CNT
    MOVE ZEROS TO W-ACUM
    PERFORM VARYING W-K FROM 1 BY 1
        UNTIL W-K > G-NUM-PRESTAMOS
        ADD 1 TO W-CNT
        IF PRE-ACTIVO(W-K) = "S"
            ADD PRE-SALDO-PDT(W-K) TO W-ACUM
            DISPLAY "  ID: " PRE-ID(W-K)
                    "  Cta: " PRE-CTA(W-K)
            DISPLAY "  Titular: " PRE-TITULAR(W-K)
            MOVE PRE-CAPITAL(W-K) TO D-IMP
            DISPLAY "  Capital: " D-IMP " EUR"
            MOVE PRE-SALDO-PDT(W-K) TO D-IMP
            DISPLAY "  Pdte.  : " D-IMP " EUR"
            MOVE PRE-CUOTA(W-K) TO D-IMP
            DISPLAY "  Cuota  : " D-IMP " EUR/mes"
            MOVE PRE-CUOTAS-PDT(W-K) TO D-CNT2
            DISPLAY "  Cuotas : " D-CNT2 " restantes"
            DISPLAY "  Estado : ACTIVO"
            DISPLAY LN-SEP2
        END-IF
    END-PERFORM
    IF W-CNT = ZEROS
        DISPLAY "  Sin prestamos registrados."
    ELSE
        MOVE W-ACUM TO D-IMP
        DISPLAY "  TOTAL PENDIENTE: " D-IMP " EUR"
    END-IF
    .

INF-INTERESES.
    DISPLAY " "
    DISPLAY "  === LIQUIDACION DE INTERESES ANUALES ==="
    DISPLAY "  Tasas: A=3.50%  C=1.50%  P=8.00%  J=4.00%  E=1.00%"
    DISPLAY "  Confirmar abono a todas las cuentas? (S/N): "
            WITH NO ADVANCING
    ACCEPT W-RESP
    MOVE FUNCTION UPPER-CASE(W-RESP) TO W-RESP
    IF W-RESP = "S"
        DISPLAY " "
        DISPLAY LN-SEP2
        DISPLAY "  CUENTA    TITULAR                      T  INTERES"
        DISPLAY LN-SEP2
        MOVE ZEROS TO W-ACUM
        PERFORM VARYING W-I FROM 1 BY 1
            UNTIL W-I > G-NUM-CUENTAS
            IF CTA-ACTIVA(W-I) = "S"
                EVALUATE CTA-TIPO(W-I)
                    WHEN "A"
                        MULTIPLY CTA-SALDO(W-I) BY CFG-TASA-AHORRO
                            GIVING W-INTERES
                    WHEN "C"
                        MULTIPLY CTA-SALDO(W-I) BY CFG-TASA-CORRIENTE
                            GIVING W-INTERES
                    WHEN "P"
                        MULTIPLY CTA-SALDO(W-I) BY CFG-TASA-PLAZO
                            GIVING W-INTERES
                    WHEN "J"
                        MULTIPLY CTA-SALDO(W-I) BY CFG-TASA-JUVENIL
                            GIVING W-INTERES
                    WHEN "E"
                        MULTIPLY CTA-SALDO(W-I) BY CFG-TASA-EMPRESA
                            GIVING W-INTERES
                END-EVALUATE
                ADD W-INTERES TO CTA-SALDO(W-I)
                ADD W-INTERES TO CTA-TOT-ING(W-I)
                ADD W-INTERES TO W-ACUM
                MOVE W-INTERES TO D-IMP
                DISPLAY "  " CTA-NUM(W-I)
                        "  " CTA-TITULAR(W-I)(1:28)
                        "  " CTA-TIPO(W-I)
                        "  +" D-IMP " EUR"
                MOVE W-INTERES TO W-IMPORTE
                MOVE "Abono intereses anuales" TO W-DESC
                MOVE "IN" TO W-TIPO-MOV
                PERFORM REG-MOV
            END-IF
        END-PERFORM
        DISPLAY LN-SEP2
        MOVE W-ACUM TO D-IMP
        DISPLAY "  TOTAL INTERESES ABONADOS: " D-IMP " EUR"
        DISPLAY "  *** LIQUIDACION COMPLETADA ***"
        ADD 1 TO SES-OPERACIONES
    ELSE
        DISPLAY "  Liquidacion cancelada."
    END-IF
    .

*> ================================================================
*> RUTINAS AUXILIARES
*> ================================================================
BUSCAR-CTA.
    MOVE "N" TO W-ENCONTRADO
    MOVE ZEROS TO W-IDX-A
    PERFORM VARYING W-I FROM 1 BY 1
        UNTIL W-I > G-NUM-CUENTAS
        IF CTA-NUM(W-I) = W-BUSCAR-NRO
            MOVE "S" TO W-ENCONTRADO
            MOVE W-I TO W-IDX-A
        END-IF
    END-PERFORM
    .

PEDIR-PIN.
    MOVE "N" TO W-PIN-OK
    DISPLAY "  PIN (4 digitos): " WITH NO ADVANCING
    ACCEPT W-PIN-INPUT
    IF W-PIN-INPUT = CTA-PIN(W-I)
        MOVE "S" TO W-PIN-OK
        MOVE ZEROS TO CTA-INTENTOS(W-I)
    ELSE
        ADD 1 TO CTA-INTENTOS(W-I)
        DISPLAY "  PIN incorrecto. Intento "
                CTA-INTENTOS(W-I) " de 3."
        IF CTA-INTENTOS(W-I) >= 3
            MOVE "S" TO CTA-BLOQUEADA(W-I)
            DISPLAY "  *** CUENTA BLOQUEADA: 3 intentos fallidos ***"
            DISPLAY "  Acuda a sucursal para desbloquear."
        END-IF
    END-IF
    .

VERIFICAR-OPERABLE.
    MOVE "N" TO W-ERROR
    IF CTA-ACTIVA(W-I) = "N"
        DISPLAY "  ERROR: Cuenta cancelada"
        MOVE "S" TO W-ERROR
    END-IF
    IF CTA-BLOQUEADA(W-I) = "S"
        DISPLAY "  ERROR: Cuenta bloqueada. Acuda a sucursal."
        MOVE "S" TO W-ERROR
    END-IF
    .

REG-MOV.
    IF G-NUM-MOVS < CFG-MAX-MOVS
        ADD 1 TO G-NUM-MOVS
        ADD 1 TO G-REF-SEQ
        ADD 1 TO CTA-NUM-MOVS(W-I)
        MOVE G-NUM-MOVS         TO MOV-ID(G-NUM-MOVS)
        MOVE CTA-NUM(W-I)       TO MOV-CTA(G-NUM-MOVS)
        MOVE W-TIPO-MOV         TO MOV-TIPO(G-NUM-MOVS)
        MOVE W-IMPORTE          TO MOV-IMPORTE(G-NUM-MOVS)
        MOVE CTA-SALDO(W-I)     TO MOV-SALDO-TR(G-NUM-MOVS)
        MOVE W-DESC             TO MOV-DESC(G-NUM-MOVS)
        MOVE SES-FECHA          TO MOV-FECHA(G-NUM-MOVS)
        MOVE SES-HORA           TO MOV-HORA(G-NUM-MOVS)
        MOVE "C"                TO MOV-ESTADO(G-NUM-MOVS)
        MOVE "REF" TO W-REF-STR(1:3)
        MOVE G-REF-SEQ TO MOV-REF(G-NUM-MOVS)
        MOVE MOV-REF(G-NUM-MOVS) TO W-REF-STR
    END-IF
    .

*> ================================================================
*> CARGA DE DATOS DEMO
*> ================================================================
CARGAR-DATOS-DEMO.
    INITIALIZE TBL-CUENTAS
    INITIALIZE TBL-MOVIMIENTOS
    INITIALIZE TBL-PRESTAMOS
    INITIALIZE TBL-TARJETAS
    INITIALIZE TBL-DIVISAS

    *> Tipos de cambio
    MOVE "USD" TO DIV-CODIGO(1)
    MOVE "Dolar Estadounidense" TO DIV-NOMBRE(1)
    MOVE 1.0870 TO DIV-CAMBIO(1)
    MOVE "$" TO DIV-SIMBOLO(1)

    MOVE "GBP" TO DIV-CODIGO(2)
    MOVE "Libra Esterlina    " TO DIV-NOMBRE(2)
    MOVE 0.8520 TO DIV-CAMBIO(2)
    MOVE "PS" TO DIV-SIMBOLO(2)

    MOVE "JPY" TO DIV-CODIGO(3)
    MOVE "Yen Japones        " TO DIV-NOMBRE(3)
    MOVE 161.50 TO DIV-CAMBIO(3)
    MOVE "JP" TO DIV-SIMBOLO(3)

    MOVE "CHF" TO DIV-CODIGO(4)
    MOVE "Franco Suizo       " TO DIV-NOMBRE(4)
    MOVE 0.9780 TO DIV-CAMBIO(4)
    MOVE "FS" TO DIV-SIMBOLO(4)

    MOVE "MXN" TO DIV-CODIGO(5)
    MOVE "Peso Mexicano      " TO DIV-NOMBRE(5)
    MOVE 20.450 TO DIV-CAMBIO(5)
    MOVE "MX" TO DIV-SIMBOLO(5)

    MOVE "BRL" TO DIV-CODIGO(6)
    MOVE "Real Brasileno     " TO DIV-NOMBRE(6)
    MOVE 5.6200 TO DIV-CAMBIO(6)
    MOVE "R$" TO DIV-SIMBOLO(6)

    *> Cuentas demo
    MOVE 1            TO G-NUM-CUENTAS
    MOVE 30000001     TO CTA-NUM(1)
    MOVE "GARCIA LOPEZ, MARIA CARMEN"      TO CTA-TITULAR(1)
    MOVE "12345678A"  TO CTA-DNI(1)
    MOVE "612345678"  TO CTA-TEL(1)
    MOVE "m.garcia@email.es"               TO CTA-EMAIL(1)
    MOVE "A"          TO CTA-TIPO(1)
    MOVE 8500.00      TO CTA-SALDO(1)
    MOVE 8500.00      TO CTA-SALDO-MAX(1)
    MOVE 8500.00      TO CTA-SALDO-MIN(1)
    MOVE 8500.00      TO CTA-TOT-ING(1)
    MOVE ZEROS        TO CTA-TOT-SAL(1)
    MOVE 1234         TO CTA-PIN(1)
    MOVE "S"          TO CTA-ACTIVA(1)
    MOVE "N"          TO CTA-BLOQUEADA(1)
    MOVE "2024-01-15" TO CTA-FECHA-APT(1)

    ADD 1             TO G-NUM-CUENTAS
    MOVE 30000002     TO CTA-NUM(2)
    MOVE "MARTINEZ RUIZ, JOSE ANTONIO"     TO CTA-TITULAR(2)
    MOVE "87654321B"  TO CTA-DNI(2)
    MOVE "698765432"  TO CTA-TEL(2)
    MOVE "j.martinez@mail.com"             TO CTA-EMAIL(2)
    MOVE "C"          TO CTA-TIPO(2)
    MOVE 22500.75     TO CTA-SALDO(2)
    MOVE 22500.75     TO CTA-SALDO-MAX(2)
    MOVE 22500.75     TO CTA-SALDO-MIN(2)
    MOVE 22500.75     TO CTA-TOT-ING(2)
    MOVE ZEROS        TO CTA-TOT-SAL(2)
    MOVE 5678         TO CTA-PIN(2)
    MOVE "S"          TO CTA-ACTIVA(2)
    MOVE "N"          TO CTA-BLOQUEADA(2)
    MOVE "2023-06-20" TO CTA-FECHA-APT(2)

    ADD 1             TO G-NUM-CUENTAS
    MOVE 30000003     TO CTA-NUM(3)
    MOVE "FERNANDEZ TORRES, ANA BELEN"     TO CTA-TITULAR(3)
    MOVE "11223344C"  TO CTA-DNI(3)
    MOVE "677112233"  TO CTA-TEL(3)
    MOVE "ana.f@empresa.es"                TO CTA-EMAIL(3)
    MOVE "P"          TO CTA-TIPO(3)
    MOVE 75000.00     TO CTA-SALDO(3)
    MOVE 75000.00     TO CTA-SALDO-MAX(3)
    MOVE 75000.00     TO CTA-SALDO-MIN(3)
    MOVE 75000.00     TO CTA-TOT-ING(3)
    MOVE ZEROS        TO CTA-TOT-SAL(3)
    MOVE 9012         TO CTA-PIN(3)
    MOVE "S"          TO CTA-ACTIVA(3)
    MOVE "N"          TO CTA-BLOQUEADA(3)
    MOVE "2022-11-30" TO CTA-FECHA-APT(3)

    ADD 1             TO G-NUM-CUENTAS
    MOVE 30000004     TO CTA-NUM(4)
    MOVE "LOPEZ GARCIA, CARLOS"            TO CTA-TITULAR(4)
    MOVE "55667788D"  TO CTA-DNI(4)
    MOVE "644556677"  TO CTA-TEL(4)
    MOVE "c.lopez@joven.es"                TO CTA-EMAIL(4)
    MOVE "J"          TO CTA-TIPO(4)
    MOVE 1250.50      TO CTA-SALDO(4)
    MOVE 1250.50      TO CTA-SALDO-MAX(4)
    MOVE 1250.50      TO CTA-SALDO-MIN(4)
    MOVE 1250.50      TO CTA-TOT-ING(4)
    MOVE ZEROS        TO CTA-TOT-SAL(4)
    MOVE 3456         TO CTA-PIN(4)
    MOVE "S"          TO CTA-ACTIVA(4)
    MOVE "N"          TO CTA-BLOQUEADA(4)
    MOVE "2025-03-01" TO CTA-FECHA-APT(4)

    ADD 1             TO G-NUM-CUENTAS
    MOVE 30000005     TO CTA-NUM(5)
    MOVE "CONSTRUCTORA IBERIA S.L."        TO CTA-TITULAR(5)
    MOVE "B12345678"  TO CTA-DNI(5)
    MOVE "915551234"  TO CTA-TEL(5)
    MOVE "admin@ciberia.es"                TO CTA-EMAIL(5)
    MOVE "E"          TO CTA-TIPO(5)
    MOVE 145000.00    TO CTA-SALDO(5)
    MOVE 145000.00    TO CTA-SALDO-MAX(5)
    MOVE 145000.00    TO CTA-SALDO-MIN(5)
    MOVE 145000.00    TO CTA-TOT-ING(5)
    MOVE ZEROS        TO CTA-TOT-SAL(5)
    MOVE 7890         TO CTA-PIN(5)
    MOVE "S"          TO CTA-ACTIVA(5)
    MOVE "N"          TO CTA-BLOQUEADA(5)
    MOVE "2021-04-10" TO CTA-FECHA-APT(5)

    MOVE 30000005     TO G-ULT-CTA

    *> Movimientos demo
    ADD 1 TO G-NUM-MOVS
    MOVE G-NUM-MOVS   TO MOV-ID(G-NUM-MOVS)
    MOVE 30000001     TO MOV-CTA(G-NUM-MOVS)
    MOVE "DE"         TO MOV-TIPO(G-NUM-MOVS)
    MOVE 8500.00      TO MOV-IMPORTE(G-NUM-MOVS)
    MOVE 8500.00      TO MOV-SALDO-TR(G-NUM-MOVS)
    MOVE "Ingreso inicial apertura"        TO MOV-DESC(G-NUM-MOVS)
    MOVE "2024-01-15" TO MOV-FECHA(G-NUM-MOVS)
    MOVE "10:00:00"   TO MOV-HORA(G-NUM-MOVS)
    MOVE "REF200001"  TO MOV-REF(G-NUM-MOVS)
    MOVE 1            TO CTA-NUM-MOVS(1)

    ADD 1 TO G-NUM-MOVS
    MOVE G-NUM-MOVS   TO MOV-ID(G-NUM-MOVS)
    MOVE 30000002     TO MOV-CTA(G-NUM-MOVS)
    MOVE "DE"         TO MOV-TIPO(G-NUM-MOVS)
    MOVE 22500.75     TO MOV-IMPORTE(G-NUM-MOVS)
    MOVE 22500.75     TO MOV-SALDO-TR(G-NUM-MOVS)
    MOVE "Ingreso inicial apertura"        TO MOV-DESC(G-NUM-MOVS)
    MOVE "2023-06-20" TO MOV-FECHA(G-NUM-MOVS)
    MOVE "11:30:00"   TO MOV-HORA(G-NUM-MOVS)
    MOVE "REF200002"  TO MOV-REF(G-NUM-MOVS)
    MOVE 1            TO CTA-NUM-MOVS(2)

    ADD 1 TO G-NUM-MOVS
    MOVE G-NUM-MOVS   TO MOV-ID(G-NUM-MOVS)
    MOVE 30000003     TO MOV-CTA(G-NUM-MOVS)
    MOVE "DE"         TO MOV-TIPO(G-NUM-MOVS)
    MOVE 75000.00     TO MOV-IMPORTE(G-NUM-MOVS)
    MOVE 75000.00     TO MOV-SALDO-TR(G-NUM-MOVS)
    MOVE "Ingreso inicial plazo fijo"      TO MOV-DESC(G-NUM-MOVS)
    MOVE "2022-11-30" TO MOV-FECHA(G-NUM-MOVS)
    MOVE "09:00:00"   TO MOV-HORA(G-NUM-MOVS)
    MOVE "REF200003"  TO MOV-REF(G-NUM-MOVS)
    MOVE 1            TO CTA-NUM-MOVS(3)

    *> Prestamo demo
    MOVE 1            TO G-NUM-PRESTAMOS
    ADD 1             TO G-ULT-PRE
    MOVE G-ULT-PRE    TO PRE-ID(1)
    MOVE 30000002     TO PRE-CTA(1)
    MOVE "MARTINEZ RUIZ, JOSE ANTONIO"     TO PRE-TITULAR(1)
    MOVE 15000.00     TO PRE-CAPITAL(1)
    MOVE 11250.00     TO PRE-SALDO-PDT(1)
    MOVE 312.50       TO PRE-CUOTA(1)
    MOVE 60           TO PRE-PLAZO(1)
    MOVE 36           TO PRE-CUOTAS-PDT(1)
    MOVE 0.0650       TO PRE-TASA(1)
    MOVE "S"          TO PRE-ACTIVO(1)
    MOVE "2023-07-01" TO PRE-FECHA(1)

    *> Tarjeta demo
    MOVE 1            TO G-NUM-TARJETAS
    MOVE "4532 100001 1234 5678" TO TAR-NUM(1)
    MOVE 30000002     TO TAR-CTA(1)
    MOVE "MARTINEZ RUIZ, JOSE ANTONIO"     TO TAR-TITULAR(1)
    MOVE "C"          TO TAR-TIPO(1)
    MOVE 3000.00      TO TAR-LIMITE(1)
    MOVE 750.00       TO TAR-USADO(1)
    MOVE 321          TO TAR-CVV(1)
    MOVE "03/28"      TO TAR-CADUCIDAD(1)
    MOVE "S"          TO TAR-ACTIVA(1)
    MOVE "N"          TO TAR-BLOQUEADA(1)
    MOVE "2023-06-20" TO TAR-FECHA-EMI(1)
    .
