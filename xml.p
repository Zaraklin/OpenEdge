CREATE ALIAS mguni FOR DATABASE mgcad.
/* Temporary Table Definitions ---                                           */
DEF VAR c-arq         AS CHAR NO-UNDO.
DEF VAR c-path        AS CHAR NO-UNDO.
DEF VAR c-tipo        AS CHAR NO-UNDO.

DEF VAR c-arq-aux     AS CHAR NO-UNDO.

DEF VAR pc-sku          AS CHAR NO-UNDO.
DEF VAR pc-lote         AS CHAR NO-UNDO.
DEF VAR pc-dtValidade   AS CHAR NO-UNDO.
DEF VAR pc-TempoValRest AS CHAR NO-UNDO.
DEF VAR pc-UnValRest    AS CHAR NO-UNDO.

define temp-table tt-param no-undo
    field dir-entrada      as character.

define temp-table tt-arquivo no-undo
    field arquivo          as character
    index tt-01 is primary unique arquivo.   
    
define temp-table Item /*Xml-node-name "Item"*/
    Field SKU          As Character /* Xml-node-name "SKU"        */
    Field Lote         As Character /* Xml-node-name "Lote"       */
    Field DtValidade   As Character /* Xml-node-name "DtValidade" */
    FIELD TempoValRest AS CHARACTER
    FIELD UnValRest    AS CHARACTER.
    . 

Define Variable lo-resultado As Logical No-undo.

Define Dataset ItemLote /*Xml-node-name "ItemLote"*/ For Item.

def var h-acomp             as handle no-undo.
 
run utp/ut-acomp.p persistent set h-acomp.

RUN pi-inicializar IN h-acomp (input "Inicializando").


create tt-param.
assign tt-param.dir-entrada = "".

/* Lista os XMLs do diret¢rio de entrada */
input from os-dir(tt-param.dir-entrada).
repeat:
    import c-arq c-path c-tipo.

    if not c-tipo = "F" then next.
    if r-index(c-arq,".xml") = 0 then next.

    if not can-find(first tt-arquivo where
                          tt-arquivo.arquivo = c-arq) then do:
        create tt-arquivo.
        assign tt-arquivo.arquivo = c-arq.
    end.
end.
input close.

for each tt-arquivo:
    assign c-arq-aux = tt-param.dir-entrada + tt-arquivo.arquivo.
    
    RUN pi-inicializar IN h-acomp (input c-arq-aux).
    
    run pi-acompanhar in h-acomp (input "Carregando itens...").

    lo-resultado = Dataset ItemLote:Read-xml("FILE", c-arq-aux,?,?,?,?).
    
    For Each Item:
        
        ASSIGN pc-sku          = ITEM.sku
               pc-lote         = ITEM.lote
               pc-dtValidade   = ITEM.DtValidade
               pc-TempoValRest = ITEM.TempoValRest
               pc-UnValRest    = ITEM.UnValRest.

        run pi-acompanhar in h-acomp (input "SKU: " + pc-sku + "/Lote:" + pc-lote).

        if dec(pc-dtValidade) > 0 then
            pc-dtValidade = string(substring(pc-dtValidade,7,2) + substring(pc-dtValidade,5,2) + substring(pc-dtValidade,1,4), '99/99/9999').
        else
            pc-dtValidade = ?.
        
        find first b-item-lote where
                   b-item-lote.it-codigo = pc-sku  and
                   b-item-lote.nr-lote   = pc-lote exclusive-lock no-error.

        if avail b-item-lote then
            delete b-item-lote.

        create b-item-lote.
        assign b-item-lote.it-codigo   = pc-sku
               b-item-lote.nr-lote     = pc-lote
               b-item-lote.dt-validade = date(pc-dtValidade)
               b-item-lote.tempo-valid = integer(TempoValRest)
               b-item-lote.unid-medida = UnValRest   
               .
    end.
end.                                     

run pi-finalizar in h-acomp.
