/*----------------------------------------------------------------------------*
 * @(#) Programa - aux_arvore_progr.p                                         *
 *                                                                            *
 * @(#) Funcao   - Auxiliar programa arvore_progr a identificar valor passado *
 *                 em chamadas run value ()                                   *
 *                                                                            *
 *      Parametos: {1} = "tabela"                                             * 
 *                   {2} = nome_tabela                                        *
 *                   {3} = campo_tabela                                       *
 *                                                                            *
 *                 {1} = "variavel"                                           *
 *                   {2} = nome_variavel                                      *
 *                   {3} = linha_run                                          *
 *                                                                            *
 *                 {1} = "assign_var"                                         *
 *                   {2} = nome_variavel                                      *
 *                   {3} = linha_run                                          *
 *                                                                            *
 *                 {1} = "init_var"                                           *
 *                   {2} = nome_variavel                                      *
 *                   {3} = linha_run                                          *
 *                                                                            * 
 *----------------------------------------------------------------------------*/

def shared var cprog_ini   as char no-undo label "Programa inicial" format "x(20)".
def shared var cprog_fim   as char no-undo label "Programa buscado"  format "x(20)".

def shared var i_nivel     as int  no-undo init 0.
def shared var c_prog_pai  as char no-undo init "".
def shared var i_id        as int  no-undo.
def shared var i_id_pai    as int  no-undo.

def        var wfonte      as char no-undo.
def        var wlinha      as char no-undo.

def shared var l_achou     as log  no-undo.
def shared var l_busca     as log  no-undo.

def shared var wini        as int  no-undo.

def        var i_laux      as int  no-undo init 0.
def        var windex      as int  no-undo.
def        var flag_ponto  as log  no-undo.
def        var flag_init   as log  no-undo.
def        var wvar        as char no-undo.
def        var wval_var    as char no-undo.
def        var wvar_fin    as char no-undo.
def        var wint        as int  no-undo.
def        var wseq        as int  no-undo.
def        var wint2       as int  no-undo.
def        var wparte      as int  no-undo.

&if     "{1}" = "tabela"     or "{1}" = "variavel" &then
def input  param r_tt-progr as rowid no-undo.
&elseif "{1}" = "assign_var" or "{1}" = "init_var" &then
def output param valor_var  as char  no-undo.
&endif

def shared temp-table tt-analise no-undo
  field id        as int
  field nivel     as int
  field id_pai    as int
  field progr_pai as char
  field progr     as char
  field runvalue  as log
  field runline   as int
  field e_xref    as log
  field escrito   as log
  index idx01 is primary id_pai progr_pai id progr  
  index idx02            id     nivel
  index idx03 is unique  id_pai progr_pai id progr.
  
def shared temp-table tt-progrs no-undo
  field prog_ini as char format "x(50)"
  field achou    as log
  index idx01 as primary unique
        prog_ini.
        
def temp-table tt-varaux no-undo
  field nome    as char
  field val     as char
  field seq     as int
  field tipo    as char
  field parte   as int
  field cs_ind  as char
  index idx01 is primary seq nome tipo
  index idx02 tipo.

def buffer btt-analise for tt-analise.
def buffer btt-varaux  for tt-varaux.

def stream sler.

&if "{1}" = "tabela" or "{1}" = "variavel" &then
find first tt-progrs where rowid(tt-progrs) = r_tt-progr exclusive-lock no-error.
&endif

/* tabela enviada como parametro no run value */
&if "{1}" = "tabela" &then
  /*
  message "tabela"
    view-as alert-box INFO buttons ok.  
  */
  
  hide message no-pause.
  message "buscando run value de tabela - Total: " string(time - wini, "hh:mm:ss").
  
  for each {2} no-lock:    
    if {2}.{3} <> "" then do:    
      i_id = i_id + 1.
      
      create btt-analise.      
      assign btt-analise.id        = i_id
             btt-analise.nivel     = i_nivel
             btt-analise.id_pai    = i_id_pai
             btt-analise.progr_pai = c_prog_pai
             btt-analise.progr     = {2}.{3}
             btt-analise.runvalue  = yes
             btt-analise.e_xref    = no
             btt-analise.escrito   = no.
      
      if entry(1, btt-analise.progr, ".") = entry(1, cprog_fim, ".") then do:
        
        /* Marca que encontrou o programa */
        assign tt-progrs.achou = yes
               l_achou         = yes.
        
        if not l_busca then do:
          message "Programa " + cprog_fim + " esta na estrutura."
            view-as alert-box INFO buttons ok.
        end.
      end.
    end.
  end.
/* variavel enviada como parametro no run value */
&elseif "{1}" = "variavel" &then
  /*
  message "variavel"
    view-as alert-box INFO buttons ok.
  */
  
  hide message no-pause.
  message "buscando run value de variavel - Total: " string(time - wini, "hh:mm:ss").
  
  assign wvar     = "{2}"
         wvar_fin = "".
  
  run aux_arvore_progr.p (output wval_var) "assign_var" wvar "{3}".
  
  /* caso nao ache assign, grava o valor inicial */
  if wval_var = "" then do:
    if index(wvar, "[") > 0 then
      run aux_arvore_progr.p (output wval_var) "init_var" entry(1, wvar, "[") "{3}".
    else
      run aux_arvore_progr.p (output wval_var) "init_var" wvar "{3}".
    
    wseq = 0.
    do wint = 1 to num-entries(wval_var, ","):      
      if trim(entry(wint, wval_var, ",")) <> "" then do:
        create tt-varaux.
        assign tt-varaux.nome  = wvar
               tt-varaux.val   = trim(entry(wint, wval_var, ","))
               tt-varaux.seq   = wseq
               tt-varaux.parte = 1
               tt-varaux.tipo  = "parte".
               
        wseq = wseq + 1.
      end.
    end.
    
    wparte = 1.
  end.
  else do:
    /* trata assign para pegar valor da variavel */
    if num-entries(wval_var, "+") <= 0 then do:
      /*
      message "Variavel com valor final"
        view-as alert-box INFO buttons ok.
      */
      /* variavel ja esta com valor final */
      wseq = 1.
      
      create tt-varaux.
      assign tt-varaux.nome  = wvar
             tt-varaux.val   = trim(wval_var)
             tt-varaux.seq   = wseq
             tt-varaux.parte = 1
             tt-varaux.tipo  = "base_var".
      
      if tt-varaux.val matches "string(*" then do:
        assign tt-varaux.val     = entry(1, wval_var, ",")
               tt-varaux.cs_ind  = entry(2, wval_var, ",").
      end.        
            
      assign tt-varaux.val     = replace(replace(replace(replace(replace(tt-varaux.val, ",", ""), ")", ""), "(", ""), "string", ""), "int", "")
             tt-varaux.cs_ind  = replace(replace(replace(tt-varaux.cs_ind, '"', ''), "(", ""), ")", "").
             
      /* remove o ponto do fim, quando houver */
      tt-varaux.val = trim(tt-varaux.val).
      if substr(tt-varaux.val, length(tt-varaux.val)) = "." then
        tt-varaux.val = substr(tt-varaux.val, 1, length(tt-varaux.val) - 1).
      
      assign wvar_fin = wval_var 
             wparte   = 1. 
    end.
    else do:
      /* e necessario montar o valor final da variavel - sera sempre char, pois e um programa */
      wseq = 0.
      do wint = 1 to num-entries(wval_var, "+"):     
        /*
        message "Montando valor run value"
          view-as alert-box INFO buttons ok.
        */
        
        wseq = wseq + 1.      
        
        create tt-varaux.
        assign tt-varaux.nome  = wvar
               tt-varaux.val   = trim(entry(wint, wval_var, "+"))
               tt-varaux.seq   = wseq
               tt-varaux.parte = wint
               tt-varaux.tipo  = "base_var".
        
        if tt-varaux.val matches "string(*" then do:
          assign tt-varaux.val     = entry(1, trim(entry(wint, wval_var, "+")), ",")
                 tt-varaux.cs_ind  = entry(2, trim(entry(wint, wval_var, "+")), ",").
        end.        
              
        assign tt-varaux.val     = replace(replace(replace(replace(replace(tt-varaux.val, ",", ""), ")", ""), "(", ""), "string", ""), "int", "")
               tt-varaux.cs_ind  = replace(replace(replace(tt-varaux.cs_ind, '"', ''), "(", ""), ")", "").
               
        /* remove o ponto do fim, quando houver */
        tt-varaux.val = trim(tt-varaux.val).
        if substr(tt-varaux.val, length(tt-varaux.val)) = "." then
          tt-varaux.val = substr(tt-varaux.val, 1, length(tt-varaux.val) - 1).
      end.
      
      wparte = wint.
    end.
        
    for each tt-varaux where tt-varaux.tipo = "base_var" no-lock:
      /*
      message "base_var: " tt-varaux.val
        view-as alert-box INFO buttons ok.    
      */
      if tt-varaux.val matches ('"*"') then do:
        /*
        message "Valor"
          view-as alert-box INFO buttons ok.
        */
        create btt-varaux.
        buffer-copy tt-varaux to btt-varaux
           assign btt-varaux.seq   = 0
                  btt-varaux.tipo  = "parte".
      end.
      else if tt-varaux.val matches ("*[*]") then do:
        /*
        message "Array"
          view-as alert-box INFO buttons ok.
        */      
        assign wseq   = 0
               wint   = 0
               wvar   = tt-varaux.val
               windex = index(wvar,"[").
        
        /* busca valores de um array */
        do wint = 1 to 999:                    
          
          /* valida se o indice do array e um inteiro ou uma variavel */
          wint2 = int(substr(wvar, windex, index(wvar, "]", windex))) no-error.          
          if error-status:num-messages > 0 then do:
            /*
            disp (substr(wvar, windex + 1, index(wvar, "]", windex) - (windex + 1)) +
                  " = "                                                             +
                  string(wint, tt-varaux.cs_ind)) format "x(50)".
            */
            
            substr(wvar, windex + 1, index(wvar, "]", windex) - (windex + 1)) = string(wint, tt-varaux.cs_ind).
            /* substr(wvar, windex + 1) = string(wint, tt-varaux.cs_ind) + "]". */
          end.
          
          run aux_arvore_progr.p (output wval_var) "assign_var" wvar "-1".
          
          /* se nao houver assign nem init, passa para o proximo */
          if wval_var = "" then next.
          else do:
            create btt-varaux.
            buffer-copy tt-varaux to btt-varaux
              assign btt-varaux.val  = wval_var
                     btt-varaux.seq  = wseq
                     btt-varaux.tipo = "parte".
            /* sempre cria o primeiro valor do array com seq = 0 */
            wseq = wseq + 1.
          end.
        end.
        /* se nao achou assign, verifica se existe init */
        if not can-find(first btt-varaux where btt-varaux.nome  = tt-varaux.nome
                                           and btt-varaux.parte = tt-varaux.parte
                                           and btt-varaux.tipo  = "parte" ) then do:
          /*
          message "Buscando init"
            view-as alert-box INFO buttons ok.                                 
          */
          run aux_arvore_progr.p (output wval_var) "init_var" entry(1, wvar, "[") "-1".
          
          if wval_var <> "" then do:
            create btt-varaux.
            buffer-copy tt-varaux to btt-varaux
              assign btt-varaux.val  = wval_var
                     btt-varaux.seq  = wseq
                     btt-varaux.tipo = "parte".
            /* sempre cria o primeiro valor do array com seq = 0 */
            wseq = wseq + 1.
          end.            
        end.
      end.
      else do:
        /*
        message "Variavel normal"
          view-as alert-box INFO buttons ok.
        */
      
        /* busca valores normais */
        run aux_arvore_progr.p (output wval_var) "assign_var" tt-varaux.val "-1".
        
        /* se nao achou assign, verifica se existe init */
        if wval_var = "" then
          run aux_arvore_progr.p (output wval_var) "init_var" tt-varaux.val "-1".
        
        if wval_var <> "" then do:
          create btt-varaux.
          buffer-copy tt-varaux to btt-varaux
            assign btt-varaux.val  = wval_var
                   btt-varaux.seq  = 0
                   btt-varaux.tipo = "parte".
        end.
      end.    
    end.
  end.
     
  /* monta valores finais */
  do wint2 = 0 to wseq:
      
    create tt-varaux.
    assign tt-varaux.nome  = "final"           
           tt-varaux.seq   = wint2
           tt-varaux.parte = 0
           tt-varaux.tipo  = "final".
  
    do wint = 1 to wparte:
      find first btt-varaux where btt-varaux.tipo  = "parte"
                              and btt-varaux.parte = wint
                              and btt-varaux.seq   = wint2
                                  no-lock no-error.
                                  
      if not avail btt-varaux then do:
        find first btt-varaux where btt-varaux.tipo  = "parte"
                                and btt-varaux.parte = wint
                                and btt-varaux.seq   = 0
                                    no-lock no-error.
      end.
      
      if avail btt-varaux then do:
        /* evita que crie registros finais iguais quando ha mais de uma ocorrencia para tal no fonte */
        if can-find (first tt-varaux where tt-varaux.val  = trim(replace(btt-varaux.val, '"', ''))
                                       and tt-varaux.tipo = "final" ) then
          delete tt-varaux.
        else
          tt-varaux.val = trim(replace(btt-varaux.val, '"', '')).
      end.        
    end.
  end.
  
  for each tt-varaux where tt-varaux.tipo = "final" no-lock:
    wval_var = tt-varaux.val.
    
    if wval_var <> "" then do:
      /*
      message "wval_var: " substr(wval_var, 1, 50)  skip
                           substr(wval_var, 51, 50) skip
        view-as alert-box INFO buttons ok.
      */
      
      i_id = i_id + 1.
      
      create btt-analise.      
      assign btt-analise.id        = i_id
             btt-analise.nivel     = i_nivel
             btt-analise.id_pai    = i_id_pai
             btt-analise.progr_pai = c_prog_pai
             btt-analise.progr     = wval_var
             btt-analise.runvalue  = yes
             btt-analise.e_xref    = no
             btt-analise.escrito   = no.
      
      if entry(1, btt-analise.progr, ".") = entry(1, cprog_fim, ".") then do:
        
        /* Marca que encontrou o programa */
        assign tt-progrs.achou = yes
               l_achou         = yes.
        
        if not l_busca then do:
          message "Programa " + cprog_fim + " esta na estrutura."
            view-as alert-box INFO buttons ok.
        end.
      end.
    end.
  end.
/* busca valor de uma variavel dentro do fonte */
&elseif "{1}" = "assign_var" &then
  hide message no-pause.
  message "montando valor da variavel - Total: " string(time - wini, "hh:mm:ss").

  /* le o fonte */
  assign i_laux     = 0
         windex     = 0
         flag_ponto = yes
         wval_var   = "".
  
  if search("" + c_prog_pai) <> ? then
    wfonte = "" + c_prog_pai.
  else if search("" + c_prog_pai) <> ? then
    wfonte = "" + c_prog_pai.
  else do:
    /* Nao encontrou o fonte */
    message "Fonte nao encontrado"
      view-as alert-box INFO buttons ok.
  end.
  
  input stream sler from value(wfonte).
    repeat:
      i_laux = i_laux + 1.
      if -1         <> int("{3}") and
         i_laux     >= int("{3}") and
         flag_ponto  = yes then leave.
    
      import stream sler unformatted wlinha.
      /* encontrou referencia a variavel, inicia o tratamento */
      if (index(wlinha, "{2}") > 0           and 
          wlinha     matches ("*{2}*=*")     and
          not wlinha matches ("*if*then*")   and
          not wlinha matches ("*when*then*")) or
         flag_ponto = no                      then do:
        /*
        message "assign_var"
          view-as alert-box INFO buttons ok.
          
        message '{*}'
          view-as alert-box INFO buttons ok.
        
        message "wlinha" skip
                wlinha
          view-as alert-box INFO buttons ok.
        */
        
        assign wlinha = trim(replace(wlinha, "assign ", ""))
               windex = index(wlinha, "=").
        
        /*
        message "substr(wlinha, 1, windex) - 1" skip
                substr(wlinha, 1, windex)
          view-as alert-box INFO buttons ok.
        */
       
        if windex > 0 then
          substr(wlinha, 1, windex) = replace(substr(wlinha, 1, windex), " ", "").
        
        /*
        message "substr(wlinha, 1, windex) - 2" skip
                substr(wlinha, 1, windex)
          view-as alert-box INFO buttons ok.
        */
        
        /* pega o valor da variavel - guarda apenas o ultimo */
        if flag_ponto = no then
          wval_var = wval_var + substr(wlinha, windex + 1).
        else
          wval_var = substr(wlinha, windex + 1).
        
        /* remove comentarios */
        if index(wval_var, "/*") > 0 then
          wval_var = trim(entry(1, wval_var, "/*")).
        
        /*
        message "substr(wval_var, length(wval_var))" skip
                substr(wval_var, length(wval_var))
          view-as alert-box INFO buttons ok.
        
        if substr(wval_var, length(wval_var)) <> "." then
          flag_ponto = no.   /* nao achou o fim do assign */
        else
          flag_ponto = yes.  /* achou o fim do assign     */
        */
      end.
    end.
  input stream sler close.
  
  /* remove o ponto do fim, quando houver */
  if wval_var <> "" then do:
    wval_var = trim(wval_var).
    if substr(wval_var, length(wval_var)) = "." then
      wval_var = substr(wval_var, 1, length(wval_var) - 1).
  end.  
  
  valor_var = wval_var.
/* busca valor inicial da variavel */
&elseif "{1}" = "init_var" &then 
  hide message no-pause.
  message "montando valor inicial da variavel - Total: " string(time - wini, "hh:mm:ss").
  /*
  message "init_var"
    view-as alert-box INFO buttons ok.
  */
  /* le o fonte */
  assign i_laux     = 0
         windex     = 0
         flag_ponto = yes
         flag_init  = no
         wval_var   = "".
  
  if search("" + c_prog_pai) <> ? then
    wfonte = "" + c_prog_pai.
  else if search("" + c_prog_pai) <> ? then
    wfonte = "" + c_prog_pai.
  else do:
    /* Nao encontrou o fonte */
    message "Fonte nao encontrado"
      view-as alert-box INFO buttons ok.
  end.
  
  input stream sler from value(wfonte).
    repeat:
      i_laux = i_laux + 1.
      if -1         <> int("{3}") and
         i_laux     >= int("{3}") and
         flag_ponto  = yes then leave.
      
      import stream sler unformatted wlinha.
      
      if (index(wlinha, "{2}") > 0                and 
          wlinha     matches ("def* var* {2} *")) or
         flag_ponto = no                          then do:
        /*
        message "Encontrei definicao"
          view-as alert-box INFO buttons ok.
        */
        if wlinha matches ("*init* *") then
          flag_init = yes.
        /*  
        message "flag_init" flag_init
          view-as alert-box INFO buttons ok.
        */  
        if flag_init then do:
          wval_var = wval_var + trim(wlinha).
          /*
          message wval_var
            view-as alert-box INFO buttons ok.
          */
          /* valida se chegou ao fim do init */
          if substr(wval_var, length(wval_var)) <> "," then
            flag_init = no.
          
          /* remove init */
          wval_var = substr(wval_var, index(wval_var, " ")).
          wval_var = replace(replace(replace(wval_var, '[', ''), ']', ''), '"', '').
          /*
          message wval_var
            view-as alert-box INFO buttons ok.
          */
        end.
        
        if substr(wlinha, length(wlinha)) <> "." then
          flag_ponto = no.   /* nao achou o fim do define */
        else
          assign flag_ponto = yes
                 flag_init  = no.  /* achou o fim do define     */
      end.
    end.
  input stream sler close.
  
  /* remove o ponto do fim, quando houver */
  if wval_var <> "" then do:
    wval_var = trim(wval_var).
    if substr(wval_var, length(wval_var)) = "." then
      wval_var = substr(wval_var, 1, length(wval_var) - 1).
  end.  
  
  valor_var = wval_var.
  
&endif