/*----------------------------------------------------------------------------*
 * @(#) Programa - arvore_progr.p                                             *
 *                                                                            *  
 *----------------------------------------------------------------------------*/
 
def new shared var cprog_ini  as char no-undo label "Progr inicial"   format "x(20)".
def new shared var cprog_fim  as char no-undo label "Progr buscado" format "x(20)".

def var            dir_sai    as char no-undo.
def var            dir_ras    as char no-undo.
def var            dir_prv    as char no-undo.
def var            dir_saf    as char no-undo.

def var            arq_sai    as char no-undo.
def var            arq_dir    as char no-undo.

def new shared var i_nivel    as int  no-undo init 0.
def new shared var c_prog_pai as char no-undo init "".
def new shared var i_id       as int  no-undo.
def new shared var i_id_pai   as int  no-undo.

def var            i_naux     as int  no-undo.
def var            c_paiaux   as char no-undo.

def new shared var wini       as int  no-undo.
def            var wfim       as int  no-undo.
def new shared var wlinha     as char no-undo.

def new shared var l_achou    as log  no-undo.
def new shared var l_busca    as log  no-undo.
def            var l_escarq   as log  no-undo.

def var            wtipo_aux  as char no-undo.
def var            wpar1_aux  as char no-undo.
def var            wpar2_aux  as char no-undo.

def var            wseta      as char no-undo init "|-------------->".

def var            wtot_arv   as int  no-undo init 0.

def new shared temp-table tt-analise no-undo
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
  
def new shared temp-table tt-progrs no-undo
  field prog_ini as char format "x(50)"
  field achou    as log
  index idx01 as primary unique
        prog_ini.
  
def buffer btt-analise  for tt-analise.
def buffer bftt-analise for tt-analise.

def stream s1.
def stream s2.

assign dir_sai = ""
       dir_ras = ""
       dir_prv = ""
       dir_saf = "".

procedure gera_xref: 

  def input param val    as char no-undo.
  def input param id_pai as int  no-undo.
  def var   vcomp        as char no-undo.
  
  def var dir_comp       as char no-undo.
  def var dir_xref       as char no-undo.
  
  if search("" + entry(1, val, ".") + ".xref") = ? and
     search(""      + entry(1, val, ".") + ".xref") = ? then do:
     
     if search(dir_prv + val) <> ? then
       vcomp = dir_prv + val.
     else if search(dir_saf + val) <> ? then
       vcomp = dir_saf + val.
     else do:
       if not l_busca then do:
         message "Programa " + val + " n�o encontrado!"
           view-as alert-box INFO buttons ok.
       end.
              
       vcomp = "".
     end.
     
     if vcomp <> "" then do:
       assign dir_comp = dir_ras + "tmp/" + val
              dir_xref = dir_ras + "tmp/" + entry(1, val, ".") + ".xref".
     
       compile value(vcomp) /* save into value(dir_comp) */ xref value(dir_xref) /* no-error */. 
     end.     
  end.  
    
  assign c_prog_pai = val
         i_id_pai   = id_pai.
end procedure.

procedure analisa_xref:

  hide message no-pause.
  message "analisa_xref - Total: " string(time - wini, "hh:mm:ss").
  
  def input param val      as char no-undo.
  def var         clinha   as char no-undo.
  def var         dir_xref as char no-undo.
  
  if search("" + entry(1, val, ".") + ".xref") <> ? then
    dir_xref = "" + entry(1, val, ".") + ".xref".
  else if search("" + entry(1, val, ".") + ".xref") <> ? then
    dir_xref = "" + entry(1, val, ".") + ".xref".
  else do:
    dir_xref = dir_ras + "tmp/" + entry(1, val, ".") + ".xref".
  end.
  
  if search(dir_xref) <> ? then do:
    input from value(dir_xref).
      
      repeat:
        import unformatted clinha.
        /* pula linhas de arquivos sem permissao */
        if entry(4, clinha, " ") <> "RUN" then
          next.
        else do:
          i_id = i_id + 1.
          
          if can-find(first btt-analise where btt-analise.id_pai = i_id_pai
                                          and btt-analise.progr  = entry(5, clinha, " ")) then
            next.
                    
          if index(entry(5, clinha, " "), "value(") > 0 then do:
            
            /* tabela enviada como parametro no run value */
            if num-entries(entry(5, clinha, " "), ".") > 1 and
               can-find(first _file where 
                              _file._file-name = 
                                entry(1,replace(replace(replace(replace(replace(entry(5, clinha, " "), "value", ""), ")", ""),"(", ""),'"',''), "trim", ""),".")
                              no-lock) then do:
              assign wtipo_aux = "tabela"
                     wpar1_aux = entry(1,replace(replace(replace(replace(replace(entry(5, clinha, " "), "value", ""), ")", ""),"(", ""),'"',''), "trim", ""),".")
                     wpar2_aux = entry(2,replace(replace(replace(replace(replace(entry(5, clinha, " "), "value", ""), ")", ""),"(", ""),'"',''), "trim", ""),".").
            end.
            /* variavel enviada como parametro no run value */
            else do:
              assign wtipo_aux = "variavel"
                     wpar1_aux = replace(replace(replace(replace(replace(entry(5, clinha, " "), "value", ""), ")", ""),"(", ""),'"',''), "trim", "")
                     wpar2_aux = replace(entry(3, clinha, " "), '"', '').
            end.
            
            /*
            message wtipo_aux
                    wpar1_aux
                    wpar2_aux
                    view-as alert-box INFO buttons ok.
            */
            
            run aux_arvore_progr.p (input rowid(tt-progrs)) wtipo_aux wpar1_aux wpar2_aux.            
          end.
          else do:
            create btt-analise.      
            assign btt-analise.id        = i_id
                   btt-analise.nivel     = i_nivel
                   btt-analise.id_pai    = i_id_pai
                   btt-analise.progr_pai = c_prog_pai
                   btt-analise.progr     = replace(entry(5, clinha, " "), '"', '')
                   btt-analise.runline   = int(replace(entry(3, clinha, " "), '"', ''))
                   btt-analise.runvalue  = no
                   btt-analise.e_xref    = no
                   btt-analise.escrito   = no.
            
            /*
            if index(btt-analise.progr, "value(") > 0 then do:
              assign btt-analise.e_xref = yes.
            end.          
            */
            
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
          /* disp tt-analise with frame f2 down. */
        end.
      end.    
    input close.
  end.
  
  release btt-analise.
end procedure.

procedure pathfinder:
  
  hide message no-pause.
  message "pathfinder - Total: " string(time - wini, "hh:mm:ss").
      
  i_nivel = i_nivel + 1.
  
  for each tt-analise where tt-analise.nivel  = (i_nivel - 1)
                        and tt-analise.e_xref = no 
                            exclusive-lock:
                            
    if can-find(first bftt-analise where bftt-analise.nivel     = (i_nivel - 2)
                                     and bftt-analise.progr_pai = tt-analise.progr 
                                     and bftt-analise.progr     = tt-analise.progr_pai) then
      next.
    
    tt-analise.e_xref = yes.
    run gera_xref(tt-analise.progr, tt-analise.id).
    run analisa_xref(tt-analise.progr).    
  end.
    
  find first tt-analise where tt-analise.nivel  = i_nivel
                          and tt-analise.e_xref = no
                              no-lock no-error.
  if i_nivel > 18 then do:
    if not l_busca then do:
      message "Programa com muitos niveis, favor verificar!"    skip
              "A lista mostrara apenas os primeiros 19 niveis!" skip
        view-as alert-box INFO buttons ok.
      /* debug
      for each tt-analise no-lock:
        disp tt-analise with frame f2 down.
      end.
      */
    end.
        
    return.
  end.  
  else if avail tt-analise then do:
      
    release tt-analise.
    release btt-analise.
    run pathfinder.
  end.
  else
    return.  
end procedure.

procedure tree:

  def input param p_id_pai   as int  no-undo.
  def input param wlin_seta  as char no-undo.  
  def       var   wint       as int  no-undo.
  def       var   wimp_prog  as char no-undo.  
  
  for each tt-analise where tt-analise.id_pai = p_id_pai no-lock:
    
    assign wint      = num-entries(wlin_seta, ";") - 1
           wimp_prog = (if tt-analise.runvalue then "value - " else "") + tt-analise.progr.    
    
    /* wlin_seta = fill(";", tt-analise.nivel). */
    if tt-analise.nivel > wint then
      wlin_seta = wlin_seta + fill(";", tt-analise.nivel - wint).
        
    entry(tt-analise.nivel, wlin_seta, ";") = wseta.
    
    /*
    disp wlin_seta format "x(30)"
         tt-analise.nivel
         wint
         with frame f2 down.
    */
    
    put stream s1 unformatted      
      wlin_seta
      wimp_prog
      ";"
      skip.
    
    find last btt-analise where btt-analise.id_pai = p_id_pai no-lock no-error.
    
    /* nao esta no ultimo registro */
    if avail btt-analise                       and
       rowid(btt-analise) <> rowid(tt-analise) then
      entry(tt-analise.nivel, wlin_seta, ";") = "|".
    else
      entry(tt-analise.nivel, wlin_seta, ";") = "".
      
    run tree(tt-analise.id, wlin_seta).
  end.
end procedure.

procedure buscar:
    
  if trim(cprog_fim) <> "" then
    l_achou = no.
  else
    l_achou = yes.
  
  empty temp-table tt-analise.
  
  /* inicial */ 
  run gera_xref(tt-progrs.prog_ini, 0).
  i_nivel = 1.
  run analisa_xref(tt-progrs.prog_ini).
  
  run pathfinder.

  /* se nao achou o programa, nao escreve o arquivo */
  if l_achou then do:
    l_escarq = yes.
  
    /* reinicia i_nivel */ 
    i_nivel = 1.
    output stream s1 to value (arq_sai) append.
      
      put stream s1 unformatted    
        tt-progrs.prog_ini
        ";"
        skip.
      run tree(0, ";").
      
      find last btt-analise no-lock no-error.
      put stream s1 unformatted
        fill(fill("-",16) + ";", if avail btt-analise then (btt-analise.nivel + 1) else 2)
        skip.
        
    output stream s1 close.
    
    unix silent chmod 777 value (arq_sai) 2> /dev/null.
  end.
end procedure.

repeat:
  assign cprog_ini = ""
         cprog_fim = "".
         
  assign i_id      = 0
         i_nivel   = 0.
         
  assign wtot_arv  = 0.
  
  assign l_escarq  = no.

  empty temp-table tt-progrs.
  empty temp-table tt-analise.
  
  update cprog_ini
         cprog_fim
         with frame f0 width 80 side-labels. 
  
  assign wini    = time
         arq_sai = dir_sai     + + "saida-"  + entry(1, cprog_ini, ".")  +
                   "_"         + entry(1, cprog_fim, ".") + "-"           +
                   string(year(today),"9999") + string(month(today),"99") +
                   string(day(today),"99")    + "-"                       +
                   replace(string(wini,"hh:mm:ss"),":","") + ".csv"
         arq_dir = dir_sai + "ls.ls".

  if trim(cprog_ini) = "" and 
     trim(cprog_fim) = "" then do:
    message "Favor informar ao menos um programa!"
      view-as alert-box INFO buttons ok.
    next.
  end.
  /* lista todos os programas chamados pelo cprog_ini */
  else if trim(cprog_ini) <> "" then do:
    if num-entries(cprog_ini, ".") < 2 or 
       (trim(cprog_fim) <> "" and (num-entries(cprog_fim, ".") < 2)) then do:
      message "Favor informar a extensao do programa!"
        view-as alert-box INFO buttons ok.
      next.
    end.
  
    create tt-progrs.
    assign tt-progrs.prog_ini = cprog_ini.
    
    unix silent rm -f value(arq_sai).
    l_busca = no.
  end.
  /* Busca todos os programas que chamam o cprog_fim */
  else if trim(cprog_ini) =  "" and
          trim(cprog_fim) <> "" then do:
          
    if num-entries(cprog_fim, ".") < 2 then do:
      message "Favor informar a extensao do programa!"
        view-as alert-box INFO buttons ok.
      next.
    end.
    
    unix silent rm -f value(arq_sai).
    unix silent rm -f value(arq_dir).
    
    unix silent ls value("*.p >> " + arq_dir).    
    
    input stream s2 from value(arq_dir).
      repeat:
        import stream s2 unformatted wlinha.
        
        if not can-find(first tt-progrs where 
                              tt-progrs.prog_ini = entry(5, wlinha, "/")) then do:
          create tt-progrs.
          assign tt-progrs.prog_ini = entry(5, wlinha, "/").
        end.
      end.
    input stream s2 close.
    
    unix silent rm -f value(arq_dir).
    l_busca = yes.
  end.
  
  output stream s1 to value (arq_sai) append.
      
    /* cabecalho padrao */
    put stream s1 unformatted
      string(today,"99/99/9999")
      " " string(time,"hh:mm:ss")      
      " Prog ini: " cprog_ini
      " Prog busca: " cprog_fim
      skip(1).
  output stream s1 close.
  
  for each tt-progrs exclusive-lock:
    run buscar.
  end.
  
  output stream s1 to value (arq_sai) append.
    if l_busca then do:    
      for each tt-progrs where tt-progrs.achou no-lock:
        wtot_arv = wtot_arv + 1.
      end.
    end.
    else
      wtot_arv = 1.
    
    put stream s1 unformatted
      string(wtot_arv) " Arvores de Programas escritas"
      skip.
  output stream s1 close.
  
  /* se nao achou o programa, apaga o arquivo de saida */
  if not l_escarq then
    unix silent rm -f value(arq_sai).
  
  wfim = time.
  
  /*
  if l_busca then do:    
    for each tt-progrs where tt-progrs.achou no-lock:
      disp tt-progrs.prog_ini no-label
        with frame f2 width 80 title " Progr encontrados ".      
    end.
  end.
  */
  
  if search(arq_sai) = ? then
    if trim(cprog_fim) <> "" then do:
      if trim(cprog_ini) <> "" then
        message "Programa procurado nao e chamado pelo programa " cprog_ini "!"
          view-as alert-box INFO buttons ok.
      else
        message "Programa procurado nao e chamado por ninguem!"
          view-as alert-box INFO buttons ok.
    end.
      
    else
      message "Nao foi possivel gerar a estrutura do programa!"
        view-as alert-box INFO buttons ok.
  else
    disp (arq_sai) format "x(71)" label "Saida"
      with frame fsai side-labels width 80 row 18 overlay.
      
  hide message no-pause.
  message "Fim do Processamento. " (i_id + 1) " programas lidos em " string(wfim - wini, "hh:mm:ss").
  
  pause.
end.

{log_fim.i}