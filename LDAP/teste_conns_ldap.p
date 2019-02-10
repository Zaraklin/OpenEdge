def var wuser   as char no-undo format "x(30)"   label "Usuario".
def var wexiste as log  no-undo format "Sim/Nao" label "Existe".

form wuser
     with frame finput side-labels title "Consulta Usuarios" width 80.
     
form wuser
     wexiste 
     with frame fcons 10 down title "Consultas" width 80.

repeat:
    wuser = "".
    update wuser with frame finput.
    
    if wuser = "" then
        undo.
    
    run cons_ldap_ativo.p (input wuser, output wexiste).
    
    disp wuser
         wexiste
         with frame fcons.
    down with frame fcons.
end.