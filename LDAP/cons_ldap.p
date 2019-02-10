/***********************************************************************
 *      Programa: cons_ldap.p                                          *
 *        Funcao: recebe um usuario e verifica se                      *
 *                ele existe na AD utilizando LDAP                     *
 *    Baseado em: https://community.progress.com/community_groups/     *
 *                openedge_general/w/openedgegeneral/                  *
 *                1473.ldap-user-authentication-in-an-abl-environment  *
 *                                                                     *
 *         Fatos: usuario habilitado/desabilitado                      *
 *                atributo: useraccountcontrol                         *
 *                   valor: 512   = Enabled                            *
 *                          514   = Disabled                           *
 *                          66048 = Enabled, password never expires    *
 *                          66050 = Disabled, password never expires   *
 *                                                                     *
 *                                                                     * 
 ***********************************************************************/
 
def input  param puser   as char format "x(10)"   no-undo.
def output param pexiste as log  format "Sim/Nao" no-undo.

/*
** Unix LDAP servers will typically use the "uid" attribute to hold the
**    simple user-id name.
** Active Directory LDAP servers will typcially use the "cn" attribute to hold
**    the simple user-id namme. 
*/
def var m_cHomeDN as character initial "CN=XX,OU=XX,DC=XX,DC=XX" no-undo.
def var m_cADDN   as character initial "CN=XX,OU=XX,DC=XX,DC=XX" no-undo.

/*
** root DNs to begin searching an LDAP directory so that when a simple
** user-id name is entered, we can look up the user's fully qualified DN (which
** is what needs to be used for authentication)
*/
def var m_cHomeRoot as char initial "DC=XX,DC=XX" no-undo.
def var m_cADRoot   as char initial "DC=XX,DC=XX" no-undo.
def var m_cAuthProc as char initial "UnixLDAPAuth64.p" no-undo.

/* 
** Define a couple of  LDAP servers.  
*/
def var m_cHomeServer as char initial "192.168.1.1" no-undo.
def var m_cADServer   as char initial "myad.com" no-undo.

/* 
** The LDAP user authentication options may be inserted here.  Refer to the
** header comments in either WinLdapAuth.p or UnixLdapAuth.p for a full 
** description of which options are supported. 
*/
def var m_cOptions as char initial "-groups -attrs cn,mail,sn,memberOf,useraccountcontrol" no-undo.

/*
** Some operational procedure variables.
*/
def var m_cbindpwd       as char                no-undo.
def var m_cuserattrs     as char                no-undo.
def var m_cbindname      as char                no-undo.
def var m_cbindserver    as char                no-undo.
def var m_csearchroot    as char                no-undo.
def var m_csearchbinddn  as char                no-undo.
def var m_csearchbindpwd as char initial ""     no-undo.
def var m_cgroups        as char initial ""     no-undo.
                                                
def var m_hldap          as handle              no-undo.

def var wattr            as char format "x(50)" no-undo.
def var wgrp             as char format "x(50)" no-undo.

on close of this-procedure do:

end.

if (opsys <> "UNIX" ) then do:
    m_cAuthProc   = "WinLDAPAuth.p".
    m_cBindServer = m_cADServer.
    m_cSearchRoot = m_cADRoot.
    
    /* 
    ** Here is the fully qualified LDAP user DN that needs to be used when the
    ** LDAP server's security is setup so that we cannot search for a user account
    ** without previously binding with an authorized LDAP account 
    */
    m_cSearchBindDN = m_cADDN.
end.
else do:
    m_cAuthProc   = "UnixLDAPAuth64.p".
    m_cBindServer = m_cHomeServer.
    m_cSearchRoot = m_cHomeRoot.
    
    /* 
    ** Here is the fully qualified LDAP user DN that needs to be used when the
    ** LDAP server's security is setup so that we cannot search for a user account
    ** without previously binding with an authorized LDAP account 
    */
    m_cSearchBindDN = m_cHomeDN.
end.

/* usuario a ser validado */
m_cBindName = puser.

/* senha do usuario p/ efetuar buscas no AD */
m_cSearchBindPwd = "XXXXXXX".

run value(m_cAuthProc) persistent set m_hLDAP (input m_cBindServer,    /* bind server */
                                               input ?,                /* default port */
                                               input m_cSearchRoot,    /* user search root*/
                                               input ?,                /* default user filter */
                                               input m_cSearchRoot,    /* group search root*/
                                               input ?,                /* default group filter */
                                               input m_cSearchBindDN,  /* Search user bind DN */
                                               input m_cSearchBindPwd, /* Search user bind Pwd */
                                               input m_cOptions        /* Misc. options */
                                               ) /* NO-ERROR */.
                                               
/*
** Evaluate whether the authentication passed or failed, and if it passed dump
** out the user's access token information.
*/
if (valid-handle( m_hLDAP )) then do:
    if ( return-value <> "" ) then do:
        message "ERROR starting LDAP authentication object: " + return-value view-as alert-box.
        quit.
    end.
    
    message "inicializando consult"
      view-as alert-box INFO buttons ok.
    
    wattr = "".
    run consult in m_hLDAP (input m_cBindName, output wattr, output wgrp) no-error.
                                 
    if (return-value = "") then do:
        display substr(wattr,   1, 50) format "x(50)"    label "Attr" colon 5 skip
                substr(wattr,  51, 50) format "x(50)" no-label        colon 5 skip
                substr(wattr, 101, 50) format "x(50)" no-label        colon 5 skip
                substr(wattr, 151, 50) format "x(50)" no-label        colon 5 skip
                substr(wattr, 201, 50) format "x(50)" no-label        colon 5 skip
                substr(wattr, 251, 50) format "x(50)" no-label        colon 5 skip
                wgrp                                     label "Grp" 
                with frame f2 side-labels.
    
        pexiste = yes.
    end.
    else do:
        pexiste = no.
    end.
    
    run finalize in m_hLDAP /* NO-ERROR */.
    
    if (valid-handle(m_hLDAP)) then do:
        delete procedure m_hLDAP.
    end.
end.
else do:
    message "ERROR starting LDAP authentication object" view-as alert-box.
end.