/*------------------------------------------------------------------------------
    File        : UnixLdapAuth64.p 
    Purpose     : Authenticate a user to a (pure) LDAP V3 directory service on a
                  Windows 2000, or later, system.  Windows Domain logins via
                  ActiveDirectory are not supported in this object.

                  DANGER:  This sample sends user account passwords in 
                           CLEAR-TEXT!!!  This is not suitable in most
                           production applications.

    Syntax      : RUN UnixLdapAuth64.p PERSISTENT SET m_hLDAPAuth
                                            ( INPUT p_cBindServer,
                                              INPUT p_cBindPort,
                                              INPUT p_cUserSearchRootDN,
                                              INPUT p_cUserSearchFilter,
                                              INPUT p_cGroupSearchRootDN,
                                              INPUT p_cGroupSearchFilter,
                                              INPUT p_cSearchBindDN,
                                              INPUT p_cSearchBindPwd,
                                              INPUT p_cOptions ) NO-ERROR.

                   RUN authenticate IN m_hLDAPAuth ( INPUT  p_cUser,
                                                     INPUT  p_cPassword,
                                                     OUTPUT p_cRetAttrList,
                                                     OUTPUT p_cRetGroupList)
                                                     NO-ERROR.
                   IF ( "" = RETURN-VALUE ) THEN
                        / * success * /
                   ELSE
                        / * failure * /

                   RUN finalize IN m_hLDAPAuth.

    Description :
              p_cBindServer   A required character DNS name of the LDAP V3 server

              p_cBindPort     An optional character TCP port number of the the
                              LDAP server.  If UNKNOWN or blank, the default 389 will
                              be used

              p_cUserSearchRootDN A required DN of where to search for unqualified
                              user-ids.  

              p_cUserSearchFilter An optional LDAP search filter that will be
                              used to find user accounts by simple-name

              p_cGroupSearchRootDN An optional DN of where to search for 
                              user account group memberships.  If specified as
                              ? or "", no groups search will be performed.

              p_cGroupSearchFilter An optional LDAP search filter that will be
                              used to find user account memberships.
                              The default is:
                                groupOfNames where member = user account DN
                                OR
                                groupOfUniqueNames where uniqueMember = user account DN


              p_cSearchBindDN An optional LDAP (fully qualified) user DN 
                              used to when searching the directory to resolve
                              a simple user-id for authentication.  When the
                              LDAP direcotry security is set, anonymous searches
                              fail (sometimes silently).  When this is the case
                              supply a fully qualified user DN that has the
                              privileges to search the directory sub-tree for
                              user-id entries.

              p_cSearchBindDN An optional password string for the p_cSearchBindDN
                              parameter.  If not specified an empty/blank string
                              will be used.  
            
              p_cUserAttrList An optional character string that holds a comma
                              delimited list of the user account attributes
                              to return from a successful user authentication.
                              
                              ?                 No attributes returned
                              ""                No attributes returned
                              "*"               Return all user attributes
                              "cn,description"  Return only the cn and descritpion
                                                attributes

              p_cOptions      An optional character string that holds optional
                              parameters for LDAP binding, searching, and 
                              authentication.

                              option        value      Desc
                              ==================================================
                              -ssl          n/a        Enable SSL connections

                              -debug        n/a        Enable debug output to 
                                                       the terminal

                              -AD           n/a        Perform an Active Directory
                                                       compatible user 
                                                       authentication

                              -attrs        attr[,attr...]
                                                       Return user account 
                                                       attributes.

                                                       *        Return all

                                                       x,y,     Return only 
                                                                specified
                                                                attributes

                              -groups       n/a        Return group memberships
                                                       in comma delimited list
                                                       (Requires 
                                                        p_cGroupSearchRootDN
                                                        parameter )

                              -ldapoptXXX   optvalue   Set ldap opt number XXX
                                                       to <value>.

                                                       If the value is a YES/NO
                                                       a boolean option will be 
                                                       set.
                                                       
                                                       A value that starts with
                                                       a number 0-9 will set a
                                                       numeric option value.
    
                                                       Any other value will set
                                                       an character string value.
    Author(s)   :
    Created     :
    Notes       :   This module supports only 64 bit hardware/OS
  ----------------------------------------------------------------------------*/
  
/* ********************  Call Parameter Definitions  ************************ */
DEFINE INPUT PARAMETER p_cBindServer            AS CHARACTER NO-UNDO.   
DEFINE INPUT PARAMETER p_cBindPort              AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p_cUserSearchRootDN      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p_cUserSearchFilter      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p_cGroupSearchRootDN     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p_cGroupSearchFilter     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p_cSearchBindDN          AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p_cSearchBindPwd         AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p_cOptions               AS CHARACTER NO-UNDO.


/* *********************  Module Variable Definitions  ********************** */
DEFINE VARIABLE m_cRetValue             AS CHARACTER INITIAL "" NO-UNDO.
DEFINE VARIABLE m_cBindServer           AS CHARACTER NO-UNDO.   
DEFINE VARIABLE m_cBindPort             AS CHARACTER INITIAL "3268" /* "389" */ NO-UNDO.
DEFINE VARIABLE m_cUserSearchRootDN     AS CHARACTER INITIAL "" NO-UNDO.
DEFINE VARIABLE m_cUserSearchFilter     AS CHARACTER INITIAL "(&(objectclass=user)(sAMAccountName=%s))" /* "(&(objectclass=user)(sAMAccountName=%s))" */ /* "(&(objectclass=person)(|(cn=%s)(uid=%s)))" */ NO-UNDO.
DEFINE VARIABLE m_cSearchBindDN         AS CHARACTER INITIAL "" NO-UNDO.
DEFINE VARIABLE m_cSearchBindPWD        AS CHARACTER INITIAL "" NO-UNDO.
DEFINE VARIABLE m_cGroupSearchRootDN    AS CHARACTER INITIAL "" NO-UNDO.
DEFINE VARIABLE m_cGroupSearchFilter    AS CHARACTER INITIAL "(|(&(objectclass=groupOfUniqueNames)(cn=%s))(&(objectclass=groupOfNames)(member=%s)))" NO-UNDO.
DEFINE VARIABLE m_cADGroupSearchFilter  AS CHARACTER INITIAL "(&(objectclass=group)(member=%s))" NO-UNDO.
DEFINE VARIABLE m_cAttrList             AS CHARACTER NO-UNDO.

DEFINE VARIABLE m_mLDAPContext          AS MEMPTR NO-UNDO.
DEFINE VARIABLE m_mOSNullPointer        AS MEMPTR NO-UNDO.
DEFINE VARIABLE m_mLastLDAPError        AS MEMPTR NO-UNDO.

DEFINE VARIABLE m_iOSPointerSize        AS INTEGER INITIAL 8 NO-UNDO.
DEFINE VARIABLE m_iEnableSSL            AS INTEGER INITIAL 0 NO-UNDO.
DEFINE VARIABLE m_iAuthMethod           AS INTEGER INITIAL 0 NO-UNDO.

DEFINE VARIABLE m_lConnectComplete      AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE m_lDebug                AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE m_lTrace                AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE m_lBindComplete         AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE m_lSearchGroups         AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE m_lADCompatible         AS LOGICAL INITIAL NO NO-UNDO.

/* TESTE 
assign m_lDebug = yes
       m_lTrace = yes.
*/
/* ***********************  Preprocessor Definitions  *********************** */
&SCOPED-DEFINE LDAP_SCOPE_BASE 0
&SCOPED-DEFINE LDAP_SCOPE_ONELEVEL 1
&SCOPED-DEFINE LDAP_SCOPE_SUBTREE 2

&SCOPED-DEFINE LDAP_OPT_SSL 0xa 
&SCOPED-DEFINE LDAP_OPT_VERSION 0x11
&SCOPED-DEFINE LDAP_OPT_AREC_EXCLUSIVE 0x98

&SCOPED-DEFINE LDAP_OPT_ON 1
&SCOPED-DEFINE LDAP_OPT_OFF 0

&SCOPED-DEFINE LDAP_VERSION2 2
&SCOPED-DEFINE LDAP_VERSION3 3

&SCOPED-DEFINE LDAP_AUTH_SIMPLE 0x80
&SCOPED-DEFINE LDAP_AUTH_NEGOTIATE 0x486
&SCOPED-DEFINE LDAP_AUTH_DIGEST 0x4086

&SCOPED-DEFINE LDAP_OPT_ERROR_NUMBER 0x31

&SCOPED-DEFINE MAX_ATTR_SIZE 2048


/* Include the Properties object to manage options */
{properties.i m_hProps }

/* ************************* Procedure Settings ***************************** */

/******************************************************************************/
/******************************************************************************/
/*********                      DESTRUCTOR                             ********/
/******************************************************************************/
/******************************************************************************/
ON CLOSE OF THIS-PROCEDURE DO:
    RUN finalize NO-ERROR.
END.

/******************************************************************************/
/******************************************************************************/
/*********               MAIN  (CONSTRUCTOR)                           ********/
/******************************************************************************/
/******************************************************************************/

RUN defaultConstructor( INPUT  p_cBindServer,
                        INPUT  p_cBindPort,
                        INPUT  p_cUserSearchRootDN,
                        INPUT  p_cUserSearchFilter,
                        INPUT  p_cUserSearchRootDN,
                        INPUT  p_cUserSearchFilter,
                        INPUT  p_cSearchBindDN,
                        INPUT  p_cSearchBindPwd,
                        INPUT  p_cOptions ) NO-ERROR.
m_cRetValue = RETURN-VALUE.
RETURN m_cRetValue.

/******************************************************************************/
/******************************************************************************/
/* ********************  INTERNAL PROCEDURES/FUNCTIONS ********************** */
/******************************************************************************/
/******************************************************************************/


/*
** PROGRAM     : Consult
** DESCRIPTION : Consult a user-id. If successful, optionally
**               return account information.
**      
** PARAMETERS  :
**   p_cUser                The user id to Consult.  Two formats may be 
**                          used: qualified and unqualified.
**                          Unqualified: specify the simple user account name
**                              without X.500 formating
**                          Qualified: specify the exact X.500 fully qualified
**                              distinguished name.  The distinguished name
**                              is signaled by the prefix "uid=" or "cn="
**
**
**
** SIDE EFFECTS:
**      none.
** RETURN-VALUE: "" for success, otherwise returns the failure reason.
*/
PROCEDURE consult :
    /* Procedure input/output parameter definitions. */
    DEFINE INPUT  PARAMETER p_cUser AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER p_attr  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER p_grp   AS CHARACTER NO-UNDO.
    
    /* Procedure local variables */
    DEFINE VARIABLE cRetValue       AS CHARACTER INITIAL "".
    DEFINE VARIABLE iStatus         AS INTEGER NO-UNDO.
    DEFINE VARIABLE cUserDN         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lDone           AS LOGICAL INITIAL NO NO-UNDO.
    DEFINE VARIABLE cGroups         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAttrs          AS CHARACTER NO-UNDO.

    IF (m_lTrace) THEN 
        MESSAGE "trace: consulting user " + p_cUser view-as alert-box.

    /* Don't try and run if the minimum required parameters are not set */
    IF ( ? <> p_cUser AND
        "" <> p_cUser ) THEN DO WHILE (NOT lDone ) :
        lDone = YES.

        /* Get which attributes to return. */
        cAttrs = m_cAttrList.

        /* Next decide whether we need to search for a fully qualified DN for
         * binding the user (authentication) or the input is already a
         * fully qualified DN so use it. */
        IF ( SUBSTRING(p_cUser, 1, 4) = "uid=" OR
             SUBSTRING(p_cUser, 1, 3) = "cn=" ) THEN DO:
             /* The input user id appears to be a fully qualified DN, so we'll
              * use it as-is */
             cUserDN = p_cUser.
        END. ELSE DO:
            /* The input user id does not appear to be an X.500 name form.  So
             * we'll go search for a user account in the registry. */
            IF (m_lDebug) THEN 
                MESSAGE "debug: resolving simple user-id DN: " + p_cUser view-as alert-box.
        
            RUN resolveUserDN ( INPUT  p_cUser,
                                OUTPUT cUserDN ) NO-ERROR.
            IF ( "" <> RETURN-VALUE ) THEN DO:
                /* cannot find the user.  issue an error and return */
                cRetValue = "Cannot find a LDAP user account for: " + p_cUser +
                    " (" + RETURN-VALUE + ")".
                LEAVE.
            END.
            
            /* Now see if we should load user account attributes */
            IF ( "" <> cAttrs) THEN DO:
                RUN loadAccountAttributes( INPUT        cUserDN,
                                           INPUT-OUTPUT cAttrs) NO-ERROR.
                /* return the error results */
                IF ( "" <> RETURN-VALUE ) THEN DO:
                    cRetValue = RETURN-VALUE.                
                    LEAVE.
                END.
                
                p_attr = cAttrs.
            END.


            /* Now see if we should load user account group memberships */
            IF ( m_lSearchGroups AND 
                 "" <> m_cGroupSearchRootDN ) THEN DO:
                RUN resolveMemberships( INPUT   cUserDN,
                                        OUTPUT  cGroups) NO-ERROR.
                /* return the error results */
                IF ( "" <> RETURN-VALUE ) THEN DO:
                    cRetValue = RETURN-VALUE.                
                END.
                
                p_grp = cGroups.
            END.
        END.
    END. ELSE DO:
        cRetValue = "Missing consult input parameter".
    END.
    
    RETURN cRetValue.
END PROCEDURE.


/*
** PROGRAM     : Authenticate
** DESCRIPTION : Authenticate a user-id and password.  If successful, optionally
**               return account information.
**      
** PARAMETERS  :
**   p_cUser                The user id to authenticate.  Two formats may be 
**                          used: qualified and unqualified.
**                          Unqualified: specify the simple user account name
**                              without X.500 formating
**                          Qualified: specify the exact X.500 fully qualified
**                              distinguished name.  The distinguished name
**                              is signaled by the prefix "uid=" or "cn="
**
**   p_cPassword            The [clear-text] user account password
**
**   p_cAttributes          An output parameter where any user account
**                          attrubutes are returns.  Attributes are returned
**                          in the form:
**                          attrName=attrValue[&attrName=attrValue ... ]
**                          A blank string may be returned.
**
**   p_cGroups              An output parameter where any user account
**                          group memberships are returned.  The groups are
**                          returned in the form:
**                          group[,group ... ]
**                          A blank string may be returned.
**
** SIDE EFFECTS:
**      none.
** RETURN-VALUE: "" for success, otherwise returns the failure reason.
*/
PROCEDURE authenticate :
    /* Procedure input/output parameter definitions. */
    DEFINE INPUT PARAMETER      p_cUser               AS CHARACTER NO-UNDO.    
    DEFINE INPUT PARAMETER      p_cPassword           AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER     p_cAttributes         AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER     p_cGroups             AS CHARACTER NO-UNDO.

    /* Procedure local variables */
    DEFINE VARIABLE cRetValue           AS CHARACTER INITIAL "".
    DEFINE VARIABLE iStatus             AS INTEGER NO-UNDO.
    DEFINE VARIABLE cUserDN             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lDone               AS LOGICAL INITIAL NO NO-UNDO.
    DEFINE VARIABLE cGroups             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAttrs              AS CHARACTER NO-UNDO.

    IF (m_lDebug) THEN 
        MESSAGE "debug: Authenticate using 64 bit archtitecture".
        
    IF (m_lTrace) THEN 
        MESSAGE "trace: authenticating user " + p_cUser.

    /* Don't try and run if the minimum required parameters are not set */
    IF ( ? <> p_cUser AND
        "" <> p_cUser AND
        ? <> p_cPassword ) THEN DO WHILE (NOT lDone ) :
        lDone = YES.

        /* Get which attributes to return. */
        cAttrs = m_cAttrList.

        /* Next decide whether we need to search for a fully qualified DN for
         * binding the user (authentication) or the input is already a
         * fully qualified DN so use it. */
        IF ( SUBSTRING(p_cUser, 1, 4) = "uid=" OR
             SUBSTRING(p_cUser, 1, 3) = "cn=" ) THEN DO:
             /* The input user id appears to be a fully qualified DN, so we'll
              * use it as-is */
             cUserDN = p_cUser.
        END. ELSE DO:
            /* The input user id does not appear to be an X.500 name form.  So
             * we'll go search for a user account in the registry. */
            IF (m_lDebug) THEN 
                MESSAGE "debug: resolving simple user-id DN: " + p_cUser.
        
            RUN resolveUserDN ( INPUT  p_cUser,
                                OUTPUT cUserDN ) NO-ERROR.
            IF ( "" <> RETURN-VALUE ) THEN DO:
                /* cannot find the user.  issue an error and return */
                cRetValue = "Cannot find a LDAP user account for: " + p_cUser +
                    " (" + RETURN-VALUE + ")".
                LEAVE.
            END.
        END.

        /* Next do the LDAP bind, it's what does the user authentication.
        **
        ** NOTE: this is using the simple bind, which sends clear-text passwords.
        **       Most auditors will NOT accept a product that does this */
        IF (m_lTrace) THEN 
            MESSAGE "trace: Authenticating user DN : " + cUserDN.
    
        RUN ldap_bind_s  ( INPUT  m_mLDAPContext,
                           INPUT  cUserDN,
                           INPUT  p_cPassword,
                           INPUT  m_iAuthMethod,
                           OUTPUT iStatus ) /* NO-ERROR */.
        IF (m_lDebug) THEN 
            MESSAGE "debug: LDAP simple bind returned " + 
                    " : " + STRING(iStatus).

        IF ( 0 <> iStatus ) THEN DO:
            RUN ldap_err2string( INPUT  iStatus, OUTPUT m_mLastLDAPError ) /* NO-ERROR */.
            cRetValue = GET-STRING(m_mLastLDAPError,1).
            LEAVE.
        END.

        /* Remember that we have a bound LDAP directory connection */
        m_lBindComplete = YES.

        /* See if we need to do Active Directory compatibility mode ( account
         * disable and locked status checking) */
        IF ( m_lADCompatible ) THEN DO:
            RUN resolveADStatus( INPUT cUserDN ) /* NO-ERROR */.
            IF ( "" <> RETURN-VALUE ) THEN DO:
                /* An error occured, so pass along the error as failing 
                 * user authenticaiton */
                cRetValue = RETURN-VALUE.
                LEAVE.
            END.
        END.

        /* Now see if we should load user account attributes */
        IF ( "" <> cAttrs) THEN DO:
            RUN loadAccountAttributes( INPUT        cUserDN,
                                       INPUT-OUTPUT cAttrs) NO-ERROR.
            /* return the error results */
            IF ( "" <> RETURN-VALUE ) THEN DO:
                cRetValue = RETURN-VALUE.                
                LEAVE.
            END.
        END.


        /* Now see if we should load user account group memberships */
        IF ( m_lSearchGroups AND 
             "" <> m_cGroupSearchRootDN ) THEN DO:
            RUN resolveMemberships( INPUT   cUserDN,
                                    OUTPUT  cGroups) NO-ERROR.
            /* return the error results */
            IF ( "" <> RETURN-VALUE ) THEN DO:
                cRetValue = RETURN-VALUE.                
            END.
        END.
    END. ELSE DO:
        cRetValue = "Missing authentication input parameter".
    END.

    /* We do not want to keep an LDAP connection open where it may be
     * exploited by a hacker (tool), so close it down. */
    IF ( m_lBindComplete ) THEN DO:
        IF (m_lTrace) THEN 
            MESSAGE "trace: Unbinding from LDAP server".
        RUN ldap_unbind_s ( INPUT  m_mLDAPContext,
                            OUTPUT iStatus) NO-ERROR.
        IF ( 0 <> iStatus ) THEN DO:
            RUN ldap_err2string( INPUT  iStatus, OUTPUT m_mLastLDAPError ) /* NO-ERROR */.
            cRetValue = "Failed unbind operation after user authentication: " +
                        GET-STRING(m_mLastLDAPError,1).
        END.
        /* Remember that we do not have a bound connection to the LDAP 
         * directory any more. */
        m_lBindComplete = NO.
    END.

    /* Return the output parameters results */
    p_cAttributes = cAttrs.
    p_cGroups = cGroups.

    RETURN cRetValue.
END PROCEDURE.

/*
** PROGRAM     : LdapGetLastError
** DESCRIPTION : Get the last error code set by the LDAP library action.
**      
** PARAMETERS  :
**      p_iError        An output integer parameter that holds the last
**                      error encountered by the LDAP library.
** SIDE EFFECTS:
**      none.
** RETURN-VALUE: "" for success, otherwise returns the failure reason.
*/
PROCEDURE LdapGetLastError :
    /* Procedure input/output parameter definitions. */
    DEFINE OUTPUT PARAMETER p_iStatus   AS INTEGER NO-UNDO.

    /* Procedure local variables */
    DEFINE VARIABLE iStatus             AS INTEGER INITIAL 0.
    DEFINE VARIABLE mInfo               AS MEMPTR NO-UNDO.
    
    SET-SIZE(mInfo) = 4.
    RUN ldap_get_option ( INPUT  m_mLDAPContext,
                          INPUT  {&LDAP_OPT_ERROR_NUMBER},
                          OUTPUT mInfo,
                          OUTPUT iStatus) /* NO-ERROR */.
    p_iStatus = GET-LONG(mInfo,1).
    SET-SIZE(mInfo) = 0.

    RETURN "".
END PROCEDURE.

/*
** PROGRAM     : resolveUserDN
** DESCRIPTION : Search the directory for a person object that has a
**      cn or uid attribute that matches the user's id.  If found, return the
**      full DN.  If more than one person is found, the user-id is ambiguous
**      and is an error.
**      
** PARAMETERS  :
**      p_cUserId       The simple user-id of the account to search for.
**      p_cReturnUserDN The location of where to return the fully qualified
**                      user X.500 DN.
**
** SIDE EFFECTS:
**      Can leave the LDAP directory bound to the user account search bind
**      user account.
** RETURN-VALUE: "" for success, otherwise returns the failure reason.
*/
PROCEDURE resolveUserDN PRIVATE :
    /* Procedure input/output parameter definitions. */
    DEFINE INPUT  PARAMETER p_cUserId           AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER p_cReturnUserDN     AS CHARACTER NO-UNDO.

    /* Procedure local variables */
    DEFINE VARIABLE cRetValue               AS CHARACTER INITIAL "".
    DEFINE VARIABLE iStatus                 AS INTEGER NO-UNDO. 
    DEFINE VARIABLE iBindStatus             AS INTEGER NO-UNDO. 
    DEFINE VARIABLE lDone                   AS LOGICAL INITIAL NO NO-UNDO.
    DEFINE VARIABLE lFreeEntry              AS LOGICAL INITIAL NO NO-UNDO.
    DEFINE VARIABLE lEntry                  AS INT64 NO-UNDO.
    DEFINE VARIABLE mRes                    AS MEMPTR NO-UNDO.
    DEFINE VARIABLE mDN                     AS MEMPTR NO-UNDO.
    DEFINE VARIABLE mLDAPMsg                AS MEMPTR NO-UNDO.
    DEFINE VARIABLE mEntry                  AS MEMPTR NO-UNDO.
    DEFINE VARIABLE iReturns                AS INTEGER INITIAL 0 NO-UNDO.
    DEFINE VARIABLE cUserFilter             AS CHARACTER NO-UNDO. 

    IF (m_lTrace) THEN 
        MESSAGE "trace: resolving user account DN for " + p_cUserId.

    IF ( ? <> p_cUserId AND
         "" <> p_cUserId ) THEN DO WHILE ( NOT lDone ):
         lDone = YES.

         /* Need to grab a bunch of memory for shared library memory pointers */
         SET-SIZE(mRes) = m_iOSPointerSize.
         PUT-BYTES(mRes, 1) = m_mOSNullPointer.

         /* Setup the user object search filter by inserting the simple
          * user account name into the filter string. */
         cUserFilter = REPLACE(m_cUserSearchFilter, "%s", p_cUserId).

         /* Sometimes LDAP security doesn't allow an anonymous bind to search.
          * In these instances, a specific user's (fully qualified) DN must
          * be used.  If specified, use the DN to bind to the directory to do
          * the search, and then unbind.  If the bind fails, so will resolving
          * the simple user-id */
         IF ( "" <> m_cSearchBindDN ) THEN DO:
             IF (m_lTrace) THEN 
                 MESSAGE "trace: Binding search user: " + m_cSearchBindDN.
         
             RUN ldap_bind_s  ( INPUT  m_mLDAPContext,
                                INPUT  m_cSearchBindDN,
                                INPUT  m_cSearchBindPwd,
                                INPUT  m_iAuthMethod,
                                OUTPUT iBindStatus ) /* NO-ERROR */.
             IF (m_lDebug) THEN 
                 MESSAGE "debug: LDAP search bind returned " + 
                         " : " + STRING(iBindStatus).
     
             IF ( 0 <> iBindStatus ) THEN DO:
                 RUN ldap_err2string( INPUT  iBindStatus, 
                                       OUTPUT m_mLastLDAPError ) /* NO-ERROR */.
                 cRetValue = "Error binding to resolve simple user-id: " + 
                             GET-STRING(m_mLastLDAPError,1).
                 LEAVE.
             END.
             m_lBindComplete = YES.
        END.

        IF (m_lTrace) THEN 
            MESSAGE "trace: searching for user-id " + p_cUserId + " using " +
                    "root " + m_cUserSearchRootDN + " and filter " +
                    cUserFilter.


        /* Search the directory starting at the root DN specified tree node
         * and search that level and all sub-tree levels.  Do not return
         * any user account attributes */
        RUN ldap_search_s  ( INPUT  m_mLDAPContext,
                             INPUT  m_cUserSearchRootDN,
                             INPUT  {&LDAP_SCOPE_SUBTREE},
                             INPUT  cUserFilter,
                             INPUT  m_mOSNullPointer,
                             INPUT  1,               /* do not return attributes */
                             OUTPUT mRes,
                             OUTPUT iStatus).


        IF (m_lDebug) THEN 
            MESSAGE "debug: LDAP search returned " + 
                    " : " + STRING(iStatus).

        IF ( 0 <> iStatus ) THEN DO:
            /* not found... */
            RUN ldap_err2string( INPUT  iStatus, OUTPUT m_mLastLDAPError ) /* NO-ERROR */.
            cRetValue = "Cannot resolve user account for " + p_cUserId + " : " + 
                        GET-STRING(m_mLastLDAPError,1).
            LEAVE.
        END.

        /* Remember to free the LDAP entry message on procedure exit. */
        lFreeEntry = YES.

        /* Get the memory pointer to the LDAPMessage structure returned by 
         * the search */
        SET-POINTER-VALUE(mLDAPMsg) = GET-INT64(mRes,1).

        /* Now check to see that we got exactly one directory entry hit.  If 
        ** we get more than one, the simple user-id is ambiguous because it
        ** exists in more than one sub-tree of the search, and an error*/
        IF (m_lTrace) THEN 
            MESSAGE "trace: determining number of LDAP matches for user-id " + 
                    p_cUserId.

        RUN ldap_count_entries( INPUT  m_mLDAPContext,
                                INPUT  mLDAPMsg,
                                OUTPUT iReturns ) /* NO-ERROR */.

        IF (m_lTrace) THEN 
            MESSAGE "trace: number of LDAP matches for user-id is " + 
                    STRING(iReturns).

        IF ( -1 = iReturns ) THEN DO:
            cRetValue = "Error resolving user-id DN".
            LEAVE.
        END. 

        IF ( 0 = iReturns ) THEN DO:
            cRetValue = "User account not found".
            LEAVE.
        END.

        IF ( 1 < iReturns ) THEN DO:
            cRetValue = "Error user-id is ambiguous".
        LEAVE.
        END.

        /* Get the LDAP search's first returned entry */
        RUN ldap_first_entry( INPUT  m_mLDAPContext,
                              OUTPUT  mLDAPMsg,
                              OUTPUT mEntry ) /* NO-ERROR */.

        /* We have a matching directory entry, so get the full DN of the user 
         * account entry. */
        IF (m_lTrace) THEN 
            MESSAGE "trace: getting LDAP DN user-id " + 
                    p_cUserId.
        RUN ldap_get_dn  ( INPUT  m_mLDAPContext,
                           INPUT  mEntry,
                           OUTPUT mDN ) NO-ERROR.
        
        RUN LdapGetLastError(OUTPUT iStatus).
        IF ( 0 <> iStatus ) THEN DO:
            cRetValue = "Error obtaining fully qualified user-id DN.".        
            LEAVE.
        END.

        IF (m_lDebug) THEN 
            MESSAGE "debug: resolved LDAP DN for user-id " + p_cUserID +
                    " is " + GET-STRING(mDN,1).

        /* Return the user-id DN */
        p_cReturnUserDN = GET-STRING(mDN, 1).

        /* Free the LDAP DN's memory allocation by the shared library */
        IF (m_lDebug) THEN 
            MESSAGE "debug: Relasing LDAP DN memory".
        RUN ldap_memfree ( INPUT  mDN ) /* NO-ERROR */.

    END. ELSE DO:
        cRetValue = "Missing user id authentication data.".
    END.

    /* Release LDAP entry memory structure allocation in the shared library */
    IF ( lFreeEntry ) THEN DO:
        IF (m_lDebug) THEN 
            MESSAGE "debug: Relasing LDAP search result".
        RUN ldap_msgfree ( INPUT  mLDAPMsg,
                          OUTPUT iStatus )  NO-ERROR.
    END.

    /* Clean up all OS memory pointer allocations */
    /* SET-SIZE(mDN) = 0. */
    /* SET-SIZE(mEntry) = 0. */
    SET-SIZE(mRes) = 0.

    /* Return the results of the user account DN search */

    RETURN cRetValue.
END PROCEDURE.

/*
** PROGRAM     : resolveMemberships
** DESCRIPTION : Search for user account memberships in groups.
**      
** PARAMETERS  :
**   p_cUserDN          The required input containing the full user DN.
**
**   p_cGroups          The output parameter that will get the comma delimited
**                      list of group names, if any.
** SIDE EFFECTS:
**      None. 
** RETURN-VALUE: "" for success, otherwise returns the failure reason.
*/
PROCEDURE resolveMemberships :
    /* Procedure input/output parameter definitions. */
    DEFINE INPUT PARAMETER  p_cUserDN           AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER p_cGroups           AS CHARACTER NO-UNDO.

    /* Procedure local variables */
    DEFINE VARIABLE cRetValue               AS CHARACTER INITIAL "".
    DEFINE VARIABLE iStatus                 AS INTEGER NO-UNDO. 
    DEFINE VARIABLE iBindStatus             AS INTEGER NO-UNDO. 
    DEFINE VARIABLE lDone                   AS LOGICAL INITIAL NO NO-UNDO.
    DEFINE VARIABLE lFreeEntry              AS LOGICAL INITIAL NO NO-UNDO.
    DEFINE VARIABLE mEntry                  AS MEMPTR NO-UNDO.
    DEFINE VARIABLE mRes                    AS MEMPTR NO-UNDO.
    DEFINE VARIABLE lEntry                  AS INT64 NO-UNDO.
    DEFINE VARIABLE mDN                     AS MEMPTR NO-UNDO.
    DEFINE VARIABLE mLDAPMsg                AS MEMPTR NO-UNDO.
    DEFINE VARIABLE mEntryMsg               AS MEMPTR NO-UNDO.
    DEFINE VARIABLE iReturns                AS INTEGER INITIAL 0 NO-UNDO.
    DEFINE VARIABLE cGroupFilter            AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cGroupList              AS CHARACTER INITIAL "" NO-UNDO.
    DEFINE VARIABLE mAttrArray              AS MEMPTR EXTENT 50 NO-UNDO.
    DEFINE VARIABLE cAttrName               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE mAttrName               AS MEMPTR NO-UNDO.
    DEFINE VARIABLE mAttrValues             AS MEMPTR NO-UNDO.
    DEFINE VARIABLE mAttrValue              AS MEMPTR NO-UNDO.
    DEFINE VARIABLE iEntryIdx               AS INTEGER NO-UNDO.
    DEFINE VARIABLE mAttrCtxHolder          AS MEMPTR NO-UNDO.

    IF (m_lTrace) THEN 
        MESSAGE "trace: searching group memberships for user-id " + p_cUserDN.
    
    /* Do not run without the minimum required parameters */
    IF ( ? <> p_cUserDN AND
         "" <> p_cUserDN ) THEN DO WHILE ( NOT lDone ):
         lDone = YES.

         /* Need to allocate memory to store memory pointer and structures
         ** returned by the shared library. */
         SET-SIZE(mAttrCtxHolder) = m_iOSPointerSize. 
         PUT-BYTES(mAttrCtxHolder, 1) = m_mOSNullPointer.
         SET-SIZE(mRes) = m_iOSPointerSize.
         PUT-BYTES(mRes, 1) = m_mOSNullPointer.

         /* Setup the one attribute we need returned from the LDAP search */
         cAttrName = "cn".
         SET-SIZE(mAttrName) = LENGTH(cAttrName) + 1.
         PUT-STRING(mAttrName,1) = cAttrName.

         /* Setup the attribute array of what to return for each entry */
         SET-SIZE(mAttrArray[1]) = LENGTH(cAttrName) + 1.
         PUT-STRING(mAttrArray[1], 1) = cAttrName.

         /* Terminate the returned attribute array with a null memory pointer*/
         SET-SIZE(mAttrArray[2]) = 0.
 
        /* Setup the user account entry search filter */
        cGroupFilter = REPLACE(m_cGroupSearchFilter, "%s", p_cUserDN).

        IF (m_lTrace) THEN 
            MESSAGE "trace: searching for groups for " + p_cUserDN + " using " +
                    "root " + m_cGroupSearchRootDN + " and filter " +
                    cGroupFilter.


        /* Search the for group/groupOfNames/groupOfUniqueNames entry objects
        ** using the full user-account DN.  For each entry found return its
        ** common name (cn) which holds the group's simple name */
        RUN ldap_search_s  ( INPUT  m_mLDAPContext,
                             INPUT  m_cGroupSearchRootDN,
                             INPUT  {&LDAP_SCOPE_SUBTREE},
                             INPUT  cGroupFilter,
                             INPUT  mAttrArray,
                             INPUT  0,               /* return attributes */
                             OUTPUT mRes,
                             OUTPUT iStatus) /* NO-ERROR */.

        IF (m_lDebug) THEN 
            MESSAGE "debug: LDAP group search returned " + 
                    " : " + STRING(iStatus).

        IF ( 0 <> iStatus ) THEN DO:
            /* not found... */
            RUN ldap_err2string( INPUT  iStatus, OUTPUT m_mLastLDAPError ) /* NO-ERROR */.
            cRetValue = "Cannot resolve group memberships for " + p_cUserDN + " : " + 
                        GET-STRING(m_mLastLDAPError,1).
            LEAVE.
        END.

        /* Remember to free the LDAP entry structure allocated by the 
        ** shared library on procedure exit. */
        lFreeEntry = YES.

        /* Get the pointer to the LDAPMessage structure returned by the search */
        SET-POINTER-VALUE(mLDAPMsg) = GET-INT64(mRes,1).

        /* Now check to see that the LDAP search got some entries.  */
        IF (m_lTrace) THEN 
            MESSAGE "trace: determining number of LDAP group matches for user-id " + 
                    p_cUserDN.

        RUN ldap_count_entries( INPUT  m_mLDAPContext,
                                INPUT  mLDAPMsg,
                                OUTPUT iReturns ) /* NO-ERROR */.

        IF (m_lDebug) THEN 
            MESSAGE "debug: number of LDAP matches for user group memberships is " + 
                    STRING(iReturns).

        IF ( -1 = iReturns ) THEN DO:
            cRetValue = "Error resolving user group memberships ".
            LEAVE.
        END.

        /* It is not an error if no groups are found. Just a condition */
        IF ( 0 = iReturns ) THEN DO:
            IF (m_lDebug) THEN 
                MESSAGE "debug: no group memberships found for " + p_cUserDN.
            LEAVE.
        END.

        /* Get the first LDAP entry returned by the search so that we can
        ** get it's attributes. */
        RUN ldap_first_entry( INPUT  m_mLDAPContext,
                              OUTPUT  mLDAPMsg,
                              OUTPUT mEntryMsg ) /* NO-ERROR */.
    
        IF ( 0 = GET-POINTER-VALUE(mEntryMsg) ) THEN DO:
            cRetValue = "error: error getting first LDAP (group) search entry".
            LEAVE.
        END.

        /* Iterate through the LDAP group entries returned by the LDAP 
        ** search */
        DO iEntryIdx = 1 TO iReturns:
            DEFINE VARIABLE cDelim          AS CHARACTER INITIAL "" NO-UNDO.

            RUN ldap_get_dn  ( INPUT  m_mLDAPContext,
                               INPUT  mEntryMsg,
                               OUTPUT mDN ) NO-ERROR.
            IF (m_lDebug) THEN
                MESSAGE "debug: returned group DN: " + GET-STRING(mDN, 1).

            /* Free the LDAP DN's memory allocation by the shared library */
            IF (m_lDebug) THEN 
                MESSAGE "debug: Relasing LDAP DN memory".
            RUN ldap_memfree ( INPUT  mDN ) /* NO-ERROR */.

            /* Now get a list of memory pointers to the attributes value(s).
            ** For a cn we should only get 1 value */
            RUN ldap_get_values  ( INPUT  m_mLDAPContext,
                                   INPUT  mEntryMsg,
                                   INPUT  mAttrName,
                                   OUTPUT mAttrValues ) /* NO-ERROR */.

            RUN LdapGetLastError(OUTPUT iStatus).
            IF (m_lDebug) THEN 
                MESSAGE "debug: return status for attribute value: " + STRING(iStatus).
            RUN LdapGetLastError(OUTPUT iStatus).
            IF ( 0 <> iStatus ) THEN DO:
                RUN ldap_err2string( INPUT  iStatus, OUTPUT m_mLastLDAPError ) /* NO-ERROR */.
                cRetValue = "error: obtaining group common-name: " +
                            GET-STRING(m_mLastLDAPError,1).        
                LEAVE.
            END.

            /* Get a count of the number of attribute values for a sanity 
            ** check */
            DEFINE VARIABLE iValueCount AS INTEGER NO-UNDO.
            RUN ldap_count_values  ( INPUT  mAttrValues,
                                    OUTPUT iValueCount ) /* NO-ERROR */.
            IF (m_lDebug) THEN 
                MESSAGE "debug: found " + STRING(iValueCount) + 
                        " attribute values for cn".

            /* Add the name of the LDAP group (the cn attribute) to the list of
             * group names the user is a member of */
            SET-POINTER-VALUE(mAttrValue) = GET-INT64(mAttrValues, 1).
            IF (m_lDebug) THEN 
                MESSAGE "debug: recoverd group cn: " + GET-STRING(mAttrValue,1).
            cGroupList = cGroupList + cDelim + GET-STRING(mAttrValue,1).
            cDelim = ",".

            /* Free up the memory allocated for the LDAP attribure values */
            IF (m_lDebug) THEN 
                MESSAGE "debug: Relasing LDAP values ".
            RUN ldap_value_free  ( INPUT mAttrValues, OUTPUT iStatus) /* NO-ERROR */.

            /* Get the next LDAP search entry */
            IF (m_lTrace) THEN 
                MESSAGE "trace: Getting next LDAP search entry ...".
            RUN ldap_next_entry( INPUT  m_mLDAPContext,
                                 INPUT  mEntryMsg,
                                 OUTPUT lEntry ) /* NO-ERROR */.

        END.

    END. ELSE DO:
        cRetValue = "Missing user id authentication data.".
    END.

    /* Release LDAP entry memory allocation made by the shared library */
    IF ( lFreeEntry ) THEN DO:
        IF (m_lDebug) THEN 
            MESSAGE "debug: Relasing LDAP search result".
        RUN ldap_msgfree ( INPUT  mLDAPMsg,
                          OUTPUT iStatus ) /* NO-ERROR */.
    END.

    /* Clean up all OS memory pointer allocations */
    /* SET-SIZE(mEntry) = 0. */
    /* SET-SIZE(mDN) = 0. */
    /* SET-SIZE(mEntryMsg) = 0. */
    /* SET-SIZE(mLDAPMsg) = 0. */
    SET-SIZE(mAttrName) = 0.
    SET-SIZE(mAttrCtxHolder) = 0.
    SET-SIZE(mRes) = 0.

    /* Return any accumulated user account memberships */
    p_cGroups = cGroupList.

    RETURN cRetValue.
END PROCEDURE.

/*
** PROGRAM     : resolveADStatus
** DESCRIPTION : Get the Active Directory user's account status and check it
**               for locked and disabled.
**      
** PARAMETERS  :
**   p_cUserDN          The required input containing the full user DN.
**
** SIDE EFFECTS:
**      none.
** RETURN-VALUE: "" for success, otherwise returns the failure reason.
*/
PROCEDURE resolveADStatus :
    /* Procedure input/output parameter definitions. */
    DEFINE INPUT PARAMETER  p_cUserDN           AS CHARACTER NO-UNDO.

    /* Procedure local variables */
    DEFINE VARIABLE cRetValue               AS CHARACTER INITIAL "".
    DEFINE VARIABLE iStatus                 AS INTEGER INITIAL 0 NO-UNDO. 
    DEFINE VARIABLE iBindStatus             AS INTEGER INITIAL 0 NO-UNDO. 
    DEFINE VARIABLE lDone                   AS LOGICAL INITIAL NO NO-UNDO.
    DEFINE VARIABLE lFreeEntry              AS LOGICAL INITIAL NO NO-UNDO.
    DEFINE VARIABLE mRes                    AS MEMPTR NO-UNDO.
    DEFINE VARIABLE lEntry                  AS INT64 INITIAL 0 NO-UNDO.
    DEFINE VARIABLE mDN                     AS MEMPTR NO-UNDO.
    DEFINE VARIABLE mLDAPMsg                AS MEMPTR NO-UNDO.
    DEFINE VARIABLE mEntry                  AS MEMPTR NO-UNDO.
    DEFINE VARIABLE iReturns                AS INTEGER INITIAL 0 NO-UNDO.
    DEFINE VARIABLE cUserFilter             AS CHARACTER INITIAL "" NO-UNDO. 
    DEFINE VARIABLE mAttrArray              AS MEMPTR EXTENT 50 NO-UNDO.
    DEFINE VARIABLE cAttrName               AS CHARACTER INITIAL "" NO-UNDO.
    DEFINE VARIABLE mAttrValues             AS MEMPTR NO-UNDO.
    DEFINE VARIABLE mAttrName               AS MEMPTR NO-UNDO.
    DEFINE VARIABLE mAttrValue              AS MEMPTR NO-UNDO.
    DEFINE VARIABLE iEntryIdx               AS INTEGER INITIAL 0 NO-UNDO.
    DEFINE VARIABLE iAcctStatus             AS INTEGER INITIAL 0 NO-UNDO.
    DEFINE VARIABLE iStatusBit              AS INTEGER INITIAL 0 NO-UNDO.
    DEFINE VARIABLE mAttrCtxHolder          AS MEMPTR NO-UNDO.

    IF (m_lTrace) THEN 
        MESSAGE "trace: resolving Active Directory status for user-id " + p_cUserDN.
    
    /* Run only when we have the required input parameters */
    IF ( ? <> p_cUserDN AND
         "" <> p_cUserDN ) THEN DO WHILE ( NOT lDone ):
         lDone = YES.

         /* Allocate memory to store the memory addresses and structures 
         ** returned by the shared library. */
         SET-SIZE(mAttrCtxHolder) = m_iOSPointerSize. 
         PUT-BYTES(mAttrCtxHolder, 1) = m_mOSNullPointer.
         SET-SIZE(mRes) = m_iOSPointerSize.
         PUT-BYTES(mRes, 1) = m_mOSNullPointer.

         /* Setup the attributes we want the LDAP search operation to return */
         cAttrName = "userAccountControl".
         SET-SIZE(mAttrName) = LENGTH(cAttrName) + 1.
         PUT-STRING(mAttrName,1) = cAttrName.

         /* Setup the attribute array of what to return for each entry.  Only
         ** Windows Active Directory (LDAP) supports account enable/lock in
         ** the userAccountControl attribute */
         SET-SIZE(mAttrArray[1]) = LENGTH(cAttrName) + 1.
         PUT-STRING(mAttrArray[1], 1) = cAttrName.

         /* Null terminate the return attribute array list with a null memory
         ** pointer.  */
         SET-SIZE(mAttrArray[2]) = 0.
 
        /* Setup the LDaP user object search filter */
        cUserFilter = "(objectclass=person)".

        IF (m_lTrace) THEN 
            MESSAGE "trace: searching for AD status for " + p_cUserDN + " using " +
                    "root " + m_cUserSearchRootDN + " and filter " +
                    cUserFilter.


        /* Search for the user account with a full DN */
        RUN ldap_search_s  ( INPUT  m_mLDAPContext,
                             INPUT  p_cUserDN,
                             INPUT  {&LDAP_SCOPE_BASE},
                             INPUT  cUserFilter,
                             INPUT  mAttrArray,
                             INPUT  0,               /* return attributes */
                             OUTPUT mRes,
                             OUTPUT iStatus).

        IF (m_lDebug) THEN 
            MESSAGE "debug: LDAP AD status search returned " + 
                    " : " + STRING(iStatus).

        IF ( 0 <> iStatus ) THEN DO:
            /* not found... */
            RUN ldap_err2string( INPUT  iStatus, OUTPUT m_mLastLDAPError ) /* NO-ERROR */.
            cRetValue = "Cannot get AD account status for " + p_cUserDN + " : " + 
                        GET-STRING(m_mLastLDAPError,1).
            LEAVE.
        END.

        /* Remember to free the LDAP entry on procedure exit. */
        lFreeEntry = YES.

        /* Get the pointer to the LDAPMessage structure returned by the search */
        SET-POINTER-VALUE(mLDAPMsg) = GET-INT64(mRes,1).

        /* Now check to see that we got exactly one return hit.  If we get more
        ** than one, the simple user-id is ambiguous, and an error*/
        IF (m_lTrace) THEN 
            MESSAGE "trace: determining number of LDAP AD status matches for user-id " + 
                    p_cUserDN.

        RUN ldap_count_entries( INPUT  m_mLDAPContext,
                                INPUT  mLDAPMsg,
                                OUTPUT iReturns ) /* NO-ERROR */.

        IF (m_lDebug) THEN 
            MESSAGE "debug: number of LDAP matches for AD status is " + 
                    STRING(iReturns).

        IF ( -1 = iReturns ) THEN DO:
            cRetValue = "Error resolving user group memberships ".
            LEAVE.
        END.

        /* It is an error if the user accounts is not found. */
        IF ( 0 = iReturns ) THEN DO:
            IF (m_lDebug) THEN 
                MESSAGE "debug: no AD user account found to obtains status for " + p_cUserDN.
            LEAVE.
        END.

        /* Get the first LDAP entry returned by the search so that we can
        ** get it's attributes. */
        RUN ldap_first_entry( INPUT  m_mLDAPContext,
                              OUTPUT  mLDAPMsg,
                              OUTPUT mEntry ) /* NO-ERROR */.
    
        IF ( 0 = GET-POINTER-VALUE(mEntry) ) THEN DO:
            cRetValue = "error: error getting first LDAP (group) search entry".
            LEAVE.
        END.
        
        /* Get a list of values for the attribute, there should only ever be
        ** one. */
        iStatus = 0.
        RUN ldap_get_values  ( INPUT m_mLDAPContext,
                               INPUT mEntry,
                               INPUT mAttrName,
                               OUTPUT mAttrValues ) /* NO-ERROR */.

        RUN LdapGetLastError(OUTPUT iStatus).
        IF (m_lDebug) THEN 
            MESSAGE "debug: return status for attribute userAccountControl : " + STRING(iStatus).
        IF ( 0 <> iStatus ) THEN DO:
            RUN ldap_err2string( INPUT  iStatus, OUTPUT m_mLastLDAPError ) /* NO-ERROR */.
            cRetValue = "error: obtaining AD entry userAccountControl : " +
                        GET-STRING(m_mLastLDAPError,1).        
            LEAVE.
        END.

        /* Get the 4 byte attribute's binary value which is a bit-mask. */
        SET-POINTER-VALUE(mAttrValue) = GET-INT64(mAttrValues, 1).
        iAcctStatus = INTEGER(GET-STRING(mAttrValue, 1)).
        IF (m_lDebug) THEN 
            MESSAGE "debug: return account status userAccountControl : " + 
                    STRING(iAcctStatus).
        /* First check account disabled bit */
        iStatusBit = GET-BITS(iAcctStatus, 2, 1).
        IF ( 0 <> iStatusBit ) THEN DO:
            cRetValue = "account is disabled".
        END. ELSE DO:
            /* If not disabled, then is it locked? */
            iStatusBit = GET-BITS(iAcctStatus, 5, 1).
            IF ( 0 <> iStatusBit ) THEN DO:
                cRetValue = "account is locked".
            END.
        END.

        /* Free up the memory allocated for the LDAP attribure values by the
        ** shared library */
        IF (m_lDebug) THEN 
            MESSAGE "debug: Relasing LDAP attribute value ".
        RUN ldap_value_free  ( INPUT mAttrValues, OUTPUT iStatus) /* NO-ERROR */.

    END. ELSE DO:
        cRetValue = "Missing user id authentication data.".
    END.

    /* Release LDAP entry memory allocation by the shared library */
    IF ( lFreeEntry ) THEN DO:
        IF (m_lDebug) THEN 
            MESSAGE "debug: Relasing LDAP search result".
        RUN ldap_msgfree ( INPUT  mLDAPMsg,
                          OUTPUT iStatus ) /* NO-ERROR */.
    END.

    /* Clean up all OS memory pointer allocations */
    /* SET-SIZE(mDN) = 0. */
    /* SET-SIZE(mLDAPMsg) = 0. */
    /* SET-SIZE(mEntry) = 0. */
    SET-SIZE(mAttrCtxHolder) = 0.
    SET-SIZE(mRes) = 0.

    RETURN cRetValue.
END PROCEDURE.

/*
** PROGRAM     : loadAccountAttributes
** DESCRIPTION : For a specific user account, get and return the list of
**               account attributes.  Scan out LDAP specific things like
**               cn, uid, objectclass, etc.
**      
** PARAMETERS  :
**      p_cUserDN       The fully qualified user-DN to get attributes for.
**      p_cAttrs        On input it contains the list of attributes to return.
**                          ""          return nothing
**                          "*"          return everything
**                          "a,b,c"     return only attributes a, b, and c
**                      On output it will contain a "&" delimited list of
**                      name=value attributes.
**
**                      NOTE: a maximum of 50 attributes can be specified
** SIDE EFFECTS:
**      none.
** RETURN-VALUE: "" for success, otherwise returns the failure reason.
*/
PROCEDURE loadAccountAttributes PRIVATE :
    /* Procedure input/output parameter definitions. */
    DEFINE INPUT  PARAMETER       p_cUserDN      AS CHARACTER NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER p_cAttrs       AS CHARACTER NO-UNDO.
    
    /* Procedure local variables */
    DEFINE VARIABLE cRetValue               AS CHARACTER INITIAL "".
    DEFINE VARIABLE cAttrList               AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cAttrName               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAttrValue              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserFilter             AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cRetAttrs               AS CHARACTER INITIAL "" NO-UNDO.
    DEFINE VARIABLE cAttrDelim              AS CHARACTER INITIAL "" NO-UNDO.
    DEFINE VARIABLE iAttrCount              AS INTEGER INITIAL 0 NO-UNDO.
    DEFINE VARIABLE iStatus                 AS INTEGER NO-UNDO. 
    DEFINE VARIABLE iAttrIndex              AS INTEGER NO-UNDO.
    DEFINE VARIABLE iValueCount             AS INTEGER NO-UNDO.
    DEFINE VARIABLE iValueIndex             AS INTEGER NO-UNDO.
    DEFINE VARIABLE iReturns                AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPtrOffset              AS INTEGER NO-UNDO.
    DEFINE VARIABLE lDone                   AS LOGICAL INITIAL NO NO-UNDO.
    DEFINE VARIABLE lAttrDone               AS LOGICAL INITIAL NO NO-UNDO.
    DEFINE VARIABLE lFreeEntry              AS LOGICAL INITIAL NO NO-UNDO.
    DEFINE VARIABLE lWildCardAttrs          AS LOGICAL INITIAL NO NO-UNDO.
    DEFINE VARIABLE mAttrArray              AS MEMPTR EXTENT 50 NO-UNDO.
    DEFINE VARIABLE mRes                    AS MEMPTR NO-UNDO.
    DEFINE VARIABLE lEntry                  AS INT64 NO-UNDO.
    DEFINE VARIABLE mLDAPMsg                AS MEMPTR NO-UNDO.
    DEFINE VARIABLE mEntry                  AS MEMPTR NO-UNDO.
    DEFINE VARIABLE mAttrCtxHolder          AS MEMPTR NO-UNDO.
    DEFINE VARIABLE mAttrCtx                AS MEMPTR NO-UNDO.
    DEFINE VARIABLE mAttrName               AS MEMPTR NO-UNDO.
    DEFINE VARIABLE mAttrValues             AS MEMPTR NO-UNDO.
    DEFINE VARIABLE mAttrValue              AS MEMPTR NO-UNDO.


    IF (m_lTrace) THEN 
        MESSAGE "trace: loading attributes for user-id " + p_cUserDN.
    
    /* Copy the list of attributes to return.  
    ** Initialize the return list.
    ** Obtain how many attributes to get ( 0 equals all attributes ) */
    cAttrList = p_cAttrs.

    /* Initialize all of the OS memory pointer variables */
    SET-SIZE(mAttrCtx) = m_iOSPointerSize. 
    SET-SIZE(mAttrName) = {&MAX_ATTR_SIZE}. 
    SET-SIZE(mAttrCtxHolder) = m_iOSPointerSize. 
    SET-SIZE(mLDAPMsg) = m_iOSPointerSize.
    SET-SIZE(mEntry) = m_iOSPointerSize.
    SET-SIZE(mRes) = m_iOSPointerSize.
    PUT-BYTES(mRes, 1) = m_mOSNullPointer.

    PUT-BYTES(mAttrCtx, 1) = m_mOSNullPointer.
    PUT-BYTES(mAttrCtxHolder, 1) = m_mOSNullPointer.
    PUT-BYTES(mLDAPMsg, 1) = m_mOSNullPointer.
    PUT-BYTES(mEntry, 1) = m_mOSNullPointer.

    /* Setup the user object search filter */
    cUserFilter = "(objectclass=person)".

    DO WHILE NOT (lDone):
        lDone = YES.

        /* initialize the return list of attributes */
        iAttrCount = NUM-ENTRIES(cAttrList, "," ).
        IF ( 50 < iAttrCount ) THEN 
            iAttrCount = 50.    
    
        /* Setup the attribute name array of memory pointers */
        IF ( 0 < iAttrCount ) THEN DO:
            IF ( 1 = iAttrCount AND
                 cAttrList = "*") THEN DO:
                 lWildCardAttrs = YES.            
            END. ELSE DO:
                /* Now build the list of attributes to ask for */
                DO iAttrIndex = 1 TO iAttrCount:
                    cAttrName = ENTRY(iAttrIndex, cAttrList, ",").
                    SET-SIZE(mAttrArray[iAttrIndex]) = LENGTH(cAttrName) + 1.
                    PUT-STRING(mAttrArray[iAttrIndex], 1) = cAttrName.
                END.
                SET-SIZE(mAttrArray[iAttrCount + 1]) = 0.
            END.
        END.


        /* Do the search to obtain a full list of user account attributes. It's
        ** easier to do a wildcard as a separate call even though it's just
        ** one attribure difference.  */
        IF ( "*" = cAttrList ) THEN DO:
            IF (m_lTrace) THEN 
                MESSAGE "trace: searching for wildcard attributes using " +
                        p_cUserDN + " and filter " + cUserFilter.
            RUN ldap_search_s  ( INPUT  m_mLDAPContext,
                                 INPUT  p_cUserDN,
                                 INPUT  {&LDAP_SCOPE_BASE},
                                 INPUT  cUserFilter,
                                 INPUT  m_mOSNullPointer,   /* wildcard */
                                 INPUT  0,                  /* return attributes */
                                 OUTPUT mRes,
                                 OUTPUT iStatus).
        END. ELSE DO:
            IF (m_lTrace) THEN 
                MESSAGE "trace: searching for named attributes using " +
                        p_cUserDN + " and filter " + cUserFilter.
            RUN ldap_search_s  ( INPUT  m_mLDAPContext,
                                 INPUT  p_cUserDN,
                                 INPUT  {&LDAP_SCOPE_BASE},
                                 INPUT  cUserFilter,
                                 INPUT  mAttrArray,
                                 INPUT  0,               /* return attributes */
                                 OUTPUT mRes,
                                 OUTPUT iStatus).
        END.

        IF (m_lDebug) THEN 
            MESSAGE "debug: LDAP search returned " + 
                    " : " + STRING(iStatus).

        IF ( 0 <> iStatus ) THEN DO:
            /* not found... */
            RUN ldap_err2string( INPUT  iStatus, OUTPUT m_mLastLDAPError ) /* NO-ERROR */.
            cRetValue = "Cannot locate user account attributes for " + p_cUserDN +
                    " : " + 
                    GET-STRING(m_mLastLDAPError,1).
            LEAVE.
        END.

        /* Don't forget to free the LDAP entry memory allocation on exit */
        lFreeEntry = YES.

        /* Get the pointer to the LDAPMessage structure returned by the search */
        SET-POINTER-VALUE(mLDAPMsg) = GET-INT64(mRes,1).

        /* Now check to see that we got exactly one entry returned by the LDAP
        ** search.  If we get more than one, the user-id is ambiguous, and an error*/
        IF (m_lTrace) THEN 
            MESSAGE "trace: determining number of LDAP matches for user-id " + 
                    p_cUserDN. 

        RUN ldap_count_entries( INPUT  m_mLDAPContext,
                                INPUT  mLDAPMsg,
                                OUTPUT iReturns ) /* NO-ERROR */.

        IF (m_lDebug) THEN 
            MESSAGE "debug: number of LDAP matches for user-id is " + 
                    STRING(iReturns).

        IF ( -1 = iReturns ) THEN DO:
            cRetValue = "Error resolving number of user-id accounts: ".
            LEAVE.
        END.

        IF ( 0 = iReturns ) THEN DO:
            cRetValue = "Error user-id account is not found".
            LEAVE.
        END.

        IF ( 1 < iReturns ) THEN DO:
            cRetValue = "Error user-id matches multiple accounts".
            LEAVE.
        END.

        IF (m_lTrace) THEN 
            MESSAGE "trace: getting first LDAP attribute name... ".

        /* Get the first LDAP entry returned by the search so that we can
        ** get it's attributes. */
        RUN ldap_first_entry( INPUT  m_mLDAPContext,
                              OUTPUT  mLDAPMsg,
                              OUTPUT mEntry ) /* NO-ERROR */.
    
        IF ( 0 = GET-POINTER-VALUE(mEntry) ) THEN DO:
            cRetValue = "error: error getting first LDAP (attributes) search entry".
            LEAVE.
        END.
        
        /* Setup the attribure retreival loop by getting the first attribute
        ** from the shared library. */
        RUN ldap_first_attribute  ( INPUT  m_mLDAPContext,
                                    INPUT  mEntry,
                                    OUTPUT mAttrCtxHolder,
                                    OUTPUT mAttrName) /* NO-ERROR */.

        RUN LdapGetLastError( OUTPUT iStatus ) /* NO-ERROR */.
        IF ( 0 <> iStatus ) THEN DO:
            RUN ldap_err2string( INPUT  iStatus, OUTPUT m_mLastLDAPError ) /* NO-ERROR */.
            cRetValue = "Error getting entry attribute name: " + 
                GET-STRING(m_mLastLDAPError,1).
            LEAVE.
        END.

        /* Get the pointer to the LDAPMessage structure returned by the search */
        SET-POINTER-VALUE(mAttrCtx) = GET-INT64(mAttrCtxHolder,1).

        /* Now extract the attribute's name to ensure we have at least one. */
        cAttrName = GET-STRING(mAttrName, 1).

        IF ( ? = cAttrName OR
             "" = cAttrName ) THEN DO:
            IF (m_lDebug) THEN 
                MESSAGE "debug: no LDAP attributes found".
            lAttrDone = YES.
        END.

        /* Perform a loop getting the returned attribute names, and for each
         * attribute name, get its value and add it to the returned list */
        DO WHILE NOT (lAttrDone):
            /* Get the values for the LDAP entry */
            IF (m_lTrace) THEN 
                MESSAGE "trace: getting value for attribute: " + cAttrName.

            /* Get a list of the attribure's values (which may be a multi-value
            ** attribute type ). */
            RUN ldap_get_values  ( INPUT m_mLDAPContext,
                                   INPUT mEntry,
                                   INPUT mAttrName,
                                   OUTPUT mAttrValues ) /* NO-ERROR */.

            RUN LdapGetLastError(OUTPUT iStatus).
            IF (m_lDebug) THEN 
                MESSAGE "debug: return status for attribute value: " + STRING(iStatus).


            /* Get the number of values for the attribute */
            RUN ldap_count_values  ( INPUT  mAttrValues,
                                    OUTPUT iValueCount ) /* NO-ERROR */.
            IF (m_lDebug) THEN 
                MESSAGE "debug: found " + STRING(iValueCount) + 
                        " attribute values for " + cAttrName.

            /* Now loop and extract the attribute values from an array of 
            ** character string pointers */
            iPtrOffset = 1.
            DO iAttrIndex = 1 TO iValueCount:
                SET-POINTER-VALUE(mAttrValue) = GET-INT64(mAttrValues, iPtrOffset).
                IF (m_lDebug) THEN 
                    MESSAGE "debug: attribute value for " + cAttrName + "[" +
                            STRING(iAttrIndex) + "] is: " + GET-STRING(mAttrValue,1).

                cRetAttrs = cRetAttrs + cAttrDelim + cAttrName + "=" + 
                            GET-STRING(mAttrValue,1).
                cAttrDelim = "&".
                iPtrOffset = iPtrOffset + m_iOSPointerSize /* GET-INT64 */.
            END.

            /* Free up the memory allocated for the LDAP attribure values */
            IF (m_lDebug) THEN 
                MESSAGE "debug: Relasing LDAP attribute values ".
            RUN ldap_value_free  ( INPUT mAttrValues, OUTPUT iStatus) /* NO-ERROR */.

            /* Give back the LDAP allocated memory when we are done with the
             * attribute name */
            cAttrName = ?.
            RUN ldap_memfree (mAttrName) /* NO-ERROR */.

            IF (m_lTrace) THEN 
                MESSAGE "trace: getting next LDAP attribute name ... ".

            /* Get the next (multi-value) attribute to process */
            RUN ldap_next_attribute  ( INPUT  m_mLDAPContext,
                                       INPUT  mLDAPMsg,
                                       INPUT  mAttrCtx,
                                       OUTPUT mAttrName ) /* NO-ERROR */.

            RUN LdapGetLastError( OUTPUT iStatus ) /* NO-ERROR */.
            IF ( 0 <> iStatus ) THEN DO:
                RUN ldap_err2string( INPUT  iStatus, OUTPUT m_mLastLDAPError ) /* NO-ERROR */.
                cRetValue = "Error getting next entry attribute: " +
                    GET-STRING(m_mLastLDAPError,1).
                LEAVE.
            END.

            /* Get the next attribute's name */
            IF ( 0 <> GET-POINTER-VALUE(mAttrName) ) THEN DO:
                cAttrName = GET-STRING(mAttrName, 1) NO-ERROR.
                IF ( ? <> cAttrName AND
                     "" <> cAttrName ) THEN DO:
                    IF (m_lTrace) THEN 
                        MESSAGE "trace: next attr name is: " + cAttrName.
                END. ELSE DO:
                    cRetValue = "Invalid blank attribute name returned".
                    LEAVE.
                END.
            END. ELSE DO:
                IF (m_lTrace) THEN 
                    MESSAGE "trace: end of LDAP attribute names".
                LEAVE.
            END.
        END.
    END.

    /* Free the memory allocated by the ldap_search function in the shared
    ** library */
    IF ( lFreeEntry ) THEN DO:
        IF (m_lTrace) THEN 
            MESSAGE "trace: Cleaning up LDAP search result".
        RUN ldap_msgfree ( INPUT  mLDAPMsg,
                           OUTPUT iStatus ).
    END.

    /* Clean up all memory allocations */
    /* SET-SIZE(mLDAPMsg) = 0. */
    /* SET-SIZE(mEntry) = 0. */
    SET-SIZE(mAttrName) = 0.
    SET-SIZE(mAttrCtx) = 0.
    SET-SIZE(mAttrCtxHolder) = 0.
    SET-SIZE(mAttrName) = 0.
    SET-SIZE(mRes) = 0.

    /* Return the ouptut if no errors have occured. */
    IF ( "" = cRetValue) THEN DO:
       p_cAttrs = cRetAttrs.
    END.

    RETURN cRetValue.
END PROCEDURE.


/*
** PROGRAM     : defaultConstructor
** DESCRIPTION : Initialize the LDAP authentication by connecting to the
**  LDAP V3 server.  Since basic LDAP authentication uses an "clear-text" 
**  password, it is always recommended that SSL connection are used.
**      
** PARAMETERS  :
**      p_cBindServer   A required character DNS name of the LDAP V3 server
**      p_cBindPort     An optional character TCP port number of the the
**                      LDAP server.  If UNKNOWN or blank, the default 389 will
**                      be used
**      p_cUserSearchRootDN An optional DN of where to search for unqualified
**                      user-ids.  
**      p_cUserSearchFilter An optional LDAP search filter that will be
**                      used to find user accounts by simple-name
**
**      p_cGroupSearchRootDN An optional DN of where to search for 
**                      user account group memberships.  If specified as
**                      ? or "", no groups search will be performed.
**
**      p_cGroupSearchFilter An optional LDAP search filter that will be
**                      used to find user account memberships.
**                      The default is:
**                        groupOfNames where member = user account DN
**                        OR
**                        groupOfUniqueNames where uniqueMember = user account DN
**
**      p_cSearchBindDN An optional LDAP (fully qualified) user DN 
**                      used to when searching the directory to resolve
**                      a simple user-id for authentication.  When the
**                      LDAP direcotry security is set, anonymous searches
**                      fail (sometimes silently).  When this is the case
**                      supply a fully qualified user DN that has the
**                      privileges to search the directory sub-tree for
**                      user-id entries.
**
**      p_cSearchBindDN An optional password string for the p_cSearchBindDN
**                      parameter.  If not specified an empty/blank string
**                      will be used.  
**      
**      p_cOptions      An optional character string that holds optional
**                      parameters for LDAP binding, searching, and 
**                      authentication.
**
**                      (See module header)
** SIDE EFFECTS:
**      none.
** RETURN-VALUE: "" for success, otherwise returns the failure reason.
*/
PROCEDURE defaultConstructor PRIVATE :
    /* Procedure input/output parameter definitions. */
    DEFINE INPUT PARAMETER p_cBindServer            AS CHARACTER NO-UNDO.   
    DEFINE INPUT PARAMETER p_cBindPort              AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER p_cUserSearchRootDN      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER p_cUserSearchFilter      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER p_cGroupSearchRootDN     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER p_cGroupSearchFilter     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER p_cSearchBindDN          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER p_cSearchBindPwd         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER p_cOptions               AS CHARACTER NO-UNDO.

    /* Procedure local variables */
    DEFINE VARIABLE cRetValue           AS CHARACTER INITIAL "".
    DEFINE VARIABLE lDone               AS LOGICAL INITIAL NO NO-UNDO.
    DEFINE VARIABLE cPort               AS CHARACTER INITIAL "389" NO-UNDO.
    DEFINE VARIABLE cBindSpec           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iStatus             AS INTEGER NO-UNDO.
    DEFINE VARIABLE mLDAPOption         AS MEMPTR NO-UNDO.
    DEFINE VARIABLE iIndex              AS INTEGER NO-UNDO.
    DEFINE VARIABLE mOptionValue        AS MEMPTR NO-UNDO.
    DEFINE VARIABLE cPropValue          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPropList           AS CHARACTER NO-UNDO.

    IF (m_lTrace) THEN 
        MESSAGE "trace: Enter LDAP default constructor".

    /* Startup the properties object to hold the options that are in 
    ** command line form:   -optname [optvalue] ...  */
    m_hProps = ?.
    RUN Properties.p PERSISTENT SET m_hProps ( INPUT ?, INPUT ? ).
    RUN loadFromCmdLine in m_hProps ( INPUT  p_cOptions ).

    /* Very early get the debug and trace options for debug purposes */
    RUN getProperty in m_hProps ( INPUT  "-debug", OUTPUT cPropValue).
    IF (? <> cPropValue ) THEN 
        m_lDebug = YES.

    RUN getProperty in m_hProps ( INPUT  "-trace", OUTPUT cPropValue).
    IF (? <> cPropValue ) THEN 
        m_lTrace = YES.

    /* Do some basic module level variable initialization */

    /* Do simple, clear-text, password authentication */
    m_iAuthMethod = {&LDAP_AUTH_SIMPLE}.

    /* init the OS NULL pointer value that is used to initialize and
     * mempointer that holds an OS NULL memory pointer value */
    SET-SIZE(m_mOSNullPointer) = m_iOSPointerSize. 
    DO iIndex = 1 to m_iOSPointerSize :
        PUT-BYTE(m_mOSNullPointer, iIndex) = 0.
    END.

    SET-SIZE(mOptionValue) = m_iOSPointerSize.

    /* record the original input parameters and do any UNKNOWN fixups */
    m_cBindServer = p_cBindServer.
    m_cBindPort = p_cBindPort.
    m_cGroupSearchRootDN = p_cGroupSearchRootDN.

    /* Insert the default LDAP TCP/IP port if one was not specified */
    IF ( ? = m_cBindPort ) THEN 
        m_cBindPort = "3268". /* "389". */

    /* Record the root DN to begin a sub-tree search for user accounts */
    IF ( ? <> p_cUserSearchRootDN AND
         "" <> p_cUserSearchRootDN ) THEN 
        m_cUserSearchRootDN = p_cUserSearchRootDN.

    /* Record the entry attribute search filter for searching for user accounts */
    IF ( ? <> p_cUserSearchFilter AND
         "" <> p_cUserSearchFilter ) THEN 
        m_cUserSearchFilter = p_cUserSearchFilter.

    /* Record the user DN and password to bind to the directory with when 
   ** sub-trees are access protected from the anonymous bind user */
    IF ( ? <> p_cSearchBindDN AND
         "" <> p_cSearchBindDN ) THEN 
        m_cSearchBindDN = p_cSearchBindDN.

    IF ( ? <> p_cSearchBindPwd AND
         "" <> p_cSearchBindPwd) THEN 
        m_cSearchBindPwd = p_cSearchBindPwd.

    /* See if we need to set the SSL property */
    RUN getProperty in m_hProps ( INPUT  "-ssl", OUTPUT cPropValue).
    IF (? <> cPropValue ) THEN 
        m_iEnableSSL = 1.

    /* Next check to see if we should do an Active Directory compatible
     * login by checking for account locked and disabled */
    RUN getProperty in m_hProps ( INPUT  "-AD", OUTPUT cPropValue).
    IF (? <> cPropValue ) THEN DO:
        m_lADCompatible = YES.
        m_cGroupSearchFilter = m_cADGroupSearchFilter.
    END.

    /* !! Override the group search filter AFTER we know whether the Active
     * Directory compatible switch is on.  That's because checking that
     * option can change what the default group filter is */
    IF ( ? <> p_cGroupSearchFilter AND
         "" <> p_cGroupSearchFilter ) THEN 
        m_cGroupSearchFilter = p_cGroupSearchFilter.

    /* Check to see if we should search for and return group memberships */
    RUN getProperty in m_hProps ( INPUT  "-groups", OUTPUT cPropValue).
    IF (? <> cPropValue ) THEN 
        m_lSearchGroups = YES.

    /* See if we have an attribute list to return */
    RUN getProperty in m_hProps ( INPUT  "-attrs", OUTPUT cPropValue).
    IF (? <> cPropValue ) THEN 
        m_cAttrList = cPropValue.

    /* Begin the process of initializing the settings to use when we first
    ** connect to the LDAP directory.  Note: this does not do a connection
    ** at this time. The LDAP shared library will do that automatically when
    ** it needs to.  */
    DO WHILE (NOT lDone ) :
        lDone = YES.

        /* Check for required values */
        IF (? = m_cBindServer OR
            "" = m_cBindServer ) THEN DO:
            cRetValue = "Missing required LDAP bind server name.".
            LEAVE.
        END.

        /* Create the LDAP server bind specification. */
        cBindSpec = m_cBindServer + ":" + m_cBindPort.

        /* Initialize the LDAP context */
        IF (m_lTrace) THEN 
            MESSAGE "trace: Initializing LDAP ...".
    
        /* Initialize the LDAP connection information */
        RUN ldap_init  ( INPUT  m_cBindServer,
                         INPUT  INTEGER(m_cBindPort),
                         OUTPUT m_mLDAPContext ).
    
        /* Set the standard LDAP options. */
        IF (m_lTrace) THEN 
            MESSAGE "trace: Setting LDAP version 3...".
        PUT-LONG(mOptionValue,1) = {&LDAP_VERSION3}.
        RUN ldap_set_option ( INPUT  m_mLDAPContext,
                              INPUT  {&LDAP_OPT_VERSION},
                              INPUT  mOptionValue,
                              OUTPUT iStatus) NO-ERROR.
        IF ( 0 <> iStatus ) THEN DO:
            RUN ldap_err2string( INPUT  iStatus, OUTPUT m_mLastLDAPError ) /* NO-ERROR */.
            cRetValue = "Error in LDAP set option (version3) : " + 
                    GET-STRING(m_mLastLDAPError,1).
            LEAVE.
        END.
        
        /* Now see if there are any other optional LDAP options to set */
        RUN getProperty in m_hProps ( INPUT  "-groups", OUTPUT cPropList).
        IF (? <> cPropList ) THEN DO:
            DEFINE VARIABLE i                   AS INTEGER NO-UNDO.
            DEFINE VARIABLE cPropName           AS CHARACTER NO-UNDO.
            DEFINE VARIABLE iPropCount          AS INTEGER NO-UNDO.
            DEFINE VARIABLE cOptNumber          AS CHARACTER NO-UNDO.
            DEFINE VARIABLE iOptNumber          AS INTEGER NO-UNDO.

            iPropCount = NUM-ENTRIES(cPropList, ",").
            DO i = 1 TO iPropCount :
                cPropName = ENTRY(i, cPropList, ",").
                IF ( 8 < LENGTH(cPropName) AND
                     "-ldapopt" = SUBSTRING(cPropName, 1, 8) ) THEN DO:
                    cOptNumber = SUBSTRING(cPropName, 9).
                    iOptNumber = INTEGER(cOptNumber) NO-ERROR.
                    IF ( ERROR-STATUS:ERROR ) THEN DO:
                        IF (m_lDebug) THEN DO:
                            MESSAGE "LDAP option name " + cPropName + " was invalid".
                            NEXT.
                        END.
                    END.
                    RUN getProperty in m_hProps ( INPUT  cPropName, OUTPUT cPropValue).
                    IF ( ? <> cPropValue AND
                         "" <> cPropValue) THEN DO:
                        /* Load the option value to set */
                        IF ( "YES" = cPropValue ) THEN DO:
                            PUT-LONG(mOptionValue,1) = {&LDAP_OPT_ON}.
                        END. ELSE IF ( "NO" = cPropValue ) THEN DO:
                            PUT-LONG(mOptionValue,1) = {&LDAP_OPT_OFF}.
                        END. ELSE IF ( "0" >= SUBSTRING(cPropValue, 1, 1) AND
                                       "9" <= SUBSTRING(cPropValue, 1, 1) ) THEN DO:
                            PUT-LONG(mOptionValue,1) = INTEGER(cPropValue).
                        END. ELSE DO:
                            PUT-STRING(mOptionValue,1) = cPropValue.
                        END.

                        /* Set the value */
                        RUN ldap_set_option ( INPUT  m_mLDAPContext,
                                              INPUT  iOptNumber,
                                              INPUT  mOptionValue,
                                              OUTPUT iStatus) NO-ERROR.

                        /* Check the status of the option call */
                        IF ( 0 <> iStatus ) THEN DO:
                            RUN ldap_err2string( INPUT  iStatus, 
                                                  OUTPUT m_mLastLDAPError ) /* NO-ERROR */.
                            cRetValue = "Error in LDAP set option number (" +
                                        cOptNumber + ") : " + 
                                        GET-STRING(m_mLastLDAPError,1).
                        END.
                    END. ELSE DO:
                        IF (m_lDebug) THEN DO:
                            MESSAGE "LDAP option " + cPropName + " had no value".
                        END.   
                    END.
                END.
            END.
        END.

    END.

    /* Release OS pointer storage */
    SET-SIZE(mOptionValue) = 0.

    RETURN cRetValue.
END PROCEDURE.

/*
** PROGRAM     : finalize
** DESCRIPTION : Rundown the procedure by releasing all allocated resources
**  and freeing all allocated memory
**      
** PARAMETERS  :
**      none.
** SIDE EFFECTS:
**      none.
** RETURN-VALUE: "" for success, otherwise returns the failure reason.
*/
PROCEDURE finalize :
    /* Procedure local variables */
    DEFINE VARIABLE cRetValue AS CHARACTER INITIAL "".
    
    /* If we are fully bound to the server, disconnect */
    IF ( m_lBindComplete ) THEN DO:
        DEFINE VARIABLE iStatus             AS INTEGER NO-UNDO.

        IF (m_lTrace) THEN 
            MESSAGE "trace: Unbinding from LDAP server. ".

        RUN ldap_unbind_s ( INPUT  m_mLDAPContext,
                            OUTPUT iStatus) NO-ERROR.
    END.

    /* If we have an LDAP context, free memory */
    IF ( 0 < GET-SIZE(m_mLDAPContext) ) THEN DO:
        SET-SIZE(m_mLDAPContext) = 0.
    END.

    /* Destroy the properties object here */
    IF ( ? <> m_hProps ) THEN DO:
        DELETE PROCEDURE m_hProps.
    END.

    RETURN cRetValue.
END PROCEDURE.


/******************************************************************************/
/******************************************************************************/
/* ********************  EXTERNAL PROCEDURES/FUNCTIONS ********************** */
/******************************************************************************/
/******************************************************************************/
 
PROCEDURE ldap_init EXTERNAL "libldap.so" PERSISTENT CDECL.
    DEFINE INPUT  PARAMETER p_cServerPort    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER p_mPort          AS LONG NO-UNDO.
    DEFINE RETURN PARAMETER p_mLDAPContext   AS MEMPTR NO-UNDO.
END PROCEDURE.

PROCEDURE ldap_set_option EXTERNAL "libldap.so" PERSISTENT CDECL.
    DEFINE INPUT  PARAMETER p_mLDAPContext      AS MEMPTR NO-UNDO.
    DEFINE INPUT  PARAMETER p_iLDAPOption       AS LONG NO-UNDO.
    DEFINE INPUT  PARAMETER p_mOptionValue      AS MEMPTR NO-UNDO.
    DEFINE RETURN PARAMETER p_iStatus           AS LONG NO-UNDO.
END PROCEDURE.

PROCEDURE ldap_get_option EXTERNAL "libldap.so" PERSISTENT CDECL.
    DEFINE INPUT  PARAMETER p_mLDAPContext      AS MEMPTR NO-UNDO.
    DEFINE INPUT  PARAMETER p_iLDAPOption       AS LONG NO-UNDO.
    DEFINE OUTPUT PARAMETER p_mOptionValue      AS MEMPTR NO-UNDO.
    DEFINE RETURN PARAMETER p_iStatus           AS LONG NO-UNDO.
END PROCEDURE.

PROCEDURE ldap_bind_s EXTERNAL "libldap.so" PERSISTENT CDECL.
    DEFINE INPUT  PARAMETER p_mLDAPContext      AS MEMPTR NO-UNDO.
    DEFINE INPUT  PARAMETER p_cBindUserDN       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER p_cBindUserPwd      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER p_iAuthMethod       AS LONG NO-UNDO.
    DEFINE RETURN PARAMETER p_iStatus           AS LONG NO-UNDO.
END PROCEDURE.

PROCEDURE ldap_search_s EXTERNAL "libldap.so" PERSISTENT CDECL.
    DEFINE INPUT  PARAMETER p_mLDAPContext      AS MEMPTR NO-UNDO.
    DEFINE INPUT  PARAMETER p_cSearchRoot       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER p_iScope            AS LONG NO-UNDO.
    DEFINE INPUT  PARAMETER p_cSearchFilter     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER p_mAttrArray        AS MEMPTR NO-UNDO.
    DEFINE INPUT  PARAMETER p_iAttrsOnly        AS LONG NO-UNDO.
    DEFINE OUTPUT  PARAMETER p_mLDAPMessage      AS MEMPTR NO-UNDO.
    /* DEFINE OUTPUT PARAMETER p_mLDAPMessage AS HANDLE TO LONG NO-UNDO. */
    DEFINE RETURN PARAMETER p_iStatus           AS LONG NO-UNDO.
END PROCEDURE.

PROCEDURE ldap_unbind_s EXTERNAL "libldap.so" PERSISTENT CDECL.
    DEFINE INPUT  PARAMETER p_mLDAPContext      AS MEMPTR NO-UNDO.
    DEFINE RETURN PARAMETER p_iStatus           AS LONG NO-UNDO.
END PROCEDURE.

PROCEDURE ldap_count_entries EXTERNAL "libldap.so" PERSISTENT CDECL.
    DEFINE INPUT  PARAMETER p_mLDAPContext      AS MEMPTR NO-UNDO.
    DEFINE INPUT  PARAMETER p_mLDAPMessage      AS MEMPTR NO-UNDO.
    DEFINE RETURN PARAMETER p_iStatus           AS LONG NO-UNDO.
END PROCEDURE.

PROCEDURE ldap_msgfree EXTERNAL "libldap.so" PERSISTENT CDECL.
    DEFINE INPUT  PARAMETER p_mLDAPMessage      AS MEMPTR NO-UNDO.
    DEFINE RETURN PARAMETER p_iStatus           AS LONG NO-UNDO.
END PROCEDURE.

PROCEDURE ldap_first_entry EXTERNAL "libldap.so" PERSISTENT CDECL.
    DEFINE INPUT  PARAMETER p_mLDAPContext      AS MEMPTR NO-UNDO.
    DEFINE OUTPUT PARAMETER p_mLDAPMessage      AS MEMPTR NO-UNDO.
    DEFINE RETURN PARAMETER p_mLDAPEntry        AS MEMPTR NO-UNDO.
END PROCEDURE.

PROCEDURE ldap_next_entry EXTERNAL "libldap.so" PERSISTENT CDECL.
    DEFINE INPUT  PARAMETER p_mLDAPContext      AS MEMPTR NO-UNDO.
    DEFINE INPUT  PARAMETER p_mLDAPMessage      AS MEMPTR NO-UNDO.
    DEFINE RETURN PARAMETER p_mLDAPEntry        AS HANDLE TO INT64 NO-UNDO.
END PROCEDURE.

PROCEDURE ldap_first_attribute EXTERNAL "libldap.so" PERSISTENT CDECL.
    DEFINE INPUT  PARAMETER p_mLDAPContext      AS MEMPTR NO-UNDO.
    DEFINE INPUT  PARAMETER p_mLDAPMessage      AS MEMPTR NO-UNDO.
    DEFINE OUTPUT PARAMETER p_mCtxPtr           AS MEMPTR NO-UNDO.
    DEFINE RETURN PARAMETER p_mAttrName         AS MEMPTR NO-UNDO.
END PROCEDURE.

PROCEDURE ldap_next_attribute EXTERNAL "libldap.so" PERSISTENT CDECL.
    DEFINE INPUT        PARAMETER p_mLDAPContext      AS MEMPTR NO-UNDO.
    DEFINE INPUT        PARAMETER p_mLDAPMessage      AS MEMPTR NO-UNDO.
    DEFINE INPUT        PARAMETER p_mCtxPtr           AS MEMPTR NO-UNDO.
    DEFINE RETURN       PARAMETER p_mAttrName         AS MEMPTR NO-UNDO.
END PROCEDURE.

PROCEDURE ldap_get_dn EXTERNAL "libldap.so" PERSISTENT.
    DEFINE INPUT  PARAMETER p_mLDAPContext      AS MEMPTR NO-UNDO.
    DEFINE INPUT  PARAMETER p_mLDAPMessage      AS MEMPTR NO-UNDO.
    DEFINE RETURN PARAMETER p_mAttrName         AS MEMPTR NO-UNDO.
END PROCEDURE.

PROCEDURE ldap_memfree EXTERNAL "libldap.so" PERSISTENT CDECL.
    DEFINE INPUT PARAMETER p_mAttrMemory        AS MEMPTR NO-UNDO.
END PROCEDURE.

PROCEDURE ldap_get_values EXTERNAL "libldap.so" PERSISTENT CDECL.
    DEFINE INPUT  PARAMETER p_mLDAPContext      AS MEMPTR NO-UNDO.
    DEFINE INPUT  PARAMETER p_mLDAPMessage      AS MEMPTR NO-UNDO.
    DEFINE INPUT  PARAMETER p_mAttrName         AS MEMPTR NO-UNDO.
    DEFINE RETURN PARAMETER p_mAttrValues       AS MEMPTR NO-UNDO.
END PROCEDURE.

PROCEDURE ldap_value_free EXTERNAL "libldap.so" PERSISTENT CDECL.
    DEFINE INPUT  PARAMETER p_mValueMemory      AS MEMPTR NO-UNDO.
    DEFINE RETURN PARAMETER p_iStatus           AS LONG NO-UNDO.
END PROCEDURE.

PROCEDURE ldap_count_values EXTERNAL "libldap.so" PERSISTENT CDECL.
    DEFINE INPUT  PARAMETER p_mValueMemory      AS MEMPTR NO-UNDO.
    DEFINE RETURN PARAMETER p_iCount            AS LONG NO-UNDO.
END PROCEDURE.

PROCEDURE ldap_err2string EXTERNAL "libldap.so" PERSISTENT CDECL.
    DEFINE INPUT  PARAMETER p_iErrorCode        AS LONG NO-UNDO.
    DEFINE RETURN PARAMETER p_mErrorString      AS MEMPTR NO-UNDO.
END PROCEDURE.

PROCEDURE ber_free EXTERNAL "libldap.so" PERSISTENT CDECL.
    DEFINE INPUT PARAMETER p_mBERCtx           AS MEMPTR NO-UNDO.
    DEFINE INPUT PARAMETER p_iFreeStruct        AS LONG NO-UNDO.
END PROCEDURE.