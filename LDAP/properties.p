/*------------------------------------------------------------------------
    File        : Properties.p 
    Purpose     : Store name-value properties

    Syntax      : run common/Properties PERSISTENT SET hProps
                                        (propfile,
                                         hParentProps) NO-ERROR.

    Description :
                    hProps is the handle holding the resulting properties object
                    propfile is an optional character parameter that points
                        to a properties file to load.  The format of the
                        properties file is:
                            propname=propvalue
                            
                            NOTE: spaces are relevent!!
                    hParentProps is an optional handle to a parent properties
                        object that will be searched if a property is not
                        in the local set.
    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
  
/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER p_cPropFile      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p_hParentProps   AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE m_ttProperties NO-UNDO
    FIELD cPropName     AS CHARACTER
    FIELD cPropValue    AS CHARACTER
    INDEX idxProperties IS UNIQUE PRIMARY cPropName.
    
DEFINE VARIABLE m_lLockProperties   AS LOGICAL INITIAL NO.
DEFINE VARIABLE m_cRetValue         AS CHARACTER INITIAL "" NO-UNDO.    
DEFINE VARIABLE m_hParent           AS HANDLE NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* *********************** Procedure Settings ************************ */

/* ***************************  Main Block  *************************** */
DEFINE VARIABLE lDebug          AS LOGICAL INITIAL NO NO-UNDO.

IF (? <> OS-GETENV("DEBUG_INIT")) THEN
    lDebug = YES.
    
IF (lDebug) THEN 
    MESSAGE "DEBUG(common/Properties.p): Loading properties...".
    
m_hParent = ?.

IF (VALID-HANDLE(p_hParentProps)) THEN 
    m_hParent = p_hParentProps.

IF (? <> p_cPropFile AND
    "" <> p_cPropFile) THEN DO:
    DEFINE VARIABLE cFullPath AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLoadCount AS INTEGER INITIAL 0 NO-UNDO.
    
    cFullPath = SEARCH(p_cPropFile).
    IF (? <> cFullPath) THEN DO:
        IF (lDebug) THEN 
            MESSAGE "DEBUG(common/Properties.p): Loading properties from " + 
                cFullPath.
        INPUT FROM VALUE(cFullPath).
        REPEAT :
            CREATE m_ttProperties.
            IMPORT DELIMITER "=" m_ttProperties NO-ERROR.
            iLoadCount = iLoadCount + 1.    
        END.
        INPUT CLOSE.    
        IF (lDebug) THEN 
            MESSAGE "DEBUG(common/Properties.p): Loaded " + STRING(iLoadCount) + 
                " properties.".
    END.
    ELSE DO:
        m_cRetValue = "Could not find property file " + p_cPropFile.
    END.
    FOR EACH m_ttProperties :
        IF (m_ttProperties.cPropName = ? OR
            m_ttProperties.cPropName = "" ) THEN DO:
            DELETE m_ttProperties NO-ERROR.
        END.
        ELSE DO:
            IF (lDebug) THEN
                MESSAGE "DEBUG(common/Properties.p): Loaded property: " + 
                    m_ttProperties.cPropName + " = " + m_ttProperties.cPropValue.
        END.
    END.
END.

RETURN m_cRetValue.

/******************************************************************************/
/******************************************************************************/
/********    INTERNAL PROCEDURES AND FUNCTIONS                         ********/
/******************************************************************************/
/******************************************************************************/

/*
** PROGRAM     : getProperty
** DESCRIPTION : Return the value of a property name.
**      
** PARAMETERS  :
**      p_cPropName is a required string parameter that holds the property name.
**      p_cPropValue is an output parameter that holds the property's value.  
**              The value may be returned as ? if the property does not exist.
** SIDE EFFECTS:
**      none.
** RETURN-VALUE: "" for success, otherwise returns the failure reason.
*/
PROCEDURE getProperty :
    /* Procedure input/output parameter definitions. */
    DEFINE INPUT PARAMETER  p_cPropName     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER p_cPropValue    AS CHARACTER NO-UNDO.
    
    /* Procedure local variables */
    DEFINE VARIABLE cRetValue       AS CHARACTER INITIAL "Property not found".
    
    IF (? <> p_cPropName AND
        "" <> p_cPropName) THEN DO:
        FIND m_ttProperties WHERE m_ttProperties.cPropName = p_cPropName NO-ERROR.
        IF (AVAILABLE m_ttProperties) THEN DO:
            p_cPropValue = m_ttProperties.cPropValue.
            cRetValue = "".    
        END.
        ELSE DO:
            /* Not found in local properties.  In parent (if exists) */
            IF (? <> m_hParent) THEN DO:
                DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.
                RUN GetProperty IN m_hParent
                                 (INPUT     p_cPropName,
                                  OUTPUT    cValue) NO-ERROR.
                /* return the found/not-found status from the parent. */                  
                cRetValue = RETURN-VALUE.                  
                p_cPropValue = cValue.                      
            END.
            ELSE DO:
                /* Not in parent either, so let the default error return
                 * status stand. */
                p_cPropValue = ?.
            END.        
        END.
    END.
    ELSE DO:
        cRetValue = "Insufficient parameters.".
        p_cPropValue = ?.
    END.
    
    RETURN cRetValue.
END PROCEDURE.

/*
**  PROCEDURE: setProperty
**
**  DESCRIPTION:    Set a property value.   If a previous value was set, and the 
**             properties are not locked, overwrite the old value.
**
** PARAMETERS  :
**      p_cPropName is a required string parameter that holds the property name.
**      p_cPropValue is an output parameter that holds the property's value.  
**
** SIDE EFFECTS:
**      none.
** RETURN-VALUE: "" for success, otherwise returns the failure reason.
**
*/
 PROCEDURE setProperty :
     /* Procedure input/output parameter definitions. */
     DEFINE INPUT PARAMETER  p_cPropName     AS CHARACTER NO-UNDO.
     DEFINE OUTPUT PARAMETER p_cPropValue    AS CHARACTER NO-UNDO.
 
     DEFINE VARIABLE cRetValue      AS CHARACTER INITIAL "" NO-UNDO.
     
     IF (NOT m_lLockProperties) THEN DO:
         IF (? <> p_cPropName AND
             "" <> p_cPropName AND
             ? <> p_cPropValue) THEN DO:
            FIND m_ttProperties WHERE 
                    m_ttProperties.cPropName = p_cPropName NO-ERROR. 
            IF (AVAILABLE m_ttProperties) THEN DO:
                ASSIGN m_ttProperties.cPropValue = p_cPropValue.
                RELEASE m_ttProperties.
            END.    
            ELSE DO:
                CREATE m_ttProperties.
                ASSIGN m_ttProperties.cPropValue = p_cPropValue
                       m_ttProperties.cPropName = p_cPropName.
                RELEASE m_ttProperties.
            END.   
         END.
         ELSE DO:
             cRetValue = "Insufficient parameters.".
         END.
     END.
     ELSE DO:
        cRetValue = "Properties are locked.".
     END.
     
     RETURN(cRetValue).
 END PROCEDURE.
 
 
/*
**  PROCEDURE: enumProperties
**
**  DESCRIPTION:    Return a comma separated list of the existing property
**              names.
**
** PARAMETERS  :
**      p_cPropNames is an output parameter that holds the property names.
**
** SIDE EFFECTS:
**      none.
** RETURN-VALUE: "" for success, otherwise returns the failure reason.
*/
 PROCEDURE enumProperties :
     /* Procedure input/output parameter definitions. */
     DEFINE OUTPUT PARAMETER  p_cPropNames     AS CHARACTER NO-UNDO.
 
     DEFINE VARIABLE cNames                 AS CHARACTER INITIAL "" NO-UNDO.
     DEFINE VARIABLE cRetValue              AS CHARACTER INITIAL "" NO-UNDO.
     
     FOR EACH m_ttProperties :
        IF ("" <> cNames) THEN DO:
            cNames = cNames + "," + m_ttProperties.cPropName.
        END.
        ELSE DO:
            cNames = m_ttProperties.cPropName.
        END.   
     END.
     
     p_cPropNames = cNames.
     
     RETURN(cRetValue).
 END PROCEDURE.
 
/*
** PROGRAM     : lockProperties
** DESCRIPTION : Lock the properties so that they may not be changed.
**      
** PARAMETERS  :
**      none.
** SIDE EFFECTS:
**      none.
** RETURN-VALUE: "" for success, otherwise returns the failure reason.
*/
PROCEDURE lockProperties :
    /* Procedure input/output parameter definitions. */
    
    /* Procedure local variables */
    DEFINE VARIABLE cRetValue AS CHARACTER INITIAL "yes".
    
    m_lLockProperties = YES.
    
    RETURN cRetValue.
END PROCEDURE.

/*
** PROGRAM     : loadFromCmdLine
** DESCRIPTION : Load properties from a command line style input.
**      Option names always are prefixed by a hyphen ("-") and are separated
**      from their optional value or the next option by a single space.
**      
** PARAMETERS  :
**      p_cInputCmdLine     A non-blank character string holding the command
**                          line.
** SIDE EFFECTS:
**      none.
** RETURN-VALUE: "" for success, otherwise returns the failure reason.
*/
PROCEDURE loadFromCmdLine :
    /* Procedure input/output parameter definitions. */
    DEFINE INPUT PARAMETER p_cInputCmdLine              AS CHARACTER NO-UNDO. 

    /* Procedure local variables */
    DEFINE VARIABLE cRetValue                   AS CHARACTER INITIAL "".
    DEFINE VARIABLE cTmp                        AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE iWordCount                  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iWordIdx                    AS INTEGER NO-UNDO.
    DEFINE VARIABLE lLastIsOption               AS LOGICAL INITIAL NO NO-UNDO.
    DEFINE VARIABLE lDebug                      AS LOGICAL INITIAL NO NO-UNDO.

    IF (? <> OS-GETENV("DEBUG_INIT")) THEN
        lDebug = YES.
    
    IF (lDebug) THEN
        MESSAGE "DEBUG(common/Properties.p): Loading command line properties ...".

    IF ( ? <> p_cInputCmdLine AND
         "" <> p_cInputCmdLine ) THEN DO:
        cTmp = p_cInputCmdLine.
        iWordCount = NUM-ENTRIES(cTmp, " ").

        IF (lDebug) THEN
            MESSAGE "DEBUG(common/Properties.p): command line had " +
                    STRING(iWordCount) + " words".
    
        IF (0 < iWordCount) THEN DO:
            DEFINE VARIABLE cCurrentWord    AS CHARACTER NO-UNDO.
            DEFINE VARIABLE cNextWord       AS CHARACTER NO-UNDO.
            DEFINE VARIABLE iState          AS INTEGER INITIAL 0 NO-UNDO.
            DEFINE VARIABLE cChar           AS CHARACTER NO-UNDO.
            DEFINE VARIABLE iDBOptNum       AS INTEGER INITIAL 1 NO-UNDO.
            DEFINE VARIABLE cDBSpec         AS CHARACTER NO-UNDO.
            DEFINE VARIABLE cPropName       AS CHARACTER NO-UNDO.
    
            cDBSpec = "".
            cPropName = "".
            DO iWordIdx = 1 to iWordCount :
                cCurrentWord = ENTRY(iWordIdx, cTmp, " ").
                cChar = SUBSTRING(cCurrentWord, 1, 1).
                IF (cChar = "-" ) THEN DO:
                    /* the current word is an option name */
                        /* an option name, but not a db spec introducer */
                    CASE (iState) :
                        WHEN 0 THEN DO:
                            /* Transition from work to new option name.  Record
                             * the option name to be saved later */
                            cPropName = cCurrentWord.    
                            /* in the option state */   
                            iState = 1.         
                        END.
                        WHEN 1 THEN DO:
                            /* transition from option name to option name. Save 
                             * the option name with a blank value and init the
                             * new option name storage. */
                            CREATE m_ttProperties.
                            ASSIGN  m_ttProperties.cPropValue = "" 
                                    m_ttProperties.cPropName = cPropName.
                            RELEASE m_ttProperties.
                            cPropName = cCurrentWord.    
                            /* in the option state */   
                            iState = 1.         
                        END.
                    END CASE.
                END.    
                ELSE DO:
                    /* not a hyphen'd option name, so what do we do? */
                    CASE (iState) :
                        WHEN 0 THEN DO:
                            /* The last was an word, this is an error */
                            MESSAGE "DEBUG(common/Properties.p): expecting option, got " + 
                                        cCurrentWord.                
                            /* New state is word */
                            iState = 0.                
                        END.
                        WHEN 1 THEN DO:
                            /* transition from option name to option value. Save
                             * the option name and the value of the current word */
                            CREATE m_ttProperties.
                            ASSIGN  m_ttProperties.cPropValue = cCurrentWord
                                    m_ttProperties.cPropName = cPropName.
                            RELEASE m_ttProperties.
                            cPropName = "".
                            /* New state is word */
                            iState = 0.                
                        END.
                    END CASE.
                END.
            END.    

            /* now do any cleanup */
            CASE (iState) :
                WHEN 0 THEN DO:
                    /* the last state was an option value, nothing to do */
                END.
                WHEN 1 THEN DO:
                    /* the last state was an option name, so save it with a blank value. */
                    CREATE m_ttProperties.
                    ASSIGN  m_ttProperties.cPropValue = "" 
                            m_ttProperties.cPropName = cPropName.
                    RELEASE m_ttProperties.
                END.
            END CASE.
    
            /* Debug input */
            IF (lDebug) THEN DO:
                FOR EACH m_ttProperties :
                    IF (m_ttProperties.cPropValue = ?) THEN DO:
                        cTmp = "?".
                    END.    
                    ELSE
                        cTmp = m_ttProperties.cPropValue.
                    IF (lDebug) THEN 
                        MESSAGE "DEBUG(common/Properties.p): loaded option " + 
                                m_ttProperties.cPropName + " " + cTmp.
                END.
            END.
        END.

    
    END.

    RETURN cRetValue.
END PROCEDURE.

 


