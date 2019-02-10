/*------------------------------------------------------------------------
    File        : properties.i 
    Purpose     : Storage and access to simple name=value character value
                  properties.

    Syntax      : {include/properties.i m_hProps}
                  run common/Properties PERSISTENT SET hProps
                                        (propfile,
                                         hParentProps) NO-ERROR.

                

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
DEFINE VARIABLE {1}                                         AS HANDLE NO-UNDO.  

/* ********************  Preprocessor Definitions  ******************** */

/* *********************** Procedure Settings ************************ */

/* ***************************  Main Block  *************************** */

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

