IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QESessionLastUser_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QESessionLastUser_Select]
GO
    

/*===========================================================================*\
  Description:	Returns the last user and datetime edited of a quick entry session

  Parameters:	
		@QESessionKey - @QE_SEssion_Key

  Created:	March 2014

  Last revision information:
    $Revision: 2 $
    $Date: 18/02/15 19:31 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessionLastUser_Select]
  @QESessionKey as int
AS

SET NOCOUNT ON
IF EXISTS (
	SELECT QS.Changed_Session_ID
	FROM QE_Session QS
	WHERE QS.QE_Session_Key = @QESessionKey
    AND QS.Changed_Session_ID IS NOT NULL)

    SELECT dbo.ufn_GetFormattedName(S.User_Name_Key) + 
			' - (' + CAST (ISNULL(S.[Date_Time_End],[Date_Time_Start]) as varchar(20))  + ' )'  AS [User_Name]
	FROM QE_Session QS
	INNER JOIN Session S 
		ON QS.Changed_Session_ID = S.Session_ID
	WHERE QS.QE_Session_Key = @QESessionKey
ELSE
	SELECT dbo.ufn_GetFormattedName(S.User_Name_Key) + 
	' - (' + CAST (ISNULL(S.[Date_Time_End],[Date_Time_Start]) as varchar(20))  + ' )' AS [User_Name]
	FROM QE_Session QS
	INNER JOIN Session S 
		ON QS.Entered_Session_ID = S.Session_ID
	WHERE QS.QE_Session_Key = @QESessionKey  
		
GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessionLastUser_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESessionLastUser_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_QESessionLastUser_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessionLastUser_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessionLastUser_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessionLastUser_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessionLastUser_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_QESessionLastUser_Select TO [Dev - JNCC SQL]
END

GO