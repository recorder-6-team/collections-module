/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetVagueDateIntegerFromDate]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetVagueDateIntegerFromDate
GO

/*===========================================================================*\
  Description:	Returns the the integer part of a Vague date. Only intended to 
				work for post 1753 dates.

  Parameters:	@DateTime - Input Date

  Created:	07 October 2003

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/04 11:01 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetVagueDateIntegerFromDate]
(
@DateTime DATETIME
)
RETURNS INT

AS
BEGIN
	RETURN CAST(@DateTime AS INT) + 2 --Difference between Date cast as integer by Delphi SQL server
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetVagueDateIntegerFromDate]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetVagueDateIntegerFromDate'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetVagueDateIntegerFromDate TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetVagueDateIntegerFromDate TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetVagueDateIntegerFromDate TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetVagueDateIntegerFromDate TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetVagueDateIntegerFromDate TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetVagueDateIntegerFromDate TO [Dev - JNCC SQL]
	END
GO
