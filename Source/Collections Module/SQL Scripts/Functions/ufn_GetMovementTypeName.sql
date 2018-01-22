/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetMovementTypeName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetMovementTypeName
GO

/*===========================================================================*\
  Description:	Returns the Movement_Type_Name as a string

  Parameters:	@MovementType - MovementType as integer

  Created:	22 September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/04 11:01 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetMovementTypeName]
(
@MovementType INT
)
RETURNS VARCHAR(25)

AS
BEGIN

DECLARE @ReturnValue VARCHAR(25)

	SET @ReturnValue = 
	CASE @MovementType
		WHEN 0 THEN 'Accession'
		WHEN 1 THEN 'Exchange'
		WHEN 2 THEN 'Loan In'
		WHEN 3 THEN 'Loan Out'
		WHEN 4 THEN 'Destroyed'
		WHEN 5 THEN 'Disposed'
		WHEN 6 THEN 'Internal Transfer'
		WHEN 7 THEN 'Lost'
		WHEN 8 THEN 'Sold'
		WHEN 9 THEN 'Hosted Material'
	END

RETURN @ReturnValue
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetMovementTypeName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetMovementTypeName'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetMovementTypeName TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetMovementTypeName TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetMovementTypeName TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetMovementTypeName TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetMovementTypeName TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetMovementTypeName TO [Dev - JNCC SQL]
	END
GO
