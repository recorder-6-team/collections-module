/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetConservationStatus]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetConservationStatus
GO

/*===========================================================================*\
  Description:	Returns Conservation Status

  Parameters:	@StatusIndex		- Status Index

  Created:	18 September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/05/04 11:01 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_GetConservationStatus(
  @StatusIndex TINYINT
)
RETURNS varchar(50)

AS
BEGIN

	DECLARE @Output VARCHAR(10)
	DECLARE @Concept_Key CHAR(16)
	SET @Output = 
		CASE @StatusIndex
			WHEN 0 THEN 'Pending'
			WHEN 1 THEN 'Open'
			WHEN 2 THEN 'Closed'
			WHEN 3 THEN 'Postponed'
			WHEN 4 THEN 'Abandoned'
		END

  RETURN @Output

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetConservationStatus]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetConservationStatus'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetConservationStatus TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetConservationStatus TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetConservationStatus TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetConservationStatus TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetConservationStatus TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetConservationStatus TO [Dev - JNCC SQL]
	END
GO