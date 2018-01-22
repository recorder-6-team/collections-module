/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetSpecimenLabelTypeName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetSpecimenLabelTypeName
GO

/*===========================================================================*\
  Description:	Returns the Specimen Label/Inscription type as a string

  Parameters:	@IsInscription - Determines if a label or inscription

  Created:	2 January 2004

  Last revision information:
    $Revision: 1 $
    $Date: 12/01/04 12:13 $
    $Author: Bencollier $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetSpecimenLabelTypeName]
(
@IsInscription BIT
)
RETURNS VARCHAR(15)

AS
BEGIN

DECLARE @ReturnValue VARCHAR(15)

	SET @ReturnValue = 
	CASE @IsInscription
		WHEN 0 THEN 'Label'
		WHEN 1 THEN 'Inscription'
	END

RETURN @ReturnValue
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetSpecimenLabelTypeName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetSpecimenLabelTypeName'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.ufn_GetSpecimenLabelTypeName TO [Dev - JNCC SQL]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetSpecimenLabelTypeName TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetSpecimenLabelTypeName TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetSpecimenLabelTypeName TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetSpecimenLabelTypeName TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetSpecimenLabelTypeName TO [R2k_RecordCardsOnly]
	END
GO
