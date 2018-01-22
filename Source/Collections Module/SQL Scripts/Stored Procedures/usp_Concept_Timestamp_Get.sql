If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Concept_Timestamp_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Concept_Timestamp_Get]
GO

/*===========================================================================*\
  Description:  Get the timestamp for a Concept

  Parameters:   @Key - Concept Key
				@Timestamp - timestamp

  Created:		September 2004

  Last revision information:
    $Revision: 1 $
    $Date: 23/09/04 15:10 $
    $Author: Anthonysimpson $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Concept_Timestamp_Get] 
	@Key CHAR(16),
	@Timestamp TIMESTAMP OUTPUT
AS

	SELECT 	@Timestamp = [Timestamp]
	FROM	Concept
	WHERE	Concept_Key = @Key
		
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Timestamp_Get') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_Timestamp_Get'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
            GRANT EXECUTE ON dbo.usp_Concept_Timestamp_Get TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Concept_Timestamp_Get TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Concept_Timestamp_Get TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Timestamp_Get TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Timestamp_Get TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Concept_Timestamp_Get TO [Dev - JNCC SQL]
END

GO