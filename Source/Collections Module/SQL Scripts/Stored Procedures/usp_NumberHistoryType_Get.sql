/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_NumberHistoryType_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_NumberHistoryType_Get]
GO

/*===========================================================================*\
  Description:	Checks to see if frame NumberHistory cmbType is set to
		Registration Number or any term that has the same meaning.

  Parameters:	@Key	Specimen Collection Unit key
		@IsRegType Specifies whether the type of number is a Registration Number

  Created:	Setember 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 16:39 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_NumberHistoryType_Get]
	@Key char(16),
	@IsRegType bit output
AS

SET NOCOUNT ON

	IF EXISTS(
		
		SELECT		*
		
		FROM		Collection_Unit_Number AS CUN
			
		INNER JOIN	Concept AS C ON C.Concept_Key = 'SYSTEM0000000001' -- Registration Number
				OR C.Meaning_Key = 'SYSTEM0000000001' -- Registration Number		
		INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = CUN.Collection_Unit_Key		
	
		WHERE 		CUN.Collection_Unit_Key = @Key)
	
		SET @IsRegType=1
	
	ELSE
		SET @IsRegType=0


SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_NumberHistoryType_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_NumberHistoryType_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_NumberHistoryType_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_NumberHistoryType_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_NumberHistoryType_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_NumberHistoryType_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_NumberHistoryType_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_NumberHistoryType_Get TO [Dev - JNCC SQL]
END

GO