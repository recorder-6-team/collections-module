IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_DragDropKey_Get') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_FieldData_DragDropKey_Get]
GO

/*===========================================================================*\
  Description:	Returns the key used for drag and drop given a Specimen_Field_Data_Key.

  Parameters:	@Key 

  Created:	January 2004

  Last revision information:
    $Revision: 2 $
    $Date: 21/07/04 16:52 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FieldData_DragDropKey_Get] 
	@Key CHAR(16),
	@DragDropKey CHAR(16) OUTPUT
AS
	SET NOCOUNT ON
	SELECT 		@DragDropKey = 
			CASE 
				WHEN O.Sample_Key IS NULL THEN XO.Sample_Key
				ELSE O.Sample_Key
			END
	FROM		Specimen_Field_Data SFD 
	LEFT JOIN 	Occurrence O ON SFD.Occurrence_Key = O.Occurrence_Key 
	LEFT JOIN 	Taxon_Occurrence XO ON SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key
	WHERE		Specimen_Field_Data_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_DragDropKey_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldData_DragDropKey_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldData_DragDropKey_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldData_DragDropKey_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldData_DragDropKey_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_DragDropKey_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_DragDropKey_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldData_DragDropKey_Get TO [Dev - JNCC SQL]
END
GO