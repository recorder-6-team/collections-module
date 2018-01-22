If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_InscriptionsAndLabels_Select_ForSpecimen]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_InscriptionsAndLabels_Select_ForSpecimen]
GO

CREATE PROCEDURE [dbo].[usp_InscriptionsAndLabels_Select_ForSpecimen] 
@ParentKey CHAR(16)

AS

--  DESCRIPTION
--  Returns Inscriptions and Labels for a specified Specimen
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@ParentKey 			Only the records associated with the parent key are returned
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-27
--
SET NOCOUNT ON
SELECT 
	Specimen_Label_Key AS Item_Key, 
	Specimen_Label_Key AS Join_Key,
	Inscription AS Item_Name
FROM 
	SPECIMEN_LABEL
WHERE Collection_Unit_Key = @ParentKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_InscriptionsAndLabels_Select_ForSpecimen') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_InscriptionsAndLabels_Select_ForSpecimen'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_InscriptionsAndLabels_Select_ForSpecimen TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_InscriptionsAndLabels_Select_ForSpecimen TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_InscriptionsAndLabels_Select_ForSpecimen TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_InscriptionsAndLabels_Select_ForSpecimen TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_InscriptionsAndLabels_Select_ForSpecimen TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_InscriptionsAndLabels_Select_ForSpecimen TO [Dev - JNCC SQL]
END

GO