If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Det_Key_From_Determination_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Det_Key_From_Determination_Get]
GO

CREATE PROCEDURE [dbo].[usp_Det_Key_From_Determination_Get] 
@A_Determination_Key CHAR(16),
@IsLifeSciences BIT,
@Det_Key CHAR(16) OUTPUT

AS

--  DESCRIPTION
--  Returns Concept_Key or Taxon_List_Item_Key for a given determination. (Used for rendering common names).
--
--	PARAMETERS
--	NAME					DESCRIPTION
--	@A_Determination_Key	Either a Determination_Key or Taxon_Determinaton_Key
--	@IsLifeSciences			Specifies whether the record to be found is Life or Earth Sciences
--	@Det_Key				Corresponding Concept_Key or Taxon_List_Item_Key
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			26/01/2004
--

IF @IsLifeSciences = 0
	SELECT @Det_Key = Concept_Key
	FROM
		Determination
	WHERE Determination_Key = @A_Determination_Key
ELSE
	SELECT @Det_Key = Taxon_List_Item_Key
	FROM
		Taxon_Determination
	WHERE Taxon_Determination_Key = @A_Determination_Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Det_Key_From_Determination_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Det_Key_From_Determination_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Det_Key_From_Determination_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Det_Key_From_Determination_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Det_Key_From_Determination_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Det_Key_From_Determination_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Det_Key_From_Determination_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Det_Key_From_Determination_Get TO [Dev - JNCC SQL]
END

GO