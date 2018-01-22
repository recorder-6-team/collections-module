If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_DeterminationName_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_DeterminationName_Get]
GO

/*===========================================================================*\
  Description:	Gets the determination name given a determination key for a
		life science or earth science determination.

  Parameters:	@Key
		@IsLifeScience
		@Caption 	OUTPUT	

  Created:	November 2003

  Last revision information:
    $Revision: 3 $
    $Date: 26/08/11 14:56 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_DeterminationName_Get] 
@Key CHAR(16),
@IsLifeScience bit,
@Caption VARCHAR(100) OUTPUT

AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	IF @IsLifeScience = 1 
		SELECT		@Caption = Preferred_Name
		FROM		Index_Taxon_Name
		WHERE 		Taxon_List_Item_Key = @Key
	ELSE
		SELECT		@Caption = Item_Name
		FROM 		VW_ConceptTerm CT
		WHERE 		Concept_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationName_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DeterminationName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DeterminationName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DeterminationName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DeterminationName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DeterminationName_Get TO [Dev - JNCC SQL]
END

GO