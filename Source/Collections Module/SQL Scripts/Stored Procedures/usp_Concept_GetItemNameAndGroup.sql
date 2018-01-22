SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Concept_GetItemNameAndGroup') IS NOT NULL
	DROP PROCEDURE dbo.usp_Concept_GetItemNameAndGroup
GO

/*============================================================================*\
  Description:	Returns the actual name and group name of a concept.

  Parameters:	@Key					Concept key
				
  Created:		November 2009

  Last revision information:
	$Revision: 1 $
	$Date: 23/11/09 16:41 $
	$Author: Simonwood $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Concept_GetItemNameAndGroup
	@Key								CHAR(16)
AS
	SELECT		dbo.ufn_ConceptItemName_Get(
						@Key,
						0,
						0,
						0)				AS	Concept_Name,
				g.Item_Name				AS	Group_Name
	FROM		Concept					AS	c
	INNER JOIN	Concept_Group			AS	g
	ON			g.Concept_Group_Key		=	c.Concept_Group_Key
	WHERE		c.Concept_Key			=	@Key
GO

/*============================================================================*\
  Grant permissions
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Concept_GetItemNameAndGroup') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Concept_GetItemNameAndGroup'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
			GRANT EXECUTE ON dbo.usp_Concept_GetItemNameAndGroup TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_GetItemNameAndGroup TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_GetItemNameAndGroup TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_GetItemNameAndGroup TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_GetItemNameAndGroup TO R2k_RecordCardsOnly
END
GO
