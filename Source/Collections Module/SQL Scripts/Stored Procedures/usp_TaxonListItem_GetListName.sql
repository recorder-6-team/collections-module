SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_TaxonListItem_GetListName') IS NOT NULL
	DROP PROCEDURE dbo.usp_TaxonListItem_GetListName
GO

/*============================================================================*\
  Description:

  Parameters:

  Created:

  Last revision information:
	$Revision: 1 $
	$Date: 23/11/09 16:50 $
	$Author: Simonwood $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_TaxonListItem_GetListName
	@TaxonListItemKey					CHAR(16),
	@TaxonListName						VARCHAR(200) OUTPUT
AS
	SELECT		@TaxonListName				=	l.ITEM_NAME
	FROM		TAXON_LIST_ITEM				AS	i
	INNER JOIN	TAXON_LIST_VERSION			AS	lv
	ON			lv.TAXON_LIST_VERSION_KEY	=	i.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_LIST					AS	l
	ON			l.TAXON_LIST_KEY			=	lv.TAXON_LIST_KEY
	WHERE		i.TAXON_LIST_ITEM_KEY		=	@TaxonListItemKey
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_TaxonListItem_GetListName') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_TaxonListItem_GetListName'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
			GRANT EXECUTE ON dbo.usp_TaxonListItem_GetListName TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_GetListName TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_GetListName TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_GetListName TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_GetListName TO R2k_RecordCardsOnly
END
GO