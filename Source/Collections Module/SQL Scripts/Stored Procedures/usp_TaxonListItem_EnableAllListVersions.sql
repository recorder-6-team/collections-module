/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF OBJECT_ID('dbo.usp_TaxonListItem_EnableAllListVersions') IS NOT NULL
	DROP PROCEDURE dbo.usp_TaxonListItem_EnableAllListVersions
GO

/*===========================================================================*\
  Description:
	Enable data entry for all taxa across all list versions linked to the 
	given list.

  Parameters:	@taxon_list_key	Taxon List key

  Created:		Oct 2008

  Last revision information:
	$Revision: 3 $
	$Date: 18/05/09 15:12 $
	$Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_TaxonListItem_EnableAllListVersions
	@taxon_list_key CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE @Expired TABLE (
		Taxon_Version_Key	CHAR(16) COLLATE Database_Default,
		Version				INT
	)

	DECLARE @LatestListVersionKey CHAR(16)
	SELECT	@LatestListVersionKey = Taxon_List_Version_Key
	FROM	Taxon_List_Version TLV
	WHERE 	Taxon_List_Key = @taxon_List_key
	AND		Version >= (
				SELECT 	MAX(Version)
				FROM 	Taxon_List_Version
				WHERE 	Taxon_List_Key = @taxon_List_key
				AND 	Version_Is_Amendment = 0
			)

	-- Get the latest version of expired items.
	INSERT INTO @Expired (Taxon_Version_Key, Version)
	SELECT	TLI1.Taxon_Version_Key, MAX(TLV1.Version)
	FROM	Taxon_List_Item		TLI1
	JOIN	Taxon_List_Version	TLV1	ON	TLV1.Taxon_List_Version_Key = TLI1.Taxon_List_Version_Key
										AND	TLV1.Taxon_List_Key			= @taxon_list_key
	WHERE	TLI1.Taxon_Version_Key NOT IN (
		SELECT	TLI2.Taxon_Version_Key
		FROM	Taxon_List_Item		TLI2
		JOIN	Taxon_List_Version	TLV2	ON	TLV2.Taxon_List_Version_Key = TLI2.Taxon_List_Version_Key
											AND	TLV2.Taxon_List_Key			= @taxon_list_key
		AND		TLI2.Taxon_List_Version_To 	IS 	NULL
		AND		TLI2.Taxon_List_Version_Key = 	@LatestListVersionKey
	)
	GROUP BY TLI1.Taxon_Version_Key

	-- Now set the Taxon_List_Version_To to NULL to "re-enable" data entry for these.
	UPDATE	TLI
	SET		Taxon_List_Version_To 		= 	NULL,
			Taxon_List_Version_Key		= 	@LatestListVersionKey
	FROM	@Expired			E
	JOIN	Taxon_List_Item		TLI	ON 	TLI.Taxon_Version_Key		= E.Taxon_Version_Key
	JOIN	Taxon_List_Version	TLV	ON	TLV.Taxon_List_Version_Key 	= TLI.Taxon_List_Version_Key
									AND	TLV.Version					= E.Version
									AND	TLV.Taxon_List_Key			= @taxon_list_key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_TaxonListItem_EnableAllListVersions'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_TaxonListItem_EnableAllListVersions TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_TaxonListItem_EnableAllListVersions TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	GRANT EXECUTE ON dbo.usp_TaxonListItem_EnableAllListVersions TO "Dev - JNCC SQL"
GO