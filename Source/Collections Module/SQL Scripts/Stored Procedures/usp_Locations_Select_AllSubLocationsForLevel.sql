/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Locations_Select_AllSubLocationsForLevel') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].usp_Locations_Select_AllSubLocationsForLevel
GO

/*===========================================================================*\
  Description:	Returns All locations and features below a given location.

  Parameters:
	@Key	Location key

  Created:	November 2009

  Last revision information:
    $Revision: 1 $
    $Date: 20/11/09 15:28 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_Locations_Select_AllSubLocationsForLevel
	@Key 	CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE	@Results
	TABLE 
	(
		ItemKey	CHAR(16) COLLATE Database_Default
	)
	
	INSERT INTO		@Results
	SELECT			Location_Key
	FROM			dbo.Location
	WHERE			Location_Key		=		@Key
	
	-- Gather hierarchy
	WHILE @@RowCount > 0
		INSERT INTO		@Results
		SELECT			Location_Key
		FROM			dbo.Location
		JOIN			@Results
		ON				ItemKey			=		Parent_Key
		-- Don't want to loop forever!
		WHERE			Location_Key	NOT IN	(SELECT ItemKey FROM @Results)
	
	SELECT	ItemKey
	FROM	@Results
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Locations_Select_AllSubLocationsForLevel') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Locations_Select_AllSubLocationsForLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Locations_Select_AllSubLocationsForLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Locations_Select_AllSubLocationsForLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Locations_Select_AllSubLocationsForLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Locations_Select_AllSubLocationsForLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Locations_Select_AllSubLocationsForLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Locations_Select_AllSubLocationsForLevel TO [Dev - JNCC SQL]
END
GO
