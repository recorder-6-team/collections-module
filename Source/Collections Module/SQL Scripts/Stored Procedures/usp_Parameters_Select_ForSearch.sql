/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Parameters_Select_ForSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Parameters_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Searches for Parameters from one or two concept groups.

  Parameters:	@SearchText
		@SearchKey	33 chars, 'ConcepGroupKey1;ConceptGroupKey2'
				(16 + 1 + 16)

  Created:	November 2003

  Last revision information:
    $Revision: 4 $
    $Date: 25/11/03 14:11 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Parameters_Select_ForSearch] 
	@SearchText varchar(100),
	@SearchKey char(33)
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	DECLARE @ConceptGroupKey1 char(16)
	DECLARE @ConceptGroupKey2 char(16)

	IF CharIndex(';', @SearchKey) = 0
	BEGIN
		SET @ConceptGroupKey1 = @SearchKey
		SET @ConceptGroupKey2 = ''
	END ELSE BEGIN
		SET @ConceptGroupKey1 = RTrim(Left(@SearchKey, CharIndex(';', @SearchKey)-1))
		SET @ConceptGroupKey2 = LTrim(Right(@SearchKey, Len(@SearchKey) - CharIndex(';', @SearchKey)))
	END

	SELECT	Concept_Key AS Item_Key, 
		Item_Name AS DisplayTerm, 
		Item_Name AS SearchTerm,
		Concept_Group_Key
	FROM 	VW_ConceptTerm
	WHERE 	Concept_Group_Key IN (@ConceptGroupKey1, @ConceptGroupKey2)
	AND 	Item_Name LIKE @SearchText + '%'
	ORDER BY Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Parameters_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Parameters_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Parameters_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Parameters_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Parameters_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Parameters_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Parameters_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Parameters_Select_ForSearch TO [Dev - JNCC SQL]
END
GO