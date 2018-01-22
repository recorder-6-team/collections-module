If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConceptRanks_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConceptRanks_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Search proc for Concept Ranks. If a duplicate is found,
				the Domain name is appended.

  Parameters:	@SearchText

  Created:		September 2004

  Last revision information:
    $Revision: 1 $
    $Date: 7/09/04 16:28 $
    $Author: Anthonysimpson $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_ConceptRanks_Select_ForSearch] 
	@SearchText varchar(100)
AS

SET NOCOUNT ON

	/*----------------------------------------------------------*\
	  Declare the local variables, cursors and table variables.
	\*----------------------------------------------------------*/
	DECLARE @CurrentTitle varchar(113), 
			@CurrentConceptRankKey char(16),
			@CurrentDomainTitle varchar(100),
			@CurrentConceptRankName varchar(100)
	
	DECLARE	@NextTitle varchar(113),
			@NextConceptRankKey char(16),
			@NextDomainTitle varchar(100),
			@NextConceptRankName varchar(100)
	
	-- Select all of the Concept Ranks
	DECLARE curConceptRanks CURSOR LOCAL FAST_FORWARD FOR
		SELECT 		CR.Concept_Rank_Key, 
					CR.Abbreviation + ' - ' + CR.Item_Name AS Title,					
					D.Item_Name,
					CR.Item_Name AS Concept_Rank_Name
		FROM 		Concept_Rank AS CR
		INNER JOIN	Domain AS D ON D.Domain_Key = CR.Domain_Key
		ORDER BY	Title
	
	-- Create a table variable to store the result set in.
	DECLARE @tableResultSet TABLE(
		Concept_Rank_Key char(16),
		Title varchar(216),
		Concept_Rank_Name varchar(100)
	)
	
	DECLARE @MatchFound bit
	SET 	@MatchFound = 0
	
	OPEN curConceptRanks
	
	/*-------------------------------------------------*\
	  Set up the local variables with initial values.
	\*-------------------------------------------------*/
	FETCH NEXT 
	FROM	curConceptRanks
	INTO	@CurrentConceptRankKey, @CurrentTitle, @CurrentDomainTitle, @CurrentConceptRankName
	
	FETCH NEXT
	FROM	curConceptRanks
	INTO	@NextConceptRankKey, @NextTitle, @NextDomainTitle, @NextConceptRankName
	
	/*-------------------------------------------------*\
	  Start going through the cursor
	\*-------------------------------------------------*/
	WHILE @@Fetch_Status = 0
	BEGIN
		-- If a match has been found between the current and the next title, append the Domain name
		IF @CurrentTitle = @NextTitle
		BEGIN
			INSERT INTO @tableResultSet (Concept_Rank_Key, Title, Concept_Rank_Name) 
			VALUES (@CurrentConceptRankKey, @CurrentTitle + ' [' + @CurrentDomainTitle + ']', @CurrentConceptRankName)
	
			SET @MatchFound = 1
		END	
		-- If a match was found between the previous row and this row, add the Domain name
		ELSE IF @MatchFound = 1
		BEGIN
			INSERT INTO @tableResultSet (Concept_Rank_Key, Title, Concept_Rank_Name) 
			VALUES (@CurrentConceptRankKey, @CurrentTitle + ' [' + @CurrentDomainTitle + ']', @CurrentConceptRankName)
	
			SET @MatchFound = 0
		END
		-- If no match was found, don't bother adding the Domain name
		ELSE
			INSERT INTO @tableResultSet (Concept_Rank_Key, Title, Concept_Rank_Name) 
			VALUES (@CurrentConceptRankKey, @CurrentTitle, @CurrentConceptRankName)
		
		-- The next row's details have already been obtained, so put them into the local variables
		-- as the new current row
		SELECT 	@CurrentConceptRankKey = @NextConceptRankKey,
				@CurrentTitle = @NextTitle,
				@CurrentDomainTitle = @NextDomainTitle,
				@CurrentConceptRankName = @NextConceptRankName
		
		-- Get a new next row's details.
		FETCH NEXT
		FROM	curConceptRanks
		INTO	@NextConceptRankKey, @NextTitle, @NextDomainTitle, @NextConceptRankName
	END
	
	CLOSE 		curConceptRanks
	DEALLOCATE 	curConceptRanks
	
	/*-----------------------------------------------------------------------*\
	  Select the values from the temporary table that match the search text.
	\*-----------------------------------------------------------------------*/
	SELECT 	Concept_Rank_Key AS Item_Key, 
			Title AS DisplayTerm,
			Title COLLATE SQL_Latin1_General_CP1_CI_AS AS SearchTerm
	FROM 	@tableResultSet
	WHERE	Title LIKE @SearchText + '%'
	OR		Concept_Rank_Name LIKE @SearchText + '%'
	
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRanks_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRanks_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRanks_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ConceptRanks_Select_ForSearch TO [Dev - JNCC SQL]
END

GO
