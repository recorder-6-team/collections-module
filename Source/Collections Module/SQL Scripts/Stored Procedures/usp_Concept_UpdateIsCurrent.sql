/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_UpdateIsCurrent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_UpdateIsCurrent]
GO

/*===========================================================================*\
  Description:	Update the Is_Current flag for a concept

  Parameters:	@ConceptKey
		@ConceptGroupKey OPTIONAL

	

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 1/04/04 11:08 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_UpdateIsCurrent]
	@ConceptKey CHAR(16),
	@ConceptGroupKey CHAR(16) = NULL

AS
	DECLARE 
		@LatestIntroduction INT,
		@LatestExpiry INT,
		@LatestVersion INT,
		@IsCurrent BIT

	IF @ConceptGroupKey IS NULL
		SELECT @ConceptGroupKey=Concept_Group_Key	
		FROM Concept
		WHERE Concept_Key=@ConceptKey
	
	SELECT @LatestIntroduction = MAX(CGV.[Sequence]) 
	FROM Concept_History CH 
	INNER JOIN Concept_Group_Version CGV ON CGV.Concept_Group_Version_Key = CH.Concept_Group_Version_From
	WHERE CH.Concept_Key=@ConceptKey
	
	SELECT @LatestExpiry = MAX(CGV.[Sequence]) 
	FROM Concept_History CH 
	INNER JOIN Concept_Group_Version CGV ON CGV.Concept_Group_Version_Key = CH.Concept_Group_Version_To
	WHERE CH.Concept_Key=@ConceptKey

	IF @LatestExpiry IS NULL
		SET @IsCurrent=1
	ELSE IF (@LatestIntroduction IS NULL) OR (@LatestIntroduction <= @LatestExpiry)
	BEGIN
		-- Concept is expired, unless the expiry is actually the last concept group version
		SELECT @LatestVersion=MAX([Sequence])
		FROM Concept_Group_Version 
		WHERE Concept_Group_Key=@ConceptGroupKey
		
		IF @LatestVersion>@LatestExpiry 
			SET @IsCurrent=0
		ELSE
			SET @IsCurrent=1
	END
	ELSE 
		SET @IsCurrent=1

	UPDATE Concept 
	SET Is_Current = @IsCurrent
	WHERE Concept_Key=@ConceptKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_UpdateIsCurrent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_UpdateIsCurrent'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateIsCurrent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateIsCurrent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_UpdateIsCurrent TO [Dev - JNCC SQL]
END