/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRanks_FixDomain]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRanks_FixDomain]
GO

/*===========================================================================*\
  Description:	When pasting concepts into a concept group, ensure that the
		ranks are also copied

  Parameters:	@FixDomainKey - domain key which should be checked for fixing
							@SessionID
							@SystemSuppliedData - both used only if new ranks must be created.

  Created:	July 2004

  Last revision information:
    $Revision: 4 $
    $Date: 4/10/05 14:42 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRanks_FixDomain]
	@FixDomainKey CHAR(16),	
	@SessionID CHAR(16),
	@SystemSuppliedData bit
AS

BEGIN TRANSACTION

-- cursor variables
DECLARE	@ConceptRankKey CHAR(16)

DECLARE @ExistingRankKey CHAR(16)
DECLARE @NewRankKey CHAR(16)

DECLARE csr CURSOR FOR
	SELECT distinct
		C.Concept_Rank_Key
	FROM Concept c
	INNER JOIN Concept_Rank cr ON cr.Concept_Rank_Key=c.Concept_Rank_Key
	INNER JOIN concept_group cg ON cg.Concept_Group_Key=c.Concept_Group_Key
	INNER JOIN Local_Domain ld ON ld.Local_Domain_Key=cg.Local_Domain_Key
	WHERE ld.Domain_Key<>cr.Domain_Key
	AND LD.Domain_Key=@FixDomainKey
OPEN csr

WHILE (1=1)
BEGIN

	FETCH NEXT FROM csr 
	INTO @ConceptRankKey

	IF @@FETCH_STATUS <>0 BREAK

	EXECUTE spNextKey 'Concept_Rank', @NewRankKey OUTPUT

	INSERT INTO Concept_Rank (
				Concept_Rank_Key,
				Domain_Key,
				Item_Name, 
				Sort_Order, 
				Abbreviation, 
				Color_R, 
				Color_G, 
				Color_B, 
				Entered_Session_ID,
				System_Supplied_Data,
				Custodian)
			SELECT 
				@NewRankKey, 
				@FixDomainKey, 
				Item_Name, 
				Sort_Order, 
				Abbreviation, 
				Color_R, 
				Color_G, 
				Color_B, 
				@SessionID,
				@SystemSuppliedData,
				LEFT(@NewRankKey, 8)
			FROM Concept_Rank
			WHERE Concept_Rank_Key=@ConceptRankKey
	IF @@Error<>0 GOTO Rollbackandexit

	UPDATE C
	SET C.Concept_Rank_Key=@NewRankKey
	FROM Concept C
	INNER JOIN Concept_Group CG ON CG.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Local_Domain LD ON LD.Local_Domain_Key=CG.Local_Domain_Key
	WHERE C.Concept_Rank_Key=@ConceptRankKey
	AND LD.Domain_Key=@FixDomainKey

END


CLOSE csr
DEALLOCATE csr
   


COMMIT TRANSACTION
RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRanks_FixDomain') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRanks_FixDomain'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRanks_FixDomain TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_FixDomain TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_FixDomain TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_FixDomain TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRanks_FixDomain TO [Dev - JNCC SQL]
END
GO