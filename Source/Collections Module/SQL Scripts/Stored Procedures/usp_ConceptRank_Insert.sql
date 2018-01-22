/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRank_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRank_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Concept_Rank table

  Parameters:	@Key OUTPUT
		@DomainKey 
		@ItemName 
		@Sort_Order 
		@Abbreviation
		@ColorR 
		@ColorG 
		@ColorB 
		@SessionID 
		@SystemSuppliedData 

  Created:	January 2004

  Last revision information:
    $Revision: 1 $
    $Date: 29/01/04 17:02 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRank_Insert]
	@Key char(16) OUTPUT,
	@DomainKey char(16),
	@ItemName varchar(100),
	@SortOrder int = NULL,
	@Abbreviation varchar(10),
	@ColorR tinyint,
	@ColorG tinyint,
	@ColorB tinyint,
	@SessionID char(16),
	@SystemSuppliedData bit = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Concept_Rank', @Key OUTPUT

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Concept_Rank.
		\*-------------------------------------------------------------*/
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
			System_Supplied_Data			
		) VALUES (
			@Key,
			@DomainKey,
			@ItemName,
			@SortOrder,
			@Abbreviation,
			@ColorR,
			@ColorG,
			@ColorB,
			@SessionID,
			IsNull(@SystemSuppliedData, 0)		
		)
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRank_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRank_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRank_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRank_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRank_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRank_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRank_Insert TO [Dev - JNCC SQL]
END
GO