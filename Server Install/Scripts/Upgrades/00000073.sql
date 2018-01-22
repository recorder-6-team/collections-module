/*===========================================================================*\
  Description:
	Update usp_Movement_Update to fix bug involving saving the ReturnComplete and
	LoanComplete fields. Previously this could lead to problems with these fields
	not being saved.

	Add one published term rule from CCN 165.

	Make PublishToWeb parameter optional in usp_Specimen_Insert to address VI 29892

  Created:
	March 2014


\*===========================================================================*/

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Movement_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Movement_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Movement table.

  Parameters:	@Key
		@MovementType
		@OtherPartyNameKey
		@StaffResponsibleNameKey
		@ContactNameKey
		@ExpVagueDateStart
		@ExpVagueDateEnd
		@ExpVagueDateType
		@LoanVagueDateStart
		@LoanVagueDateEnd
		@LoanComplete
		@ReturnVagueDateStart
		@ReturnVagueDateEnd
		@ReturnVagueDateType
		@ReturnComplete
		@Number
		@Notes
		@SessionID
		@Timestamp 

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 8/05/14 13:44 $
    $Author: Brynhorsfieldschonhut $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Movement_Update]
	@Key char(16) OUTPUT,
	@MovementType tinyint, 
	@StaffResponsibleNameKey char(16),
	@OtherPartyNameKey char(16),
	@ContactNameKey char(16),
	@VagueDateStart int, 
	@VagueDateEnd int, 
	@VagueDateType varchar(2),
	@LoanVagueDateStart int = null,
	@LoanVagueDateEnd int = null,
	@LoanVagueDateType varchar(2) = null,
	@LoanComplete bit = null,
	@ReturnVagueDateStart int = null,
	@ReturnVagueDateEnd int = null,
	@ReturnVagueDateType varchar(2) = null,
	@ReturnComplete bit = null,
	@Number varchar(30),
	@Notes text,
	@SessionID char(16), 
	@Timestamp timestamp,
	@RecordsAffected int OUTPUT

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @Error int
	DECLARE @InboundMovementKey char(16), @OutboundMovementKey char(16)

	SET @InboundMovementKey = (SELECT MOM.Movement_Of_Material_Key
								FROM Movement_Of_Material MOM
								INNER JOIN Movement_Direction MD
								ON MOM.Movement_Direction_Key = MD.Movement_Direction_Key
								WHERE MD.Movement_Key = @Key AND MD.OutBound = 0)
	SET @OutboundMovementKey = (SELECT MOM.Movement_Of_Material_Key
								FROM Movement_Of_Material MOM
								INNER JOIN Movement_Direction MD
								ON MOM.Movement_Direction_Key = MD.Movement_Direction_Key
								WHERE MD.Movement_Key = @Key AND MD.OutBound = 1)
 

	BEGIN TRANSACTION
		
		UPDATE 	Movement
		SET 	Movement_Type = @MovementType, 
			Staff_Responsible_Name_Key = @StaffResponsibleNameKey,			
			Other_Party_Name_Key = @OtherPartyNameKey,
			Contact_Name_Key = @ContactNameKey,
			Exp_Vague_Date_Start = @VagueDateStart, 
			Exp_Vague_Date_End = @VagueDateEnd, 
			Exp_Vague_Date_Type = @VagueDateType,
			Number = @Number, 
			Notes = @Notes, 
			Changed_Session_ID = @SessionID

		WHERE	Movement_Key = @Key
		AND		[Timestamp] = @Timestamp

		SELECT	@RecordsAffected = @@RowCount,
			@Error = @@Error

		IF @Error <> 0 GOTO RollbackAndExit

		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Movement WHERE Movement_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		IF @MovementType = 2
		BEGIN
			-- Loan In
			IF @LoanVagueDateStart IS NOT NULL AND @LoanVagueDateEnd IS NOT NULL AND @LoanVagueDateType IS NOT NULL AND @LoanComplete IS NOT NULL
			BEGIN
				UPDATE Movement_Of_Material
				SET Vague_Date_Start = @LoanVagueDateStart,
					Vague_Date_End = @LoanVagueDateEnd,
					Vague_Date_Type = @LoanVagueDateType,
					Completed = @LoanComplete,
					Changed_Session_ID = @SessionID
				WHERE Movement_Of_Material_Key = @InboundMovementKey

				SELECT	@RecordsAffected = @RecordsAffected + @@RowCount,
					 @Error = @@Error

				IF @Error <> 0 GOTO RollbackAndExit

				IF @RecordsAffected = 0 AND EXISTS(SELECT 1 
											FROM Movement_Of_Material
										 WHERE Movement_Of_Material_Key = @InboundMovementKey)
				BEGIN
					RAISERROR('Record updated by another user', 16, 1)
					GOTO RollbackAndExit
				END
			END
			ELSE IF @LoanComplete IS NOT NULL
			BEGIN
				UPDATE Movement_Of_Material
				SET Completed = @LoanComplete,
					Changed_Session_ID = @SessionID
				WHERE Movement_Of_Material_Key = @InboundMovementKey

				SELECT	@RecordsAffected = @RecordsAffected + @@RowCount,
					 @Error = @@Error

				IF @Error <> 0 GOTO RollbackAndExit

				IF @RecordsAffected = 0 AND EXISTS(SELECT 1 
											FROM Movement_Of_Material
										 WHERE Movement_Of_Material_Key = @OutboundMovementKey)
				BEGIN
					RAISERROR('Record updated by another user', 16, 1)
					GOTO RollbackAndExit
				END	
			END	

			IF @ReturnVagueDateStart IS NOT NULL AND @ReturnVagueDateEnd IS NOT NULL AND @ReturnVagueDateType IS NOT NULL AND @ReturnComplete IS NOT NULL
			BEGIN
				UPDATE Movement_Of_Material
				SET Vague_Date_Start = @ReturnVagueDateStart,
					Vague_Date_End = @ReturnVagueDateEnd,
					Vague_Date_Type = @ReturnVagueDateType,
					Completed = @ReturnComplete,
					Changed_Session_ID = @SessionID
				WHERE Movement_Of_Material_Key = @OutboundMovementKey

				SELECT	@RecordsAffected = @RecordsAffected + @@RowCount,
					 @Error = @@Error

				IF @Error <> 0 GOTO RollbackAndExit

				IF @RecordsAffected = 0 AND EXISTS(SELECT 1 
											FROM Movement_Of_Material
										 WHERE Movement_Of_Material_Key = @OutboundMovementKey)
				BEGIN
					RAISERROR('Record updated by another user', 16, 1)
					GOTO RollbackAndExit
				END
			END
			ELSE IF @ReturnComplete IS NOT NULL
			BEGIN
				UPDATE Movement_Of_Material
				SET Completed = @ReturnComplete,
					Changed_Session_ID = @SessionID
				WHERE Movement_Of_Material_Key = @OutboundMovementKey

				SELECT	@RecordsAffected = @RecordsAffected + @@RowCount,
					 @Error = @@Error

				IF @Error <> 0 GOTO RollbackAndExit

				IF @RecordsAffected = 0 AND EXISTS(SELECT 1 
											FROM Movement_Of_Material
										 WHERE Movement_Of_Material_Key = @OutboundMovementKey)
				BEGIN
					RAISERROR('Record updated by another user', 16, 1)
					GOTO RollbackAndExit
				END				
			END			
		END

		IF @MovementType = 3
		BEGIN
			-- Loan Out
			IF @ReturnVagueDateStart IS NOT NULL AND @ReturnVagueDateEnd IS NOT NULL AND @ReturnVagueDateType IS NOT NULL AND @ReturnComplete IS NOT NULL
			BEGIN
				UPDATE Movement_Of_Material
				SET Vague_Date_Start = @ReturnVagueDateStart,
					Vague_Date_End = @ReturnVagueDateEnd,
					Vague_Date_Type = @ReturnVagueDateType,
					Completed = @ReturnComplete,
					Changed_Session_ID = @SessionID
				WHERE Movement_Of_Material_Key = @InboundMovementKey

				SELECT	@RecordsAffected = @RecordsAffected + @@RowCount,
					 @Error = @@Error

				IF @Error <> 0 GOTO RollbackAndExit

				IF @RecordsAffected = 0 AND EXISTS(SELECT 1 
											FROM Movement_Of_Material
										 WHERE Movement_Of_Material_Key = @InboundMovementKey)
				BEGIN
					RAISERROR('Record updated by another user', 16, 1)
					GOTO RollbackAndExit
				END
			END
			ELSE IF @ReturnComplete IS NOT NULL
			BEGIN
				UPDATE Movement_Of_Material
				SET Completed = @ReturnComplete,
					Changed_Session_ID = @SessionID
				WHERE Movement_Of_Material_Key = @InboundMovementKey

				SELECT	@RecordsAffected = @RecordsAffected + @@RowCount,
					 @Error = @@Error

				IF @Error <> 0 GOTO RollbackAndExit

				IF @RecordsAffected = 0 AND EXISTS(SELECT 1 
											FROM Movement_Of_Material
										 WHERE Movement_Of_Material_Key = @OutboundMovementKey)
				BEGIN
					RAISERROR('Record updated by another user', 16, 1)
					GOTO RollbackAndExit
				END				
			END	

			IF @LoanVagueDateStart IS NOT NULL AND @LoanVagueDateEnd IS NOT NULL AND @LoanVagueDateType IS NOT NULL AND @LoanComplete IS NOT NULL
			BEGIN
				UPDATE Movement_Of_Material
				SET Vague_Date_Start = @LoanVagueDateStart,
					Vague_Date_End = @LoanVagueDateEnd,
					Vague_Date_Type = @LoanVagueDateType,
					Completed = @LoanComplete,
					Changed_Session_ID = @SessionID
				WHERE Movement_Of_Material_Key = @OutboundMovementKey

				SELECT	@RecordsAffected = @RecordsAffected + @@RowCount,
					 @Error = @@Error

				IF @Error <> 0 GOTO RollbackAndExit

				IF @RecordsAffected = 0 AND EXISTS(SELECT 1 
											FROM Movement_Of_Material
										 WHERE Movement_Of_Material_Key = @OutboundMovementKey)
				BEGIN
					RAISERROR('Record updated by another user', 16, 1)
					GOTO RollbackAndExit
				END	
			END
			ELSE IF @LoanComplete IS NOT NULL
			BEGIN
				UPDATE Movement_Of_Material
				SET Completed = @LoanComplete,
					Changed_Session_ID = @SessionID
				WHERE Movement_Of_Material_Key = @OutboundMovementKey

				SELECT	@RecordsAffected = @RecordsAffected + @@RowCount,
					 @Error = @@Error

				IF @Error <> 0 GOTO RollbackAndExit

				IF @RecordsAffected = 0 AND EXISTS(SELECT 1 
											FROM Movement_Of_Material
										 WHERE Movement_Of_Material_Key = @OutboundMovementKey)
				BEGIN
					RAISERROR('Record updated by another user', 16, 1)
					GOTO RollbackAndExit
				END	
			END	
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movement_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movement_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movement_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movement_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movement_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movement_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movement_Update TO [Dev - JNCC SQL]
END
GO

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.ufn_GeneratePublishedTermBasicTermRankAuthority') IS NOT NULL
	DROP FUNCTION dbo.ufn_GeneratePublishedTermBasicTermRankAuthority
GO

/*============================================================================*\
	Description:
		Function used to calculate a published term in the format
		<i>[Basic Term]</i> [Rank Abbreviation] [ConceptGroup.Authority from lineage]
		See CCN 165

	Created: February 2014

	Last revision information:
		$Revision: 3 $
		$Date: 8/05/14 13:44 $
		$Author: Brynhorsfieldschonhut $
\*============================================================================*/

CREATE FUNCTION dbo.ufn_GeneratePublishedTermBasicTermRankAuthority
(
	@Plaintext NVARCHAR(150),
	@AuthorAndDate VARCHAR(100),
	@Attributes VARCHAR(100),
	@RankKey CHAR(16),
	@ParentConceptKey CHAR(16)
)
RETURNS NVARCHAR(256)
AS
BEGIN
	DECLARE @RankAbbreviation  VARCHAR(10)
	DECLARE @ParentAuthority VARCHAR(100)

	SET @RankAbbreviation = (SELECT Abbreviation
		FROM dbo.Concept_Rank
		WHERE Concept_Rank_Key = @RankKey)
	SET @ParentAuthority = (SELECT cg.Authority
		FROM dbo.Concept_Group cg
		INNER JOIN dbo.Concept c
		ON cg.Concept_Group_Key = c.Concept_Group_Key
		WHERE c.Concept_Key = @ParentConceptKey)

	RETURN '<i>' + @Plaintext + '</i> ' + ISNULL(@RankAbbreviation, '') + ' ' + ISNULL(@ParentAuthority, '')
END
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on function ufn_GeneratePublishedTermBasicTermRankAuthority'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermBasicTermRankAuthority TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermBasicTermRankAuthority TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermBasicTermRankAuthority TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermBasicTermRankAuthority TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermBasicTermRankAuthority TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermBasicTermRankAuthority TO "Dev - JNCC SQL"
GO

IF NOT EXISTS (SELECT 1 
				FROM dbo.Term_Generator 
				WHERE Item_Name = 'Basic Term Rank Authority')
BEGIN
	DECLARE @TermGeneratorKey CHAR(16)
	EXECUTE spNextKey 'Term_Generator', @TermGeneratorKey OUTPUT
	SELECT @TermGeneratorKey
	INSERT INTO dbo.Term_Generator(
		Term_Generator_Key,
		Item_Name, 
		Published_Term_Function, 
		Search_Term_Procedure)
	VALUES (
		@TermGeneratorKey,
		'Basic Term Rank Authority', 
		'dbo.ufn_GeneratePublishedTermBasicTermRankAuthority', 
		'dbo.usp_Search_Term_GenerateTermsUsingDefaultRule')
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Specimen_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Specimen_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record in the Specimen table and a preferred 
		determination in the Determination table.
		Ensures the Domain mask of the specimen is also updated.

  Parameters:	@Key 
		@ParentCollectionCollectionUnitKey 
		@SpecimenTypeConceptKey 
		@Confidential 
		@Dangerous
		@PublishToWeb
		@LifeSciences 
		@Checked 
		@CurrentContainerCollectionUnitKey 
		@CurrentLocationCode
		@UsualContainerCollectionUnitKey
		@UsualLocationCode 
		@SessionID

  Created:	July 2003

  Last revision information:
    $Revision: 3 $
    $Date: 8/05/14 13:44 $
    $Author: Brynhorsfieldschonhut $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimen_Insert]
	--for specimen_unit
	@Key char(16) OUTPUT,
	@ExistingCollectionUnitKey char(16) = NULL,
	@ParentCollectionCollectionUnitKey char(16),
	@SpecimenTypeConceptKey char(16),
	@Confidential bit,
	@Dangerous bit,
	@PublishToWeb bit = 0,
	@LifeSciences bit,
	@Checked bit,
	--for Collection_Unit
	@CurrentContainerCollectionUnitKey char(16),
	@CurrentLocationCode varchar(30),
	@UsualContainerCollectionUnitKey char(16),
	@UsualLocationCode varchar(30),
	-- Both
	@SessionID char(16),
	@InternalUse bit
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	SET XACT_ABORT ON
	
	BEGIN TRANSACTION
	
		/*-------------------------------------------------------------*\
		  Get the concept mask.
		\*-------------------------------------------------------------*/
		DECLARE @SpecimenMask int
		EXECUTE	usp_Get_Concept_Domain_Mask @SpecimenTypeConceptKey, @SpecimenMask OUTPUT

		IF @SpecimenMask IS NULL
			SET @SpecimenMask=0

		IF @ExistingCollectionUnitKey IS NULL BEGIN
			/*-------------------------------------------------------------*\
			  Get a new key.
			\*-------------------------------------------------------------*/
			EXECUTE spNextKey 'Collection_Unit', @Key OUTPUT
			/*-------------------------------------------------------------*\
			  Insert in Collection_Unit first.
			\*-------------------------------------------------------------*/
			INSERT INTO Collection_Unit (
				Collection_Unit_Key, 
				Current_Container_Collection_Unit_Key, 
				Current_Location_Code,
				Usual_Container_Collection_Unit_Key, 
				Usual_Location_Code, 
				Domain_Mask,
				Entered_Session_ID
			) VALUES (
				@Key, 
				@CurrentContainerCollectionUnitKey, 
				@CurrentLocationCode, 
				@UsualContainerCollectionUnitKey, 
				@UsualLocationCode, 
				@SpecimenMask, -- Domain_Mask is empty for new Collections
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit		
		END
		ELSE
			SET @Key = @ExistingCollectionUnitKey

		INSERT INTO Specimen_Unit (
			Collection_Unit_Key, 
			Parent_Collection_Collection_Unit_Key,
			Specimen_Type_Concept_Key,
			Confidential,
			Dangerous,
			Publish_To_Web, 
			Life_Sciences,
			Entered_Session_ID,
			Checked,
			Internal_Use)
		 VALUES (
			@Key, 
			@ParentCollectionCollectionUnitKey,
			@SpecimenTypeConceptKey,
			IsNull(@Confidential, 0),
			IsNull(@Dangerous, 0),
			IsNull(@PublishToWeb, 0),
			@LifeSciences,
			@SessionID,
			@Checked,
			@InternalUse 
		)
	
		IF @@Error <> 0 GOTO RollbackAndExit

		--The following line removed by Polly Shaw
		--EXECUTE usp_Determination_Insert @NewDeterminationKey, @NewSpecimenKey, @ConceptKey
	
		/*-------------------------------------------------------------*\
		  And switch bits ON.
		\*-------------------------------------------------------------*/
		-- Update the *new* container mask
		EXECUTE	usp_CollectionUnit_Update_DomainMask @CurrentContainerCollectionUnitKey, @SpecimenMask, 1
		-- Update the *new* collection mask
		EXECUTE	usp_Collection_Update_DomainMask @ParentCollectionCollectionUnitKey, @SpecimenMask, 1

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimen_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimen_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimen_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimen_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimen_Insert TO [Dev - JNCC SQL]
END
GO