/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementMaterialDetail_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementMaterialDetail_Update]
GO

/*===========================================================================*\
  Description:	Updates records in the Movement_Of_Material table.

  Parameters:	@Key	Collection key

  Created:	September 2003

  Last revision information:
    $Revision: 6 $
    $Date: 3/02/09 9:50 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementMaterialDetail_Update]
	@Key char(16), -- The key of the leafnode to be updated.
	@ContactNameKey char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@Completed bit,
	@ReceiverNameKey char(17),	-- Must be 17 because organisations will have a * on the end.
	@ValueAmount money,
	@CurrencyConceptKey char(16),
	@AcquisitionMethodConceptKey char(16),
	@Notes text,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @RefinedReceiverNameKey varchar(16)
	DECLARE @RefinedDepartmentKey varchar(16)
	DECLARE @MovementDirectionKey varchar(16)

	/*-------------------------------------------------------------*\
	  Need to get the MovementDirectionKey.
	\*-------------------------------------------------------------*/
	SET @MovementDirectionKey = (SELECT Movement_Direction_Key
					FROM Movement_Of_Material
					WHERE Movement_Of_Material_Key = @Key)

	/*-------------------------------------------------------------*\
	  If the @ReceiverNameKey doesn't have a '*' at the end of it,
	  then it is the key of and individual or an organisation.
	\*-------------------------------------------------------------*/
	IF CHARINDEX('*', @ReceiverNameKey)=0
	BEGIN
		SET @RefinedReceiverNameKey = @ReceiverNameKey
		SET @RefinedDepartmentKey = NULL
	END
	ELSE
 	BEGIN
		SET @RefinedReceiverNameKey =  (SELECT Name_Key 
			    			FROM Organisation_Department
			     			WHERE Organisation_Department_Key = 
							LEFT(@ReceiverNameKey, CHARINDEX('*', @ReceiverNameKey)-1))
		SET @RefinedDepartmentKey = LEFT(@ReceiverNameKey, CHARINDEX('*', @ReceiverNameKey)-1)
	END	

	BEGIN TRANSACTION

		UPDATE 	Movement_Of_Material
		SET	Movement_Direction_Key = @MovementDirectionKey,
			Contact_Name_Key = @ContactNameKey,
			Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Vague_Date_Type = @VagueDateType,
			Completed = @Completed,
			Receiver_Name_Key = @RefinedReceiverNameKey,
			Receiver_Organisation_Department_Key = @RefinedDepartmentKey,
			Value_Amount = @ValueAmount,
			Currency_Concept_Key = @CurrencyConceptKey,
			Acquisition_Method_Concept_Key = @AcquisitionMethodConceptKey,
			Notes = @Notes,
			Changed_Session_ID = @SessionID

		WHERE	Movement_Of_Material_Key = @Key
		AND		[Timestamp] = @Timestamp
		
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Movement_Of_Material WHERE Movement_Of_Material_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementMaterialDetail_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementMaterialDetail_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Update TO [Dev - JNCC SQL]
END
GO