/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementMaterialDetail_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementMaterialDetail_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Movement_Of_Material table.

  Parameters:	@Key	Collection key

  Created:	October 2003

  Last revision information:
    $Revision: 6 $
    $Date: 5/04/04 14:37 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementMaterialDetail_Insert]
	@Key char(16) OUTPUT, -- The key of the top level movement node.
	@ParentKey char(16),
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
	@Outbound bit,
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @MovementDirectionKey varchar(16)
	DECLARE @RefinedReceiverNameKey varchar(16)
	DECLARE @RefinedDepartmentKey varchar(16)

	/*-------------------------------------------------------------*\
	  Get the MovementDirectionKey.
	\*-------------------------------------------------------------*/
	SET @MovementDirectionKey = (SELECT 	Movement_Direction_Key
					FROM 	Movement_Direction
					WHERE 	Movement_Key = @ParentKey
					AND	Outbound = @Outbound
				     )

	/*-------------------------------------------------------------*\
	  If the @ReceiverNameKey doesn't have a '*' at the end of it,
	  then it is the key of an individual or an organisation.
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

	EXECUTE spNextKey 'Movement_Of_Material', @Key OUTPUT

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Movement_Of_Material.
		\*-------------------------------------------------------------*/
		INSERT INTO Movement_Of_Material (
			Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
			Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Completed,
			Receiver_Name_Key, Receiver_Organisation_Department_Key, 
			Value_Amount, Currency_Concept_Key, Acquisition_Method_Concept_Key,
			Notes, Entered_Session_ID
		) VALUES (
			@Key, @MovementDirectionKey, @ContactNameKey,
			@VagueDateStart, @VagueDateEnd, @VagueDateType, @Completed,
			@RefinedReceiverNameKey, @RefinedDepartmentKey, 
			@ValueAmount, @CurrencyConceptKey, @AcquisitionMethodConceptKey,
			@Notes, @SessionID
		)		

		IF @@Error <> 0 GOTO RollbackAndExit

		--If this is not the first movement of material, exclude all collection_units from it
		IF EXISTS(SELECT * FROM Movement_Of_Material 
				WHERE (Movement_Of_Material_Key <> @Key) AND (Movement_Direction_Key = @MovementDirectionKey))
		BEGIN
			DECLARE @Collection_Unit_Key CHAR(16)
			DECLARE @Movement_Of_Material_Exclusion_Key CHAR(16)

			DECLARE MOMEcsr CURSOR LOCAL FAST_FORWARD
			FOR 
			SELECT CU.Collection_Unit_Key			
			FROM
				Movement_Collection_Unit MCU
				INNER JOIN Collection_Unit CU ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
			WHERE MCU.Movement_Direction_Key = @MovementDirectionKey
		
			OPEN MOMEcsr
			
			FETCH NEXT FROM MOMEcsr INTO @Collection_Unit_Key
			
			WHILE @@FETCH_STATUS = 0
			BEGIN
			
				EXECUTE spNextKey 'Movement_Of_Material_Exclusion', @Movement_Of_Material_Exclusion_Key OUTPUT				
				INSERT INTO Movement_Of_Material_Exclusion
					(Movement_Of_Material_Exclusion_Key,
					Movement_Of_Material_Key,
					Collection_Unit_Key,
					Entered_Session_ID)
				VALUES
					(@Movement_Of_Material_Exclusion_Key,
					@Key,
					@Collection_Unit_Key,
					@SessionID)

				IF @@Error <> 0 GOTO RollbackAndExit
			
			
			FETCH NEXT FROM MOMEcsr INTO @Collection_Unit_Key
			END --End While
			
			CLOSE MOMEcsr
			DEALLOCATE MOMEcsr
		END


	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementMaterialDetail_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementMaterialDetail_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Insert TO [Dev - JNCC SQL]
END
GO