/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Movement_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Movement_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Movement table

  Parameters:	@Key
		@MovementType
		@OtherPartyNameKey
		@StaffResponsibleNameKey
		@DepartmentKey - if null, obtains department for staff responsible
		@ContactNameKey
		@VagueDateStart
		@VagueDateEnd
		@VagueDateType
		@LoanVagueDateStart
		@LoanVagueDateEnd
		@LoanVagueDateType
		@LoanComplete
		@ActualReturnVagueDateStart
		@ActualReturnVagueDateEnd
		@ActualReturnVagueDateType
		@ReturnComplete
		@Number
		@Notes
		@SessionID

  Created:	September 2003

  Last revision information:
    $Revision: 12 $
    $Date: 18/03/14 13:58 $
    $Author: Brynhorsfieldschonhut $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Movement_Insert]
	@Key char(16) OUTPUT,
	@MovementType tinyint,
	@WithAcquisition bit, 
	@OtherPartyNameKey char(16),
	@StaffResponsibleNameKey char(16),
	@DepartmentKey char(16)=null,
	@ContactNameKey char(16),
	@VagueDateStart int, 
	@VagueDateEnd int, 
	@VagueDateType varchar(2),
	@LoanVagueDateStart int=null, 
	@LoanVagueDateEnd int=null, 
	@LoanVagueDateType varchar(2)=null,
	@LoanComplete bit=null,
	@ActualReturnVagueDateStart int=null, 
	@ActualReturnVagueDateEnd int=null, 
	@ActualReturnVagueDateType varchar(2)=null,
	@ReturnComplete bit=null,
	@Number varchar(30),
	@Notes text,
	@SessionID char(16) 
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Movement', @Key OUTPUT

	DECLARE @MovementDirectionKey char(16),
		@MovementOfOwnershipKey char(16),
		@MovementOfMaterialKey char(16),
		@HoldingOrg char(16)

	SELECT @HoldingOrg = (SELECT Data FROM Setting WHERE Name = 'HoldingOrg')

	IF @DepartmentKey IS NULL
		SELECT 	@DepartmentKey = Organisation_Department_Key
		FROM	Individual
		WHERE	Name_Key = @StaffResponsibleNameKey

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Movement table.
		\*-------------------------------------------------------------*/
		INSERT INTO Movement (
			Movement_Key, Movement_Type, Other_Party_Name_Key, 
			Staff_Responsible_Name_Key, Contact_Name_Key, Exp_Vague_Date_Start, 
			Exp_Vague_Date_End, Exp_Vague_Date_Type, Number, 
			Notes, Entered_Session_ID
			
		) VALUES (
			@Key, @MovementType, @OtherPartyNameKey, 
			@StaffResponsibleNameKey, @ContactNameKey, @VagueDateStart, 
			@VagueDateEnd, @VagueDateType, @Number, 
			@Notes, @SessionID
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*----------------------------------------*\
		   Accession / Accession with Acquisition
		\*----------------------------------------*/
		IF (@MovementType = 0)
		BEGIN
			-- Insert a record into the Movement_Direction table
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@HoldingOrg, 0, @SessionID
			)	

			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit
			
			/*----------------------------------------------------------------*\
		 	   Insert record into the Movement_Of_Material table if it is an
			   Accession with Acquisition.
			\*----------------------------------------------------------------*/			
			IF (@WithAcquisition = 1)
			BEGIN
				EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
				INSERT INTO Movement_Of_Material (
					Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
					Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
					Receiver_Organisation_Department_Key
				) VALUES (
					@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey, 
					@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @StaffResponsibleNameKey,
					@DepartmentKey
				)	
				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END ELSE
		/*----------------*\
		  Exchange
		\*----------------*/
		IF @MovementType = 1
		BEGIN
			/*-------------------------------------------------------------*\
		 	   Inserting records into the tables for the inbound movement.
			\*-------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@HoldingOrg, 0, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @StaffResponsibleNameKey,
				@DepartmentKey
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			/*-------------------------------------------------------------*\
		 	   Inserting records into the tables for the outbound movement.
			\*-------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@OtherPartyNameKey, 1, @SessionID
			)		
			IF @@Error <> 0 GOTO RollbackAndExit
			
			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @OtherPartyNameKey,
				@DepartmentKey
			)	
			IF @@Error <> 0 GOTO RollbackAndExit
		END ELSE
		/*------------*\
		  Loan In 
		\*------------*/
		IF @MovementType = 2
		BEGIN
			/*-------------------------------------------------------------*\
		 	   Inserting records into the tables for the inbound movement.
			\*-------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@HoldingOrg, 0, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit
			
			IF @LoanVagueDateStart IS NOT NULL 
				AND @LoanVagueDateEnd IS NOT NULL 
				AND @LoanVagueDateType IS NOT NULL
			BEGIN
				EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
				INSERT INTO Movement_Of_Material (
					Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key, Completed,
					Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
					Receiver_Organisation_Department_Key
				) VALUES (
					@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey, @LoanComplete,
					@LoanVagueDateStart, @LoanVagueDateEnd, @LoanVagueDateType, @SessionID, @StaffResponsibleNameKey,
					@DepartmentKey
				)	
				IF @@Error <> 0 GOTO RollbackAndExit
			END
			ELSE
			BEGIN				
				EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
				INSERT INTO Movement_Of_Material (
					Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key, Completed,
					Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
					Receiver_Organisation_Department_Key
				) VALUES (
					@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey, @LoanComplete,
					@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @StaffResponsibleNameKey,
					@DepartmentKey
				)	
				IF @@Error <> 0 GOTO RollbackAndExit
			END

			/*----------------------------------------------------------------------*\
		 	  A 'loan in' movement means that the linked items are owned elsewhere.
			  This information is not currently stored anywhere, so create a 
			  Movement_Of_Ownership record so we know the items are owned elsewhere.
			\*----------------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit

			/*-------------------------------------------------------------*\
		 	   Inserting records into the tables for the outbound movement.
			\*-------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@OtherPartyNameKey, 1, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit	
			
			IF @ActualReturnVagueDateStart IS NOT NULL 
				AND @ActualReturnVagueDateEnd IS NOT NULL 
				AND @ActualReturnVagueDateType IS NOT NULL
			BEGIN
				EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
				INSERT INTO Movement_Of_Material (
					Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key, Completed,
					Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
					Receiver_Organisation_Department_Key
				) VALUES (
					@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey, @ReturnComplete,
					@ActualReturnVagueDateStart, @ActualReturnVagueDateEnd, @ActualReturnVagueDateType, @SessionID, @OtherPartyNameKey,
					@DepartmentKey
				)
			END
			ELSE
			BEGIN
				EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
				INSERT INTO Movement_Of_Material (
					Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key, Completed,
					Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
					Receiver_Organisation_Department_Key
				) VALUES (
					@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey, @ReturnComplete,
					@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @OtherPartyNameKey,
					@DepartmentKey
				)
			END	


		END ELSE
		/*----------*\
		  Loan out
		\*----------*/
		IF @MovementType = 3
		BEGIN
			/*-------------------------------------------------------------*\
		 	   Inserting records into the tables for the outbound movement.
			\*-------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@HoldingOrg, 1, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			IF @ActualReturnVagueDateStart IS NOT NULL 
				AND @ActualReturnVagueDateEnd IS NOT NULL 
				AND @ActualReturnVagueDateType IS NOT NULL
			BEGIN
				EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
				INSERT INTO Movement_Of_Material (
					Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key, Completed,
					Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
					Receiver_Organisation_Department_Key
				) VALUES (
					@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey, @ReturnComplete,
					@ActualReturnVagueDateStart, @ActualReturnVagueDateEnd, @ActualReturnVagueDateType, @SessionID, @OtherPartyNameKey,
					@DepartmentKey
				)	
				IF @@Error <> 0 GOTO RollbackAndExit
			END
			ELSE
			BEGIN
				EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
				INSERT INTO Movement_Of_Material (
					Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key, Completed,
					Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
					Receiver_Organisation_Department_Key
				) VALUES (
					@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey, @ReturnComplete,
					@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @OtherPartyNameKey,
					@DepartmentKey
				)	
				IF @@Error <> 0 GOTO RollbackAndExit
			END

			/*-------------------------------------------------------------*\
		 	   Inserting records into the tables for the inbound movement.
			\*-------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@OtherPartyNameKey, 0, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit	
			
			IF @LoanVagueDateStart IS NOT NULL 
				AND @LoanVagueDateEnd IS NOT NULL
				AND @LoanVagueDateType IS NOT NULL
			BEGIN
				EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
				INSERT INTO Movement_Of_Material (
					Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key, Completed,
					Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
					Receiver_Organisation_Department_Key
				) VALUES (
					@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey, @LoanComplete,
					@LoanVagueDateStart, @LoanVagueDateEnd, @LoanVagueDateType, @SessionID, @StaffResponsibleNameKey,
					@DepartmentKey
				)	
				IF @@Error <> 0 GOTO RollbackAndExit
			END
			ELSE
			BEGIN
				EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
				INSERT INTO Movement_Of_Material (
					Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key, Completed,
					Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
					Receiver_Organisation_Department_Key
				) VALUES (
					@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey, @LoanComplete,
					@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @StaffResponsibleNameKey,
					@DepartmentKey
				)	
				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END ELSE
		/*-----------------*\
		  Destroyed / Lost
		\*-----------------*/
		IF @MovementType = 4 OR @MovementType = 7
		BEGIN
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				NULL, 1, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, 
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID,
				Receiver_Organisation_Department_Key 
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, 
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID,
				@DepartmentKey 
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END ELSE
		/*-----------*\
		  Disposed
		\*-----------*/
		IF @MovementType = 5
		BEGIN
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				NULL, 1, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @OtherPartyNameKey,
				@DepartmentKey
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END ELSE
		/*-------------------*\
		  Internal transfer
		\*-------------------*/
		IF @MovementType = 6
		BEGIN
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@HoldingOrg, 0, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @StaffResponsibleNameKey,
				@DepartmentKey	
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END ELSE
		/*-------*\
		  Sold
		\*-------*/
		IF @MovementType = 8
		BEGIN
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@OtherPartyNameKey, 1, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @OtherPartyNameKey,
				@DepartmentKey
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END ELSE
		/*----------------*\
		  Hosted Material
		\*----------------*/
		IF @MovementType = 9
		BEGIN
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@OtherPartyNameKey, 1, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movement_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movement_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movement_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movement_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movement_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movement_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movement_Insert TO [Dev - JNCC SQL]
END
GO