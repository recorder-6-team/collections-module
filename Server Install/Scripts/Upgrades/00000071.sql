/*===========================================================================*\
  Description:
	Updates for CCNs 163v4 and 168

  Created:
	March 2014


\*===========================================================================*/

/*===========================================================================*\
  Alter captions to conform to TSD update in CCN 163v4.
\*===========================================================================*/

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

UPDATE Movement
SET Search_Caption=
		dbo.ufn_GetMovementTypeName(Movement_Type) + ' - ' + dbo.ufn_GetFormattedName(Other_Party_Name_Key) +  ' - ' +
		dbo.ufn_GetDateFromVagueDate(Exp_Vague_Date_Start, Exp_Vague_Date_End, Exp_Vague_Date_Type),
	Display_Caption=
		dbo.ufn_GetMovementTypeName(Movement_Type) + ' - ' + dbo.ufn_GetFormattedName(Other_Party_Name_Key) +  ' - ' +
		dbo.ufn_GetDateFromVagueDate(Exp_Vague_Date_Start, Exp_Vague_Date_End, Exp_Vague_Date_Type)
WHERE Movement_Type IN (2, 3)	

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Movement_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Movement_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the General tab of the Movement screen.

  Parameters:	@Key	Collection key

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 18/03/14 16:05 $
    $Author: Brynhorsfieldschonhut $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Movement_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT 		M.Movement_Type,
			M.Other_Party_Name_Key,
			IsNull(N.Organisation, 0) AS OtherPartyIsOrganisation,
			dbo.ufn_GetFormattedName(M.Other_Party_Name_Key) AS Other_Party_Name,
			M.Staff_Responsible_Name_Key, 
			dbo.ufn_GetFormattedName(M.Staff_Responsible_Name_Key) AS Staff_Responsible_Name,
			M.Contact_Name_Key,
			dbo.ufn_GetFormattedName(M.Contact_Name_Key) AS Contact_Name,
			M.Exp_Vague_Date_Start, 
			M.Exp_Vague_Date_End, 
			M.Exp_Vague_Date_Type, 
			M.Number,
			M.Notes,
			M.Display_Caption,
			M.Timestamp,
			LoanDate_Vague_Date_Start = CASE
				WHEN M.Movement_Type = 2 THEN MOMIN.Vague_Date_Start
				WHEN M.Movement_Type = 3 THEN MOMOUT.Vague_Date_Start
				ELSE NULL
			END,
			LoanDate_Vague_Date_End = CASE
				WHEN M.Movement_Type = 2 THEN MOMIN.Vague_Date_End
				WHEN M.Movement_Type = 3 THEN MOMOUT.Vague_Date_End
				ELSE NULL
			END,
			LoanDate_Vague_Date_Type = CASE
				WHEN M.Movement_Type = 2 THEN MOMIN.Vague_Date_Type
				WHEN M.Movement_Type = 3 THEN MOMOUT.Vague_Date_Type
				ELSE NULL
			END,
			LoanComplete = CASE
				WHEN M.Movement_Type = 2 THEN MOMIN.Completed
				WHEN M.Movement_Type = 3 THEN MOMOUT.Completed
				ELSE NULL
			END,
			ActualReturnDate_Vague_Date_Start = CASE
				WHEN M.Movement_Type = 2 THEN MOMOUT.Vague_Date_Start
				WHEN M.Movement_Type = 3 THEN MOMIN.Vague_Date_Start
				ELSE NULL
			END,
			ActualReturnDate_Vague_Date_End = CASE
				WHEN M.Movement_Type = 2 THEN MOMOUT.Vague_Date_End
				WHEN M.Movement_Type = 3 THEN MOMIN.Vague_Date_End
				ELSE NULL
			END,
			ActualReturnDate_Vague_Date_Type = CASE
				WHEN M.Movement_Type = 2 THEN MOMOUT.Vague_Date_Type
				WHEN M.Movement_Type = 3 THEN MOMIN.Vague_Date_Type
				ELSE NULL
			END,
			ReturnComplete = CASE
				WHEN M.Movement_Type = 2 THEN MOMOUT.Completed
				WHEN M.Movement_Type = 3 THEN MOMIN.Completed
				ELSE NULL
			END,			

			--Following fields are used to get async controls
			M.Movement_Key,
			0 AS CollectionIndex,
			1 AS SpecimenIndex,
			2 AS StoreIndex
	FROM 		Movement AS M
	LEFT JOIN	[Name] AS N ON N.Name_Key = M.Other_Party_Name_Key
	LEFT JOIN 	Movement_Direction 
		AS MDIN 
		ON M.Movement_Key = MDIN.Movement_Key AND MDIN.Outbound = 0
	LEFT JOIN	Movement_Of_Material 
		AS MOMIN 
		ON MDIN.Movement_Direction_Key = MOMIN.Movement_Direction_Key
	LEFT JOIN 	Movement_Direction 
		AS MDOUT 
		ON M.Movement_Key = MDOUT.Movement_Key AND MDOUT.Outbound = 1
	LEFT JOIN	Movement_Of_Material 
		AS MOMOUT 
		ON MDOUT.Movement_Direction_Key = MOMOUT.Movement_Direction_Key
	
	WHERE 	M.Movement_Key = @Key

SET NOCOUNT OFF

GO

 
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movement_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movement_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movement_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movement_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movement_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Movement_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movement_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movement_Select TO [Dev - JNCC SQL]
END

GO

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
    $Revision: 2 $
    $Date: 18/03/14 16:05 $
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
    $Revision: 2 $
    $Date: 18/03/14 16:05 $
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
			IF @LoanVagueDateStart IS NOT NULL AND @LoanVagueDateEnd IS NOT NULL AND @LoanVagueDateType IS NOT NULL
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

			IF @ReturnVagueDateStart IS NOT NULL AND @ReturnVagueDateEnd IS NOT NULL AND @ReturnVagueDateType IS NOT NULL
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
			IF @ReturnVagueDateStart IS NOT NULL AND @ReturnVagueDateEnd IS NOT NULL AND @ReturnVagueDateType IS NOT NULL
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

			IF @LoanVagueDateStart IS NOT NULL AND @LoanVagueDateEnd IS NOT NULL AND @LoanVagueDateType IS NOT NULL
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
			ELSE
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
    $Revision: 2 $
    $Date: 18/03/14 16:05 $
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
	@PublishToWeb bit,
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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Specimen_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the Specimen General frame.

  Parameters:	@Key		Specimen Collection Unit key
		@IsLifeScience	Whether we should be using the Taxon_Determination
				or Taxon tables.

  Created:	Setember 2003

  Last revision information:
    $Revision: 2 $
    $Date: 18/03/14 16:05 $
    $Author: Brynhorsfieldschonhut $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimen_Select]
	@Key char(16),
	@IsLifeScience bit
AS

SET NOCOUNT ON

	IF @IsLifeScience = 1
	BEGIN
		SELECT		CU.Collection_Unit_Key,
				ITN.Taxon_List_Item_Key AS Term_Key,
				CU.Current_Location_Code,
				CU.Usual_Location_Code,
				CU.Domain_Mask,
				SU.Dangerous,
				SU.Publish_To_Web,
				SU.Confidential,
				SU.Parent_Collection_Collection_Unit_Key AS Parent_Unit_Key,
				Coll.Item_Name AS ParentCollectionCollectionUnitName,
				CU.Current_Container_Collection_Unit_Key,
				CU.Usual_Container_Collection_Unit_Key,
				SCU.Item_Name + ISNULL(' - ' + CSC.Current_Location_Code, ISNULL(' - ' + CSC.Usual_Location_Code, '')) AS Current_Location_Name,
				SUS.Item_Name + ISNULL(' - ' + CSU.Current_Location_Code, ISNULL(' - ' + CSU.Usual_Location_Code, '')) AS Usual_Location_Name,
				CASE 	WHEN ITN.Preferred_Name_Italic = 1 THEN '<i>' + ITN.Preferred_Name + '</i>'
					ELSE ITN.Preferred_Name 
				END AS Item_Name,
				Specimen_Type_Concept_Key,
				CTType.PlainText AS Type,
				SU.Timestamp AS SUTimeStamp,
				CU.Timestamp AS CUTimeStamp,
				SU.Checked,
				SU.Internal_Use

		FROM		Collection_Unit AS CU
		INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key	
		INNER JOIN	Collection AS Coll On Coll.Collection_Unit_Key = SU.Parent_Collection_Collection_Unit_Key
		LEFT JOIN	Taxon_Determination AS TD ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key
		LEFT JOIN	Index_Taxon_Name AS ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
	
		INNER JOIN 	VW_ConceptTerm AS CTType ON Specimen_Type_Concept_Key = CTType.Concept_Key

		LEFT JOIN 	Store SCU ON SCU.Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
		LEFT JOIN	Collection_Unit CSC ON CSC.Collection_Unit_Key = SCU.Collection_Unit_Key 
		LEFT JOIN 	Store SUS ON SUS.Collection_Unit_Key = CU.Usual_Container_Collection_Unit_Key
		LEFT JOIN	Collection_Unit CSU ON CSU.Collection_Unit_Key = SUS.Collection_Unit_Key
	
		WHERE SU.Collection_Unit_Key = @Key
	END
	ELSE
	BEGIN
		SELECT		CU.Collection_Unit_Key,
				C.Term_Key,
				CU.Current_Location_Code,
				CU.Usual_Location_Code,
				CU.Domain_Mask,
				SU.Dangerous,
				SU.Publish_To_Web,
				SU.Confidential,
				SU.Parent_Collection_Collection_Unit_Key AS Parent_Unit_Key,
				Coll.Item_Name AS ParentCollectionCollectionUnitName,
				CU.Current_Container_Collection_Unit_Key,
				CU.Usual_Container_Collection_Unit_Key,
				SCU.Item_Name + ISNULL(' - ' + CSC.Current_Location_Code, ISNULL(' - ' + CSC.Usual_Location_Code, '')) AS Current_Location_Name,
				SUS.Item_Name + ISNULL(' - ' + CSU.Current_Location_Code, ISNULL(' - ' + CSU.Usual_Location_Code, '')) AS Usual_Location_Name,
				C.Published_Term	AS	Item_Name,
				Specimen_Type_Concept_Key,
				CTType.PlainText AS Type,
				SU.Timestamp AS SUTimeStamp,
				CU.Timestamp AS CUTimeStamp,
				SU.Checked,
				SU.Internal_Use
	
		FROM		Collection_Unit AS CU
		INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key
		INNER JOIN	Collection AS Coll On Coll.Collection_Unit_Key = SU.Parent_Collection_Collection_Unit_Key
		LEFT JOIN	Determination AS D ON D.Determination_Key = SU.Preferred_Determination_Key
		LEFT JOIN	Concept AS C ON C.Concept_Key = D.Concept_Key
		
		INNER JOIN 	VW_ConceptTerm AS CTType ON Specimen_Type_Concept_Key = CTType.Concept_Key
		LEFT JOIN 	Store SCU ON SCU.Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
		LEFT JOIN	Collection_Unit CSC ON CSC.Collection_Unit_Key = SCU.Collection_Unit_Key 
		LEFT JOIN 	Store SUS ON SUS.Collection_Unit_Key = CU.Usual_Container_Collection_Unit_Key
		LEFT JOIN	Collection_Unit CSU ON CSU.Collection_Unit_Key = SUS.Collection_Unit_Key
	
		WHERE SU.Collection_Unit_Key = @Key
	END

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimen_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimen_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimen_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimen_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimen_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Specimen_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Specimen_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Specimen table.
		Ensures the Domain mask of the specimen is also updated.

  Parameters:	@Key
		@ParentCollectionCollectionUnitKey
		@SpecimenTypeConceptKey
		@Confidential
		@Dangerous
		@PublishToWeb
		@LifeSciences 
		@SUTimestamp 
		@Checked 
		@CurrentContainerCollectionUnitKey
		@CurrentLocationCode
		@UsualContainerCollectionUnitKey 
		@UsualLocationCode
		@CUTimestamp 
		@SessionID 

  Created:	July 2003

  Last revision information:
    $Revision: 2 $
    $Date: 18/03/14 16:05 $
    $Author: Brynhorsfieldschonhut $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimen_Update]
	--for specimen_unit
	@Key char(16), 
	@ParentCollectionCollectionUnitKey char(16),
	@SpecimenTypeConceptKey char(16),
	@Confidential bit,
	@Dangerous bit,
	@PublishToWeb bit,
	@LifeSciences bit,
	@SUTimestamp timestamp,
	@Checked bit,
	--for Collection_Unit
	@CurrentContainerCollectionUnitKey char(16),
	@CurrentLocationCode varchar(30),
	@UsualContainerCollectionUnitKey char(16),
	@UsualLocationCode varchar(30),
	@CUTimestamp timestamp,
	-- Both
	@SessionID char(16),
	@InternalUse bit
AS
	DECLARE @ExistingCollectionKey char(16),
		@ExistingContainerKey char(16),
		@SpecimenMask int,
		@ContainerMask int,
		@CollectionMask int

	/*-------------------------------------------------------------*\
	  Initialise variables.
	\*-------------------------------------------------------------*/
	SELECT		@ExistingCollectionKey = S.Parent_Collection_Collection_Unit_Key, 
			@ExistingContainerKey = CU.Current_Container_Collection_Unit_Key
	FROM		Specimen_Unit S
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
	WHERE		S.Collection_Unit_Key = @Key

	-- Retrieve the mask of the preferred concept for specimen.
	EXECUTE	usp_Get_Concept_Domain_Mask_From_Specimen @Key, @SpecimenMask OUTPUT

	/*-------------------------------------------------------------*\
	  If container changing, might need to update container mask.
	\*-------------------------------------------------------------*/
	IF @ExistingContainerKey <> @CurrentContainerCollectionUnitKey
	BEGIN
		-- A container may also be a specimen with its own concept. So get it.
		-- But if it is a storage, don't.
		SET @ContainerMask = 0
		IF EXISTS(SELECT * FROM Specimen_Unit WHERE Collection_Unit_Key = @ExistingContainerKey)
			EXECUTE	usp_Get_Concept_Domain_Mask_From_Specimen @ExistingContainerKey, @ContainerMask OUTPUT

		-- Work out the bit(s) to turn OFF
		SELECT		@ContainerMask = ~ @ContainerMask & ~ Sum(DISTINCT DM.Domain_Mask) & @SpecimenMask
		FROM		Collection_Unit CU
		INNER JOIN	Determination D 	WITH (INDEX (IX_Determination_Specimen)) 
							ON D.Specimen_Collection_Unit_Key = CU.Collection_Unit_Key
		INNER JOIN	Concept C 		WITH (INDEX (PK_Concept)) -- non-clustered
							ON C.Concept_Key = D.Concept_Key
		INNER JOIN	Concept_Group CG 	ON CG.Concept_Group_Key = C.Concept_Group_Key
		INNER JOIN	Local_Domain LD 	ON LD.Local_Domain_Key = CG.Local_Domain_Key
		INNER JOIN	Domain DM 		ON DM.Domain_Key = LD.Domain_Key
		WHERE		D.Preferred = 1
		AND		CU.Current_Container_Collection_Unit_Key = @ExistingContainerKey
		AND		CU.Collection_Unit_Key <> @Key
	END

	/*-------------------------------------------------------------*\
	  If collection changing, might need to update collection mask.
	\*-------------------------------------------------------------*/
	IF @ExistingCollectionKey <> @ParentCollectionCollectionUnitKey
	BEGIN
		-- Work out the bit(s) to turn OFF
		SELECT		@CollectionMask = ~ Sum(DISTINCT DM.Domain_Mask) & @SpecimenMask
		FROM		Specimen_Unit S
		INNER JOIN	Determination D 	WITH (INDEX (IX_Determination_Specimen)) 
							ON D.Specimen_Collection_Unit_Key = S.Collection_Unit_Key
		INNER JOIN	Concept C 		WITH (INDEX (PK_Concept))  -- non-clustered
							ON C.Concept_Key = D.Concept_Key
		INNER JOIN	Concept_Group CG 	ON CG.Concept_Group_Key = C.Concept_Group_Key
		INNER JOIN	Local_Domain LD 	ON LD.Local_Domain_Key = CG.Local_Domain_Key
		INNER JOIN	Domain DM 		ON DM.Domain_Key = LD.Domain_Key
		WHERE		D.Preferred = 1
		AND		S.Parent_Collection_Collection_Unit_Key = @ExistingCollectionKey
		AND		S.Collection_Unit_Key <> @Key
	END

	/*-------------------------------------------------------------*\
	  Do the table updates first. Or the containers will still have
	  the specimen and its mask!
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION	

		UPDATE	Specimen_Unit
		SET	Parent_Collection_Collection_Unit_Key = @ParentCollectionCollectionUnitKey,
			Specimen_Type_Concept_Key = @SpecimenTypeConceptKey,
			Confidential = @Confidential,
			Dangerous = @Dangerous,
			Publish_To_Web = @PublishToWeb,
			Life_Sciences = @LifeSciences,
			Changed_Session_ID = @SessionID,
			Checked = @Checked,
			Internal_Use = @InternalUse 
		WHERE	Collection_Unit_Key = @Key
		AND		[Timestamp] = @SUTimestamp
	
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Specimen_Unit WHERE Collection_Unit_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END
	
		UPDATE	Collection_Unit
		SET	Current_Container_Collection_Unit_Key = @CurrentContainerCollectionUnitKey,
			Usual_Container_Collection_Unit_Key = @UsualContainerCollectionUnitKey,
			Current_Location_Code = @CurrentLocationCode,
			Usual_Location_Code = @UsualLocationCode,
			Changed_Session_ID = @SessionID
		WHERE	Collection_Unit_Key = @Key
		AND		[Timestamp] = @CUTimestamp
	
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit WHERE Collection_Unit_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END
	
		/*-------------------------------------------------------------*\
		  Now switch the bits OFF, if necessary.
		\*-------------------------------------------------------------*/
		IF (@ExistingContainerKey <> @CurrentContainerCollectionUnitKey) OR (@ExistingCollectionKey <> @ParentCollectionCollectionUnitKey)
		BEGIN
			-- Loop through all 32 bits and check which ones need to be switched OFF or left alone.
			-- From 10000000 00000000 00000000 00000000 down to 00000000 00000000 00000000 00000001
			DECLARE	@BitMask bigint  -- Use BIGINT, no unsigned int in SQL Server!!!!!
			SET @BitMask = 0x80000000
	
			-- 1 / 2 = 0, use this as stop condition
			WHILE @BitMask <> 0
			BEGIN
				-- If the bit is found ON for container, switch it OFF
				IF (@ExistingContainerKey <> @CurrentContainerCollectionUnitKey) AND (@ContainerMask & @BitMask = @BitMask)
					-- Update the container mask
					EXECUTE	usp_CollectionUnit_Update_DomainMask @ExistingContainerKey, @BitMask, 0
				
	
				-- If the bit is found ON for collection, switch it OFF
				IF (@ExistingCollectionKey <> @ParentCollectionCollectionUnitKey) AND (@CollectionMask & @BitMask = @BitMask)
					-- Update the collection mask
					EXECUTE	usp_Collection_Update_DomainMask @ExistingCollectionKey, @BitMask, 0
	
				-- Set mask for next single bit in line
				SET @BitMask = @BitMask / 2
			END
		END
	
		/*-------------------------------------------------------------*\
		  And ON. Straight forward all bits in this case.
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimen_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimen_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimen_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimen_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimen_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[tr_Movement_SearchCaption]') 
	   AND    Type = 'TR')
    DROP TRIGGER [dbo].[tr_Movement_SearchCaption]
GO

/*===========================================================================*\
  Description:	Update search caption on the Movement table.

  Type:		AFTER INSERT, UPDATE

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 18/03/14 16:05 $
    $Author: Brynhorsfieldschonhut $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_Movement_SearchCaption]
ON [dbo].[Movement]
AFTER INSERT, UPDATE
AS 

	IF UPDATE(Exp_Vague_Date_Start) 
			OR UPDATE(Exp_Vague_Date_End)
			OR UPDATE(Exp_Vague_Date_Type)
			OR UPDATE(Number)
			OR UPDATE(Other_Party_Name_Key)
	BEGIN
			UPDATE Movement 
			SET Search_Caption=
					dbo.ufn_GetMovementTypeName(I.Movement_Type) + ' (' + I.Number +  ') - ' +
					dbo.ufn_GetDateFromVagueDate(I.Exp_Vague_Date_Start, I.Exp_Vague_Date_End, I.Exp_Vague_Date_Type),
				Display_Caption=
					dbo.ufn_GetMovementTypeName(I.Movement_Type) + ' (' + I.Number +  ') - ' +
					dbo.ufn_GetDateFromVagueDate(I.Exp_Vague_Date_Start, I.Exp_Vague_Date_End, I.Exp_Vague_Date_Type)
			FROM Movement M
			INNER JOIN Inserted I on I.Movement_Key=M.Movement_Key
			WHERE I.Movement_Type IN (0, 1)

			UPDATE Movement
			SET Search_Caption=
					dbo.ufn_GetMovementTypeName(I.Movement_Type) + ' - ' + dbo.ufn_GetFormattedName(I.Other_Party_Name_Key) +  ' - ' +
					dbo.ufn_GetDateFromVagueDate(I.Exp_Vague_Date_Start, I.Exp_Vague_Date_End, I.Exp_Vague_Date_Type),
				Display_Caption=
					dbo.ufn_GetMovementTypeName(I.Movement_Type) + ' - ' + dbo.ufn_GetFormattedName(I.Other_Party_Name_Key) +  ' - ' +
					dbo.ufn_GetDateFromVagueDate(I.Exp_Vague_Date_Start, I.Exp_Vague_Date_End, I.Exp_Vague_Date_Type)
			FROM Movement M
			INNER JOIN Inserted I on I.Movement_Key=M.Movement_Key
			WHERE I.Movement_Type IN (2, 3)			 

			UPDATE Movement 
			SET Search_Caption=
					dbo.ufn_GetMovementTypeName(I.Movement_Type) + ' - ' +
					dbo.ufn_GetDateFromVagueDate(I.Exp_Vague_Date_Start, I.Exp_Vague_Date_End, I.Exp_Vague_Date_Type),
				Display_Caption=
					dbo.ufn_GetMovementTypeName(I.Movement_Type) + ' - ' +
					dbo.ufn_GetDateFromVagueDate(I.Exp_Vague_Date_Start, I.Exp_Vague_Date_End, I.Exp_Vague_Date_Type)
			FROM Movement M
			INNER JOIN Inserted I on I.Movement_Key=M.Movement_Key
			WHERE I.Movement_Type NOT IN (0, 1, 2, 3) 
	END

GO
