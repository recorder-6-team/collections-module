SET QUOTED_IDENTIFIER ON
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Accessions_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Accessions_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_Accessions_Select_ForSearch] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SearchText VARCHAR(100)

AS
--
--  DESCRIPTION
--  Returns Movement_Key and Movement caption as search characters are entered.
--  Checks that the movement type is either 0 or 1, i.e. an Accession or an Exchange.
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned.
--	@SessionID 		User's SessionID.
--	@SearchText 		Search text used to find collections.
--
--  AUTHOR:			Anthony Simpson, Dorset Software
--  CREATED:			2003-09-23

SET NOCOUNT ON

--DISTINCT removes duplicate records due to two direction types

SELECT DISTINCT M.Movement_Key AS Item_Key, Display_Caption AS DisplayTerm
FROM	MOVEMENT M
		INNER JOIN
			MOVEMENT_DIRECTION MD
		ON M.Movement_Key = MD.Movement_Key 
			AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
		LEFT JOIN 
			(MOVEMENT_COLLECTION_UNIT MCU 
			INNER JOIN 
				COLLECTION_UNIT CU 
			ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
				AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0))
			)
		ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
		WHERE ((CU.Collection_Unit_Key IS NOT NULL) 
			OR ((MCU.Collection_Unit_Key IS NULL) AND ((M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID))))
			AND Search_Caption LIKE @SearchText + '%'

GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Accessions_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Accessions_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Accessions_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Accessions_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Accessions_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Accessions_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Accessions_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Accessions_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_RefreshDomainMask]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_RefreshDomainMask]
GO

/*===========================================================================*\
  Description:	Returns Collections associated with a specified Condition Check.
	This is a support stored procedure for system administrators rather than
	a stored proc accessed from within the application.

  Parameters:	
	None

  Created:	April 2008

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Collections_RefreshDomainMask] 
AS

-- Reset the domain masks for all collections
UPDATE CU
SET CU.Domain_Mask=0
FROM Collection_Unit CU
INNER JOIN Collection C ON C.Collection_Unit_Key=CU.Collection_Unit_Key

DECLARE @CollectionUnitKey CHAR(16), @DomainMask INT

-- Declare to find the distinct specimen domains
DECLARE csr CURSOR FOR
	SELECT DISTINCT CUCol.Collection_Unit_Key, CUSpec.Domain_Mask
	FROM Collection_Unit CUCol
	INNER JOIN Collection CCol ON CCol.Collection_Unit_Key=CUCol.Collection_Unit_Key
	INNER JOIN Specimen_Unit SU ON SU.Parent_Collection_Collection_Unit_Key=CUCol.Collection_Unit_Key
	INNER JOIN Collection_Unit CUSpec ON CUSpec.Collection_Unit_Key=SU.Collection_Unit_Key

OPEN csr

-- OR each domain into the Collection. Note a SUM won't work in the case that a specimen has 2 domains.
WHILE (1=1)
BEGIN
	FETCH NEXT FROM csr INTO @CollectionUnitKey, @DomainMask
	IF @@FETCH_STATUS<>0 BREAK

	UPDATE Collection_Unit SET Domain_Mask = Domain_Mask | @DomainMask
	WHERE Collection_Unit_Key=@CollectionUnitKey

END

CLOSE csr

DEALLOCATE csr

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitAccessionNumber_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitAccessionNumber_Get]
GO

/*===========================================================================*\
  Description:	Returns the accession number for a collection unit record.

  Parameters:	@Key	Collection unit key
		@Number	OUTPUT

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitAccessionNumber_Get]
	@Key char(16),
	@Number varchar(100) OUTPUT
AS
	
	SELECT	TOP 1 	@Number = M.Number
	FROM		Collection_Unit CU
	INNER JOIN 	Movement_Collection_Unit MCU ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
	INNER JOIN 	Movement_Direction MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN 	Movement M ON M.Movement_Key = MD.Movement_Key
	WHERE		CU.Collection_Unit_Key = @Key
	AND		MD.OutBound = 0			-- Inbound = 0
	AND		M.Movement_Type IN (0, 1)	-- Accession = 0, Exchange = 1
	ORDER BY 	Exp_Vague_Date_Start DESC

	-- Can't have it in select, in case NO records are returned.
	SET @Number = ISNULL(@Number, 'Unknown')
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitAccessionNumber_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitAccessionNumber_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitAccessionNumber_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitAccessionNumber_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitAccessionNumber_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitAccessionNumber_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitAccessionNumber_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitAccessionNumber_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitDepartment_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitDepartment_Get]
GO

/*===========================================================================*\
  Description:	Returns the department for a collection unit record.

  Parameters:	@Key		Collection unit key
		@Department	OUTPUT

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitDepartment_Get]
	@Key char(16),
	@Department varchar(100) OUTPUT
AS
	DECLARE	@DeptName varchar(100),
		@ReceiverKey char(16)

	/*-------------------------------------------------------------*\
	  Get the relevant data.
	\*-------------------------------------------------------------*/
	SELECT	TOP 1 	@DeptName = OD.Item_Name, @ReceiverKey = MD.Receiver_Name_Key
	FROM		Movement_Collection_Unit MCU 
	INNER JOIN 	Movement_Direction MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN 	Movement_Of_Material MM ON MM.Movement_Direction_Key = MD.Movement_Direction_Key
	LEFT JOIN 	Organisation_Department OD ON OD.Organisation_Department_Key = MM.Receiver_Organisation_Department_Key
	WHERE		MCU.Collection_Unit_Key = @Key
	ORDER BY 	MM.Vague_Date_Start DESC

	/*-------------------------------------------------------------*\
	  Now work out what to display.
	\*-------------------------------------------------------------*/
	IF @DeptName IS NOT NULL
		SET @Department = @DeptName
	ELSE
	IF @ReceiverKey IS NOT NULL
		SET @Department = dbo.ufn_GetFormattedName(@ReceiverKey)
	ELSE
		SET @Department = 'Unknown'
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitDepartment_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitDepartment_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitDepartment_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitDepartment_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitDepartment_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitDepartment_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitDepartment_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitDepartment_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitNumber_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitNumber_Get]
GO

/*===========================================================================*\
  Description:	Returns the value of Number from the Collection_Unit_Number table.

  Parameters:	@Key	Collection unit key
		@Name	OUTPUT

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitNumber_Get]
	@Key char(16),
	@Number varchar(30) OUTPUT
AS
	SELECT 	@Number = Number
	FROM	Collection_Unit_Number
	WHERE	Collection_Unit_Key = @Key
	AND	Preferred = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitNumber_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitNumber_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitStatus_Get') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_CollectionUnitStatus_Get]
GO

/*===========================================================================*\
  Description:	Returns the movement status for a collection unit record.

  Parameters:	@Key	Collection unit key
		@Status	OUTPUT

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitStatus_Get]
	@Key char(16),
	@Status varchar(200) OUTPUT
AS
	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	-- If unknown, set result and exit.
	IF NOT (EXISTS(
		SELECT * FROM Movement_Collection_Unit MCU 
		INNER JOIN 	Movement_Of_Material MM ON MM.Movement_Direction_Key = MCU.Movement_Direction_Key
		LEFT JOIN 	Movement_Of_Material_Exclusion MME 
					ON MME.Movement_Of_Material_Key = MM.Movement_Of_Material_Key
					AND MME.Collection_Unit_Key=MCU.Collection_Unit_Key
		WHERE 	MCU.Collection_Unit_Key = @Key
		AND 	MME.Movement_Of_Material_Exclusion_Key IS NULL)
	OR EXISTS(
		SELECT * FROM Movement_Collection_Unit MCU 
		INNER JOIN 	Movement_Of_Ownership MO ON MO.Movement_Direction_Key = MCU.Movement_Direction_Key
		LEFT JOIN 	Movement_Of_Ownership_Exclusion MOE 
					ON MOE.Movement_Of_Ownership_Key = MO.Movement_Of_Ownership_Key
					AND MOE.Collection_Unit_Key=MCU.Collection_Unit_Key
		WHERE 	MCU.Collection_Unit_Key = @Key
		AND 	MOE.Movement_Of_Ownership_Exclusion_Key IS NULL))
	BEGIN
		SET @Status = 'Unknown'
		GOTO CheckSpecimenPresence
	END

	/*-------------------------------------------------------------*\
	  Need to work out what it is.
	\*-------------------------------------------------------------*/
	-- Some variables.
	DECLARE	@HoldingOrganisationKey char(16),
		@OwnershipNameKey char(16),
		@OwnershipNameKeyIsHoldingOrg bit,
		@OwnershipMovementType int,
		@MaterialNameKey char(16),
		@MaterialNameKeyIsHoldingOrg bit,
		@MaterialMovementType int,
		@DepartmentName varchar(100)

	-- Get the organisation holding software install	
	SELECT	@HoldingOrganisationKey = Data FROM Setting WHERE [Name] = 'HoldingOrg'

	/*-------------------------------------------------------------*\
	  Get most recent movement of material.
	\*-------------------------------------------------------------*/
	SELECT	TOP 1	@MaterialNameKey = MM.Receiver_Name_Key, 
			@MaterialMovementType = Movement_Type, 
			@DepartmentName = OD.Item_Name
	FROM		Movement_Collection_Unit MCU
	INNER JOIN 	Movement_Direction MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN 	Movement M ON M.Movement_Key = MD.Movement_Key
	INNER JOIN 	Movement_Of_Material MM ON MM.Movement_Direction_Key = MCU.Movement_Direction_Key
	LEFT JOIN 	Movement_Of_Material_Exclusion MME 
				ON MME.Movement_Of_Material_Key = MM.Movement_Of_Material_Key
				AND MME.Collection_Unit_Key=MCU.Collection_Unit_Key
	LEFT JOIN	Organisation_Department OD ON OD.Organisation_Department_Key = MM.Receiver_Organisation_Department_Key
	WHERE		MCU.Collection_Unit_Key = @Key
	AND 		MME.Movement_Of_Material_Key IS NULL
	ORDER BY	MM.Vague_Date_Start DESC

	/*-------------------------------------------------------------*\
	  Lost and destroyed movements
	\*-------------------------------------------------------------*/
	IF @MaterialMovementType = 4
	BEGIN
		SET @Status='Destroyed'
		GOTO CheckSpecimenPresence
	END
	IF @MaterialMovementType = 7
	BEGIN
		SET @Status='Lost'
		GOTO CheckSpecimenPresence
	END

	/*-------------------------------------------------------------*\
	  Get most recent movement of ownership.
	\*-------------------------------------------------------------*/
	SELECT	TOP 1	@OwnershipNameKey = IsNull(MD2.Receiver_Name_Key, MD.Receiver_Name_Key),
			@OwnershipMovementType = M.Movement_Type
	FROM		Movement_Collection_Unit MCU
	INNER JOIN 	Movement_Direction MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN 	Movement M ON M.Movement_Key = MD.Movement_Key
			-- If the Movement is a loan in, the name key stored in the Movement_Direction
			-- record linked to the Movement_Collection_Unit record will be the receiver.
			-- The actual owner will be stored in the outbound Movement_Direction record
			-- associated with the movement, so do a left join on this.
	LEFT JOIN	(Movement_Direction MD2 JOIN Movement AS M2 ON M2.Movement_Key = MD2.Movement_key AND M2.Movement_Type = 2)
				ON MD2.Movement_Key = M.Movement_Key
				AND MD2.Outbound = 1
	INNER JOIN 	Movement_Of_Ownership MO ON MO.Movement_Direction_Key = MCU.Movement_Direction_Key
	LEFT JOIN 	Movement_Of_Ownership_Exclusion MOE 
				ON MOE.Movement_Of_Ownership_Key = MO.Movement_Of_Ownership_Key
				AND MOE.Collection_Unit_Key=MCU.Collection_Unit_Key
	WHERE		MCU.Collection_Unit_Key = @Key
	AND 		MOE.Movement_Of_Ownership_Key IS NULL
	ORDER BY	MO.Vague_Date_Start DESC

	/*-----------------------------------------------------------------------------*\
	  Now that we have the Name Key of the most recent Movement_Of_Onwership for 
	  this Collection_Unit, we want to see if this key refers to
	  the holding organisation, or if it refers to an individual who belongs
	  to the holding organisation
	\*-----------------------------------------------------------------------------*/
	IF @OwnershipNameKey = @HoldingOrganisationKey
		SET 	@OwnershipNameKeyIsHoldingOrg = 1
	ELSE IF EXISTS( SELECT 		I.Name_Key
			FROM		Individual AS I
			LEFT JOIN	Name_Relation AS NR1 ON NR1.Name_Key_1 = I.Name_Key
			LEFT JOIN	Name_Relation AS NR2 ON NR2.Name_Key_2 = I.Name_Key
			INNER JOIN	Organisation AS O ON (O.Name_Key = NR1.Name_Key_2 OR O.Name_Key = NR2.Name_Key_1)
			WHERE		I.Name_Key = @OwnershipNameKey
			AND 		O.Name_Key = @HoldingOrganisationKey)
		SET @OwnershipNameKeyIsHoldingOrg = 1
	-- If there is no @OwnershipNameKey assume that it belongs to the holding org.
	ELSE IF @OwnershipNameKey Is NULL
		SET @OwnershipNameKeyIsHoldingOrg = 1
	ELSE
		SET @OwnershipNameKeyIsHoldingOrg = 0

	/*==============================================================================*\
	  Find out if the item is currently in the Holding Organisation or not.
	\*==============================================================================*/	

	/*-----------------------------------------------------------------------------*\
	  Now that we have the Name Key for the most recent Movement of Material, we 
	  want to see if this key refers to the holding organisation, or if it refers 
	  to an individual who belongs to the holding organisation
	\*-----------------------------------------------------------------------------*/
	IF @MaterialNameKey = @HoldingOrganisationKey
		SET 	@MaterialNameKeyIsHoldingOrg = 1
	ELSE IF EXISTS( SELECT 		I.Name_Key
			FROM		Individual AS I
			LEFT JOIN	Name_Relation AS NR1 ON NR1.Name_Key_1 = I.Name_Key
			LEFT JOIN	Name_Relation AS NR2 ON NR2.Name_Key_2 = I.Name_Key
			INNER JOIN	Organisation AS O ON (O.Name_Key = NR1.Name_Key_2 OR O.Name_Key = NR2.Name_Key_1)
			WHERE		I.Name_Key = @MaterialNameKey
			AND 		O.Name_Key = @HoldingOrganisationKey)
		SET @MaterialNameKeyIsHoldingOrg = 1
	ELSE
		SET @MaterialNameKeyIsHoldingOrg = 0


	-- Formatted additional info after status. No need to repeat all over the place.
	DECLARE @ByOwner varchar(200), @ToOwner varchar(200), @FromOwner varchar(200)
	SET @ByOwner = IsNull(NullIf(' by '+ dbo.ufn_GetFormattedName(@OwnershipNameKey), ' by '), '')
	SET @ToOwner = IsNull(NullIf(' to '+ dbo.ufn_GetFormattedName(@OwnershipNameKey), ' to '), '')
	SET @FromOwner = IsNull(NullIf(' from '+ dbo.ufn_GetFormattedName(@OwnershipNameKey), ' from '), '')

	/*==============================================================================*\
	  Now work out what to display.
	\*==============================================================================*/		
	IF @OwnershipNameKeyIsHoldingOrg=1
	BEGIN
		IF @MaterialNameKeyIsHoldingOrg=1
		BEGIN
			IF @DepartmentName IS NOT NULL 
				SET @Status = 'In ' + @DepartmentName + ' department.'
			ELSE BEGIN
				SELECT	@DepartmentName = Full_Name
				FROM	Organisation
				WHERE	Name_Key = @HoldingOrganisationKey
	
				SET @Status = 'In ' + @DepartmentName + ' organisation.' 
			END 
		END ELSE
		IF @DepartmentName IS NOT NULL 
			SET @Status = 'In ' + @DepartmentName + ' department.'
		ELSE
			SET @Status = 'Owned' + @ByOwner
	END
	ELSE
	BEGIN
		IF @MaterialNameKeyIsHoldingOrg = 1
			SET @Status = 'On loan' + @FromOwner
		ELSE IF @MaterialMovementType = 8
			SET @Status = 'Sold' + @ToOwner
		ELSE IF @MaterialMovementType = 5
			SET @Status = 'Disposed' + @ToOwner		
		ELSE
			SET @Status = 'Owned' + @ByOwner
	END

CheckSpecimenPresence:
	IF EXISTS (SELECT * FROM Specimen_Unit WHERE Collection_Unit_Key = @Key)
	BEGIN
		IF NOT (@Status IN ('Lost', 'Destroyed', 'Sold', 'Disposed'))
		BEGIN
			DECLARE @MeaningKey CHAR(16)

			SELECT @MeaningKey = C.Meaning_Key
			FROM Collection_Unit_Check CUC
			INNER JOIN Conservation_Check CC ON CUC.Conservation_Check_Key = CC.Conservation_Check_Key
			INNER JOIN Concept C ON C.Concept_Key = CC.Condition_Concept_Key
			WHERE CUC.Collection_Unit_Key = @Key 

			IF @MeaningKey IS NULL
			BEGIN
				IF @Status = 'Unknown'
					SET @Status = 'Presence Unknown'
				ELSE
					SET @Status = 'Presence Unknown ' + @Status
			END
			ELSE IF @MeaningKey = 'SYSTEM00000021ZW'
			BEGIN
				IF @Status = 'Unknown'
					SET @Status = 'Not Found'
				ELSE
					SET @Status = 'Not Found ' + @Status
			END
		END
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitStatus_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitStatus_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collection_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collection_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_Collection_Select_ForSearch] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SearchText VARCHAR(100)

AS

--  DESCRIPTION
--  Returns top level Collections data to the CollectionsBrowser
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@SearchText 		Search text used to find collections
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-08
--

SET NOCOUNT ON
SET NO_BROWSETABLE OFF

SELECT C.Collection_Unit_Key AS Item_Key, 
	CASE WHEN M.Number IS NULL THEN C.Item_Name ELSE C.Item_Name + ' - ' + M.Number END AS DisplayTerm,
	CASE WHEN M.Number IS NULL THEN C.Item_Name ELSE C.Item_Name + ' - ' + M.Number END AS SearchTerm

FROM 
	(COLLECTION C
	INNER JOIN
   	    COLLECTION_UNIT CU 
   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	)
	LEFT JOIN
		(MOVEMENT_COLLECTION_UNIT MCU
		INNER JOIN
			MOVEMENT_DIRECTION MD
		ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
			AND (MD.Outbound = 0)
		INNER JOIN
			MOVEMENT M
		ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
		LEFT JOIN
			(CONCEPT CON
			INNER JOIN 
				TERM T
			ON CON.Term_Key = T.Term_Key)
		ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
	ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
WHERE C.Item_Name LIKE @SearchText + '%'
ORDER BY DisplayTerm

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collection_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collection_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collection_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collection_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collection_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collection_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CommonName_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CommonName_Get]
GO

/*===========================================================================*\
  Description: Returns the common name for a concept

  Parameters:	@ParentConceptKey
							@HierarchyRelationTypeKey - relationship type used to populate
							hierarchy.

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CommonName_Get]
	@ConceptKey char(16),
	@CommonName varchar(150) output
AS

SELECT Top 1 	@CommonName = T.Item_Name
FROM 		Concept C1
INNER JOIN 	Concept C2 	on C2.Meaning_Key=C1.Meaning_Key
    				AND C2.Name_Type_Concept_Key='SYSTEM000000000L'
				AND C2.Preferred = 1
INNER JOIN 	Term T 		on T.Term_Key=C2.Term_Key
INNER JOIN 	Language L 	on L.Language_Key=T.Language_Key
    				AND L.Priority=1
WHERE 		C1.Concept_Key = @ConceptKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CommonName_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CommonName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CommonName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CommonName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CommonName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CommonName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CommonName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CommonName_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptCount_Get_ForDomain]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptCount_Get_ForDomain]
GO

/*===========================================================================*\
  Description:	Counts the number of Concepts in a specified domain.

  Parameters:	@Domain - key of the domain

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptCount_Get_ForDomain]
	@Domain char(16),
	@ConceptCount int OUTPUT
AS

	SELECT 		@ConceptCount = Count(C.Concept_Key) 
	FROM 		Concept AS C
	INNER JOIN 	Concept_Group AS CG ON CG.Concept_Group_Key = C.Concept_Group_Key
	INNER JOIN 	Local_Domain AS LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
	WHERE 		LD.Domain_Key = @Domain

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptCount_Get_ForDomain') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptCount_Get_ForDomain'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptCount_Get_ForDomain TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptCount_Get_ForDomain TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptCount_Get_ForDomain TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptCount_Get_ForDomain TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptCount_Get_ForDomain TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptCount_Get_ForDomain TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupHierarchyKey_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroupHierarchyKey_Get]
GO

/*===========================================================================*\
  Description:	Returns the Hierarchy_Relation_Type_Key from the 
		Concept_Group table.

  Parameters:	@ConceptKey

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupHierarchyKey_Get]
	@ConceptKey char(16),
	@HierarchyRelationTypeKey char(16) OUTPUT
AS
	SELECT 		@HierarchyRelationTypeKey = CG.Hierarchy_Relation_Type_Key
	FROM		Concept AS C
	INNER JOIN	Concept_Group AS CG ON CG.Concept_Group_Key = C.Concept_Group_Key
	WHERE		C.Concept_Key = @ConceptKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupHierarchyKey_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupHierarchyKey_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupHierarchyKey_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupHierarchyKey_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupHierarchyKey_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupHierarchyKey_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupHierarchyKey_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupHierarchyKey_Get TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConceptRanks_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConceptRanks_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Search proc for Concept Ranks. If a duplicate is found,
				the Domain name is appended.

  Parameters:	@SearchText

  Created:		September 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRank_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptRank_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import concept ranks corresponding to taxon ranks from the
				specified taxon list.

  Parameters:   @job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 3 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRank_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @taxon_list_key		CHAR(16),
				@domain_key			CHAR(16),
				@taxon_rank_key		CHAR(16),
				@item_name			VARCHAR(100),
				@sort_order			INT,
				@abbreviation		VARCHAR(10),
				@ins_user_key		CHAR(16),
				@ins_date			SMALLDATETIME,
				@ins_session_id		CHAR(16),
				@upd_user_key		CHAR(16),
				@upd_date			SMALLDATETIME,
				@upd_session_id		CHAR(16),
				@system				BIT,
				@concept_rank_key	CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key,
				@domain_key								=	ld.Domain_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	INNER JOIN	Concept_Group							AS	g
	ON			g.Concept_Group_Key						=	m.Concept_Group_Key
	INNER JOIN	Local_Domain							AS	ld
	ON			ld.Local_Domain_Key						=	g.Local_Domain_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing concept ranks'
	IF @@ERROR <> 0 RETURN

	DECLARE		ranks	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tr.TAXON_RANK_KEY,
				ISNULL(tr.LONG_NAME, tr.SHORT_NAME),
				tr.SEQUENCE,
				tr.SHORT_NAME,
				tr.ENTERED_BY,
				tr.ENTRY_DATE,
				tr.CHANGED_BY,
				tr.CHANGED_DATE,
				tr.SYSTEM_SUPPLIED_DATA
	FROM		TAXON_LIST_VERSION			AS	tlv
	INNER JOIN	TAXON_LIST_ITEM				AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY	=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_RANK					AS	tr
	ON			tr.TAXON_RANK_KEY			=	tli.TAXON_RANK_KEY
	WHERE		tlv.TAXON_LIST_KEY			=	@taxon_list_key

	OPEN		ranks

	WHILE 1 = 1
	BEGIN
		FETCH		ranks
		INTO        @taxon_rank_key,
					@item_name,
					@sort_order,
					@abbreviation,		/* TODO: may clip! */
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		/* obtain session identifiers */
		EXECUTE		usp_Session_ForDate		@ins_user_key,
											@ins_date,
											@ins_session_id		OUTPUT
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @upd_user_key IS NULL OR @upd_date IS NULL
		BEGIN
			SET			@upd_session_id		=	NULL
		END
		ELSE
		BEGIN
			EXECUTE		usp_Session_ForDate		@upd_user_key,
												@upd_date,
												@upd_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		SELECT		@concept_rank_key						=	M.Concept_Rank_Key
		FROM		Taxon_Dictionary_Concept_Rank_Mapping M
		INNER JOIN	Concept_Rank CR 
					ON CR.Concept_Rank_Key					=	M.Concept_Rank_Key
					AND CR.Domain_Key						=	@Domain_Key
		WHERE		M.Taxon_Rank_Key						=	@taxon_rank_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update concept rank */
			UPDATE		Concept_Rank
			SET			Domain_Key				=	@domain_key,
						Item_Name				=	@item_name,
						Sort_Order				=	@sort_order,
						Abbreviation			=	@abbreviation,
						Entered_Session_ID		=	@ins_session_id,
						Changed_Session_ID		=	@upd_session_id,
						System_Supplied_Data	=	@system
			WHERE		Concept_Rank_Key		=	@concept_rank_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create concept rank */
			EXECUTE		spNextKey	'Concept_Rank',
									@concept_rank_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Concept_Rank (
						Concept_Rank_Key,
						Domain_Key,
						Item_Name,
						Sort_Order,
						Abbreviation,
						Color_R,
						Color_G,
						Color_B,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			VALUES		(@concept_rank_key,
						@domain_key,
						@item_name,
						@sort_order,
						@abbreviation,
						0,
						0,
						0,
						@ins_session_id,
						@upd_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Rank_Mapping (
						Taxon_Rank_Key,
						Concept_Rank_Key)
			VALUES		(@taxon_rank_key,
						@concept_rank_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		ranks
	DEALLOCATE	ranks
	RETURN

fail_from_cursor:
	CLOSE		ranks
	DEALLOCATE	ranks

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptRank_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRank_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRank_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRank_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRank_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRank_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Concepts_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Concepts_Select_ForSearch]
GO

/*===========================================================================*\
  Description: Search proc for concepts where Term_Version_Key is not null.

  Parameters:	@SearchText

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Concepts_Select_ForSearch] 
	@SearchText varchar(100)
AS

SET NOCOUNT ON

	SELECT 
		CT.Concept_Key AS Item_Key,
		CT.Item_Name + ' - ' + CG.Item_Name AS DisplayTerm,
		CT.Plaintext + ' - ' + CG.Item_Name COLLATE SQL_Latin1_General_CP1_CI_AS AS SearchTerm
	FROM
		VW_ConceptTerm CT
	INNER JOIN Concept_Group CG ON CG.Concept_Group_Key=CT.Concept_Group_Key
	WHERE CT.Plaintext LIKE @SearchText + '%'	
	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concepts_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concepts_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concepts_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concepts_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concepts_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concepts_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concepts_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concepts_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptToConceptRelations_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptToConceptRelations_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of relationships of any type that exist 
			between a concept and a list of other concepts.  Both directions are scanned.

  Parameters:	@FromKey 		Concept_Key
							@ToKeys			Concept_Key\Concept_Key etc
							@DoAncestors - if 0, then inherited relationships not found.  If 1, 
							then only inherited relationships returned

  Created:	Dec 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptToConceptRelations_Select]
	@FromKey CHAR(16),
	@ToKeys VARCHAR(1600),  -- max 100 links scanned
	@IncludeInherited BIT

AS

SET NOCOUNT ON

DECLARE @CharPos INTEGER

DECLARE @FromConceptKeys TABLE (
  Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
	Ancestor BIT,   -- is this a concept in the lineage above the selected concept?
	Concept_Group_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
)

DECLARE @ToConceptKeys TABLE (
  Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
	Leaf_Concept_Key CHAR(16),
	Concept_Group_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
	Ancestor BIT,   -- is this a concept in the lineage above the selected concept?
	PRIMARY KEY (Concept_Key, Leaf_Concept_Key)
)

/*===========================================================================*\	
  Create temp tables to store the concept keys we are scanning from and to.
	This includes those in the hierarchy, because relationships can be 
	inherited
\*===========================================================================*/

--Find the current concept groups to aid when filtering lineage
DECLARE @FromConceptGroupKey char(16)
SELECT @FromConceptGroupKey=Concept_Group_Key FROM Concept WHERE Concept_Key=@FromKey

--Store the from concept key, we'll add the ancestors later
INSERT INTO @FromConceptKeys (Concept_Key, Ancestor, Concept_Group_Key) 
VALUES (@FromKey, 0, @FromConceptGroupKey)

-- and retrieve the To keys by parsing the \ separated list
SET @CharPos=1

WHILE @CharPos<LEN(@ToKeys)
BEGIN
  IF SUBSTRING(@ToKeys, @CharPos, 1)='\'
		INSERT INTO @ToConceptKeys (Concept_Key, Leaf_Concept_Key, Concept_Group_Key, Ancestor)
		  SELECT C.Concept_Key, C.Concept_Key, C.Concept_Group_Key, 0
			FROM Concept C
			LEFT JOIN @ToConceptKeys T ON T.Concept_Key=C.Concept_Key
			WHERE C.Concept_Key=SUBSTRING(@ToKeys, @CharPos-16, 16)
			AND T.Concept_Key IS NULL
  SET @CharPos=@CharPos+1
END

-- read the last item which has no \ after it
INSERT INTO @ToConceptKeys (Concept_Key, Leaf_Concept_Key, Concept_Group_Key, Ancestor)
  SELECT C.Concept_Key, C.Concept_Key, C.Concept_Group_Key, 0
	FROM Concept C
	LEFT JOIN @ToConceptKeys T ON T.Concept_Key=C.Concept_Key
	WHERE C.Concept_Key=RIGHT(@ToKeys, 16)
			AND T.Concept_Key IS NULL

/*===========================================================================*\	
	Retrieve the list of lineage concept keys that we need to look at for 
	inherited relationships, for both the From and the To ends.
	Note that Concept_Group_Key is included and any concept group is matched, 
	then the data is filtered afer.  This seems much faster than filtering
	out the concept group key at the start.
\*===========================================================================*/
IF @IncludeInherited=1 
BEGIN
  INSERT INTO @FromConceptKeys (Concept_Key, Ancestor, Concept_Group_Key) 
		SELECT DISTINCT CL2.Concept_Key, 1, C.Concept_Group_Key
		FROM @FromConceptKeys F
		INNER JOIN Concept_Lineage CL1 ON CL1.Concept_Key=F.Concept_Key
		INNER JOIN Concept_Lineage CL2 ON CL2.Lineage = LEFT(CL1.Lineage, LEN(CL2.Lineage))
		INNER JOIN Concept C ON C.Concept_Key=CL2.Concept_Key
		LEFT JOIN @FromConceptKeys F2 ON F2.Concept_Key=CL2.Concept_Key
		WHERE F2.Concept_Key IS NULL 

	INSERT INTO @ToConceptKeys (Concept_Key, Leaf_Concept_Key, Concept_Group_Key, Ancestor)
		SELECT DISTINCT C.Concept_Key, T.Concept_Key, C.Concept_Group_Key, 1
		FROM @ToConceptKeys T
		INNER JOIN Concept_Lineage CL1 ON CL1.Concept_Key=T.Concept_Key
		INNER JOIN Concept_Lineage CL2 ON CL2.Lineage = LEFT(CL1.Lineage, LEN(CL2.Lineage))
		INNER JOIN Concept C ON C.Concept_Key=CL2.Concept_Key
		LEFT JOIN @ToConceptKeys T2 ON T2.Concept_Key=CL2.Concept_Key
				AND T2.Leaf_Concept_Key=T.Concept_Key Collate SQL_Latin1_General_CP1_CI_AS
		WHERE T2.Concept_Key IS NULL 

		DELETE FROM @FromConceptKeys WHERE Concept_Group_Key<>@FromConceptGroupKey

END


SELECT DISTINCT
		'Concept' AS Type, 
		'Forward' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Concept_Relation CR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=CR.Thesaurus_Relation_Type_Key
INNER JOIN @FromConceptKeys F ON From_Concept_Key = F.Concept_Key
INNER JOIN @ToConceptKeys T ON To_Concept_Key=T.Concept_Key
WHERE @IncludeInherited=0 OR (CR.Inherited=1 AND (F.Ancestor=1 OR T.Ancestor=1))
UNION
SELECT 
		'Concept' AS Type, 
		'Reverse' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Concept_Relation CR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=CR.Thesaurus_Relation_Type_Key
INNER JOIN @FromConceptKeys F ON To_Concept_Key = F.Concept_Key
INNER JOIN @ToConceptKeys T ON From_Concept_Key=T.Concept_Key
WHERE @IncludeInherited=0 OR (CR.Inherited=1 AND (F.Ancestor=1 OR T.Ancestor=1))
UNION
SELECT 
		'Meaning' AS Type, 
		'Forward' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Meaning_Relation MR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=MR.Thesaurus_Relation_Type_Key
INNER JOIN Concept CFrom ON CFrom.Meaning_Key=MR.From_Meaning_Key
INNER JOIN Concept CTo ON CTo.Meaning_Key=MR.To_Meaning_Key
INNER JOIN @FromConceptKeys F ON CFrom.Concept_Key = F.Concept_Key
INNER JOIN @ToConceptKeys T ON CTo.Concept_Key=T.Concept_Key
WHERE @IncludeInherited=0 OR (MR.Inherited=1 AND (F.Ancestor=1 OR T.Ancestor=1))
UNION
SELECT 
		'Meaning' AS Type, 
		'Reverse' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Meaning_Relation MR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=MR.Thesaurus_Relation_Type_Key
INNER JOIN Concept CFrom ON CFrom.Meaning_Key=MR.From_Meaning_Key
INNER JOIN Concept CTo ON CTo.Meaning_Key=MR.To_Meaning_Key
INNER JOIN @FromConceptKeys F ON CTo.Concept_Key=F.Concept_Key
INNER JOIN @ToConceptKeys T ON CFrom.Concept_Key=T.Concept_Key
WHERE @IncludeInherited=0 OR (MR.Inherited=1 AND (F.Ancestor=1 OR T.Ancestor=1))
UNION
SELECT 
		'TermVersion' AS Type, 
		'Forward' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Term_Version_Relation TVR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
INNER JOIN Concept CFrom ON CFrom.Term_Version_Key=TVR.From_Term_Version_Key
INNER JOIN Concept CTo ON CTo.Term_Version_Key=TVR.To_Term_Version_Key
INNER JOIN @ToConceptKeys T ON T.Concept_Key=CTo.Concept_Key
WHERE CFrom.Concept_Key=@FromKey
AND @IncludeInherited=0
UNION
SELECT 
		'TermVersion' AS Type, 
		'Reverse' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Term_Version_Relation TVR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
INNER JOIN Concept CFrom ON CFrom.Term_Version_Key=TVR.From_Term_Version_Key
INNER JOIN Concept CTo ON CTo.Term_Version_Key=TVR.To_Term_Version_Key
INNER JOIN @ToConceptKeys T ON T.Concept_Key=CFrom.Concept_Key
WHERE CTo.Concept_Key=@FromKey
AND @IncludeInherited=0

SET NOCOUNT OFF

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptToConceptRelations_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptToConceptRelations_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [Dev - JNCC SQL]
END

GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import concepts corresponding to the contents of a taxon list.

  Parameters:	@job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 3 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON

	DECLARE     @taxon_list_key				CHAR(16),
				@concept_group_key			CHAR(16),
				@taxon_list_item_key		CHAR(16),
				@taxon_version_key			CHAR(16),
				@term_key					CHAR(16),
				@term_version_key			CHAR(16),
				@list_preferred				BIT,
				@is_current					BIT,
				@is_preferred				BIT,
				@taxon_rank_key				CHAR(16),
				@rank_uses_italics			BIT,
				@concept_rank_key			CHAR(16),
				@name_type_concept_key		CHAR(16),
				@sort_code					INT,
				@ins_user_key				CHAR(16),
				@ins_date					SMALLDATETIME,
				@ins_session_id				CHAR(16),
				@upd_user_key				CHAR(16),
				@upd_date					SMALLDATETIME,
				@upd_session_id				CHAR(16),
				@system						BIT,
				@preferred_name				CHAR(16),
				@taxon_list_version_from	CHAR(16),
				@taxon_list_version_to		CHAR(16),	
				@concept_group_version_from	CHAR(16),
				@concept_group_version_to	CHAR(16),
				@meaning_key				CHAR(16),
				@concept_key				CHAR(16),
				@domain_key					CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key,
				@concept_group_key						=	m.Concept_Group_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	SELECT		@domain_key								=	LD.Domain_Key
	FROM		Concept_Group CG
	INNER JOIN	Local_Domain LD 
	ON LD.Local_Domain_Key								=	CG.Local_Domain_Key
	WHERE		CG.Concept_Group_Key					=	@concept_group_key

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing concepts'
	IF @@ERROR <> 0 RETURN

	/* remove current lineage data */
	DELETE		l
	FROM		Concept					AS	c
	INNER JOIN	Concept_Lineage			AS	l
	ON			l.Concept_Key			=	c.Concept_Key
	WHERE		c.Concept_Group_Key		=	@concept_group_key

	IF @@ERROR <> 0 RETURN

	DECLARE		@items	TABLE (
				Taxon_List_Item_Key	CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				Rank_Uses_Italic	BIT)

	INSERT		@items
	SELECT      tli.TAXON_LIST_ITEM_KEY,
				tr.LIST_FONT_ITALIC
	FROM        TAXON_LIST_VERSION				AS	tlv
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY		=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_RANK						AS	tr
	ON			tr.TAXON_RANK_KEY				=	tli.TAXON_RANK_KEY
	WHERE		tlv.TAXON_LIST_KEY				=	@taxon_list_key

	DECLARE		items		CURSOR FAST_FORWARD LOCAL FOR
	SELECT		tli.TAXON_LIST_ITEM_KEY,
				tli.TAXON_VERSION_KEY,
				CASE WHEN tli.TAXON_LIST_ITEM_KEY = tli.PREFERRED_NAME
					THEN 1	/* list preferred */
					ELSE 0
				END,
				CASE WHEN tli.TAXON_LIST_VERSION_TO IS NULL
					THEN 1	/* current */
					ELSE 0
				END,
				tli.TAXON_RANK_KEY,
				itm.Rank_Uses_Italic,
				tli.SORT_CODE,
				tli.ENTERED_BY,
				tli.ENTRY_DATE,
				tli.CHANGED_BY,
				tli.CHANGED_DATE,
				tli.SYSTEM_SUPPLIED_DATA,
				tli.PREFERRED_NAME,
				tli.TAXON_LIST_VERSION_KEY,
				tli.TAXON_LIST_VERSION_TO
	FROM		@items							AS	itm
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_ITEM_KEY			=	itm.TAXON_LIST_ITEM_KEY

	OPEN        items

	WHILE 1 = 1
	BEGIN
		FETCH		items
		INTO		@taxon_list_item_key,
					@taxon_version_key,
					@list_preferred,
					@is_current,
					@taxon_rank_key,
					@rank_uses_italics,
					@sort_code,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system,
					@preferred_name,
					@taxon_list_version_from,
					@taxon_list_version_to

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		/* perform mappings */
		SELECT		@term_version_key						=	Term_Version_Key
		FROM		Taxon_Dictionary_Term_Version_Mapping
		WHERE		Taxon_Version_Key						=	@taxon_version_key

		IF @@ROWCOUNT = 0 GOTO skip_item

		SELECT		@term_key								=	tm.Term_Key,
					@name_type_concept_key					=	ntm.Thesaurus_Name_Type_Key
		FROM		TAXON_VERSION							AS	tv
		INNER JOIN	TAXON									AS	tx
		ON			tx.TAXON_KEY							=	tv.TAXON_KEY
		INNER JOIN	Taxon_Dictionary_Term_Mapping			AS	tm
		ON			tm.Taxon_Key							=	tx.TAXON_KEY
		AND			tm.Italic_Font							=	CASE WHEN tx.Language = 'La'
																	 AND @rank_uses_italics = 1
																	THEN 1
																	ELSE 0
																END
		INNER JOIN	Taxon_Dictionary_Name_Type_Mapping		AS	ntm
		ON			ntm.Taxon_Name_Type_Key					=	tx.TAXON_NAME_TYPE_KEY
		WHERE		tv.TAXON_VERSION_KEY					=	@taxon_version_key

		IF @@ROWCOUNT = 0 GOTO skip_item

		SELECT		@concept_rank_key						=	M.Concept_Rank_Key
		FROM		Taxon_Dictionary_Concept_Rank_Mapping M
		INNER JOIN	Concept_Rank CR 
					ON CR.Concept_Rank_Key					=	M.Concept_Rank_Key
					AND	CR.Domain_Key						=	@domain_key
		WHERE		M.Taxon_Rank_Key						=	@taxon_rank_key

		IF @@ROWCOUNT = 0 GOTO skip_item

		IF @list_preferred = 1
			SET			@is_preferred		=	1
		ELSE
		BEGIN
			SELECT		@is_preferred 		=	CASE WHEN TAXON_VERSION_KEY = @taxon_version_key
													THEN 1
													ELSE 0
												END
			FROM		TAXON_COMMON_NAME
			WHERE		TAXON_LIST_ITEM_KEY	=	@taxon_list_item_key
		END

		SELECT      @concept_group_version_from						=	Concept_Group_Version_Key
		FROM		Taxon_Dictionary_Concept_Group_Version_Mapping
		WHERE		Taxon_List_Version_Key							=   @taxon_list_version_from

		IF @@ROWCOUNT = 0 GOTO skip_item

		SELECT		@concept_group_version_to						=	Concept_Group_Version_Key
		FROM		Taxon_Dictionary_Concept_Group_Version_Mapping
		WHERE		Taxon_List_Version_Key							=	@taxon_list_version_to

		IF @@ROWCOUNT = 0
		BEGIN
			SET			@concept_group_version_to	=	NULL
		END

		/* obtain meaning key */
		SELECT		@meaning_key						=	Meaning_Key
		FROM        Taxon_Dictionary_Meaning_Mapping
		WHERE		Preferred_Name						=	@preferred_name

		IF @@ROWCOUNT = 0
		BEGIN
			/* look for meaning assigned to synonyms of @preferred_name from
			 * some other taxon list */
			SELECT		@meaning_key						=	tdm.Meaning_Key
			FROM		INDEX_TAXON_SYNONYM					AS	its
			INNER JOIN	Taxon_Dictionary_Meaning_Mapping	AS	tdm
			ON			tdm.Preferred_Name					=	its.SYNONYM_LIST_ITEM_KEY
			WHERE		its.TAXON_LIST_ITEM_KEY				=	@preferred_name

			IF @@ROWCOUNT = 0
			BEGIN
				/* create new meaning */
				EXECUTE		spNextKey	'Meaning',
										@meaning_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Meaning (
							Meaning_Key)
				VALUES		(@meaning_key)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END

			INSERT		Taxon_Dictionary_Meaning_Mapping (
						Preferred_Name,
						Meaning_Key)
			VALUES		(@preferred_name,
						@meaning_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		IF @meaning_key IS NOT NULL
				/* meaning not explicitly mapped to null,
				 * so we can import item */
		BEGIN
			/* obtain session identifiers */
			EXECUTE		usp_Session_ForDate		@ins_user_key,
												@ins_date,
												@ins_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @upd_user_key IS NULL OR @upd_date IS NULL
			BEGIN
				SET			@upd_session_id		=	NULL
			END
			ELSE
			BEGIN
				EXECUTE		usp_Session_ForDate		@upd_user_key,
													@upd_date,
													@upd_session_id		OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor
			END

			SELECT      @concept_key						=	Concept_Key
			FROM		Taxon_Dictionary_Concept_Mapping
			WHERE		Taxon_List_Item_Key					=	@taxon_list_item_key

			IF @@ROWCOUNT > 0
			BEGIN
				DECLARE		@old_group_key			CHAR(16),
							@was_list_preferred		BIT
							
				/* update concept */
				UPDATE		Concept
				SET         @old_group_key				=	Concept_Group_Key,
							@was_list_preferred			=	List_Preferred,
							Term_Key					=	@term_key,
							Concept_Group_Key			=	@concept_group_key,
							Term_Version_Key			=	@term_version_key,
							List_Preferred				=	@list_preferred,
							Is_Current					=	@is_current,
							Preferred					=	@is_preferred,
							Concept_Rank_Key			=	@concept_rank_key,
							Name_Type_Concept_Key		=   @name_type_concept_key,
							Meaning_Key					=	@meaning_key,
							Sort_Code					=	@sort_code,
							Entered_Session_ID			=	@ins_session_id,
							Changed_Session_ID			=	@upd_session_id,
							System_Supplied_Data		=	@system
				WHERE		Concept_Key					=	@concept_key

				IF @@ERROR <> 0 GOTO fail_from_cursor

				/* re-create concept history */
				DELETE		Concept_History
				WHERE		Concept_Key					=	@concept_key

				IF @@ERROR <> 0 GOTO fail_from_cursor

				EXECUTE		usp_ConceptHistory_Insert_Imported	@concept_key,
																@concept_group_version_from,
																@concept_group_version_to
				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
			ELSE
			BEGIN
				/* create concept */
				EXECUTE		spNextKey	'Concept',
										@concept_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Concept
							(Concept_Key,
							Term_Key,
							Concept_Group_Key,
							Term_Version_Key,
							List_Preferred,
							Is_Current,
							Preferred,
							Concept_Rank_Key,
							Name_Type_Concept_Key,
							Meaning_Key,
							Sort_Code,
							Entered_Session_ID,
							Changed_Session_ID,
							System_Supplied_Data)
				VALUES		(@concept_key,
							@term_key,
							@concept_group_key,
							@term_version_key,
							@list_preferred,
							@is_current,
							@is_preferred,
							@concept_rank_key,
							@name_type_concept_key,
							@meaning_key,
							@sort_code,
							@ins_session_id,
							@upd_session_id,
							@system)

				IF @@ERROR <> 0 GOTO fail_from_cursor

				/* create concept history */
				EXECUTE		usp_ConceptHistory_Insert_Imported	@concept_key,
																@concept_group_version_from,
																@concept_group_version_to
				IF @@ERROR <> 0 GOTO fail_from_cursor

				/* record mapping */
				INSERT		Taxon_Dictionary_Concept_Mapping
							(Taxon_List_Item_Key,
							Concept_Key)
				VALUES		(@taxon_list_item_key,
							@concept_key)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		
skip_item:
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		items
	DEALLOCATE	items
	RETURN

fail_from_cursor:
	CLOSE		items
	DEALLOCATE	items

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects 
       WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForConceptGroupSearch]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupSearch]
GO

/*===========================================================================*\
  Description:  Retrieves a list of concepts that match a search string, in a 
                                specified concept group.

  Parameters:   @ConceptGroup - key of the concept group
                            @SearchText - search text

  Created:  August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupSearch]
	@SearchKey char(16),
	@SearchText varchar(100),
	@SearchSize int = NULL
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

    SET @SearchSize =   ISNULL(@SearchSize, 0)

    SET ROWCOUNT @SearchSize

	SELECT Concept_Key as Item_Key,
	  CASE WHEN Author_Copy IS NULL THEN
		Item_Name
	  ELSE
		Item_Name + ' ' + Author_Copy
	  END AS DisplayTerm,
	  CASE WHEN Author_Copy IS NULL THEN
		Plaintext
	  ELSE
		Plaintext + ' ' + Author_Copy COLLATE SQL_Latin1_General_CP1_CI_AI
	  END AS SearchTerm,
	  Author_copy,
	  Concept_Rank_Key
	FROM VW_ConceptTerm
	WHERE Concept_Group_Key = @SearchKey
	AND (Plaintext like @SearchText + '%'
	OR Author_Copy like @SearchText + '%')
	AND Is_Current = 1
	ORDER BY SearchTerm, Author_Copy

    SET ROWCOUNT 0    
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForConceptGroupSearch') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_Select_ForConceptGroupSearch'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForConceptGroupVersionSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupVersionSearch]
GO

/*===========================================================================*\
  Description:	Retrieves a list of concepts that match a search string, in a 
 								specified concept group version.

  Parameters:	@SearchKey - key of the concept group version
							@SearchText - search text

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupVersionSearch]
	@SearchKey char(16),
  @SearchText varchar(100)
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF


SELECT CT.Concept_Key as Item_Key, 
  CASE WHEN CT.Author_Copy IS NULL THEN
    CT.Item_Name 
  ELSE
    CT.Item_Name + ' ' + CT.Author_Copy  
  END AS DisplayTerm, 
  CASE WHEN CT.Author_Copy IS NULL THEN
    CT.Plaintext 
  ELSE
    CT.Plaintext + ' ' + CT.Author_Copy COLLATE SQL_Latin1_General_CP1_CI_AI  
  END AS SearchTerm, 
  CT.Author_copy,
  CT.Concept_Rank_Key
FROM VW_ConceptTerm CT
  INNER JOIN Concept_Group_Version CGV on CGV.Concept_Group_Key=CT.Concept_Group_Key
      AND CGV.Concept_Group_Version_Key=@SearchKey
  LEFT JOIN Concept_History CH on CH.Concept_Key=CT.Concept_Key
  LEFT JOIN Concept_Group_Version CGV1 ON CGV1.Concept_Group_Version_Key=CH.Concept_Group_Version_From
  LEFT JOIN Concept_Group_Version CGV2 ON CGV2.Concept_Group_Version_Key=CH.Concept_Group_Version_To
WHERE (CT.Plaintext like @SearchText + '%'
  OR CT.Author_Copy like @SearchText + '%')
  AND (CGV1.Concept_Group_Version_Key IS NULL OR CGV1.Sequence<=CGV.Sequence)
  AND (CGV2.Concept_Group_Version_Key IS NULL OR CGV2.Sequence>=CGV.Sequence)
ORDER BY CT.SearchTerm, CT.Author_Copy
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForConceptGroupVersionSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForConceptGroupVersionSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects 
       WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForSubjectAreaSearch]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForSubjectAreaSearch]
GO

/*===========================================================================*\
  Description:  Retrieves a list of concepts that match a search string, in a 
                                specified subject area.

  Parameters:   @SearchKey - key of the subject area
                            @SearchText - search text

  Created:  August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForSubjectAreaSearch]
	@SearchKey char(16),
	@SearchText varchar(100),
	@SearchSize int = NULL
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

    SET @SearchSize =   ISNULL(@SearchSize, 0)

    SET ROWCOUNT @SearchSize

	SELECT Concept_Key as Item_Key,
	  CASE WHEN Author_Copy IS NULL THEN
		CT.Item_Name
	  ELSE
		CT.Item_Name + ' ' + Author_Copy
	  END AS DisplayTerm,
	  CASE WHEN Author_Copy IS NULL THEN
		CT.Plaintext
	  ELSE
		CT.Plaintext + ' ' + Author_Copy COLLATE SQL_Latin1_General_CP1_CI_AI
	  END AS SearchTerm,
	  CT.Author_copy,
	  CT.Concept_Rank_Key
	FROM VW_ConceptTerm CT
	INNER JOIN Concept_Group CG ON CT.Concept_Group_Key = CG.Concept_Group_Key
	INNER JOIN Local_Domain LD ON CG.Local_Domain_Key = LD.Local_Domain_Key
	INNER JOIN Domain D ON LD.Domain_Key = D.Domain_Key

	WHERE D.Subject_Area_Key = @SearchKey
	AND (CT.Plaintext like @SearchText + '%'
	OR CT.Author_Copy like @SearchText + '%')
	AND CT.Is_Current = 1
	ORDER BY CT.SearchTerm, CT.Author_Copy

    SET ROWCOUNT 0    
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForSubjectAreaSearch') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_Select_ForSubjectAreaSearch'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForVersionParent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForVersionParent]
GO

/*===========================================================================*\
  Description: Returns a list of concepts that are with the supplied parent,
      				 for a given concent group version	

  Parameters:	@ParentConceptKey
							@ConceptGroupVersionKey
							@HierarchyRelationTypeKey - relationship type used to populate
							hierarchy.

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForVersionParent]
	@ParentConceptKey varchar(100),
  @ConceptGroupVersionKey char(16),
  @HierarchyRelationTypeKey char(16)
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

SELECT DISTINCT CT.Concept_Key, CT.Item_Name, CT.Concept_Rank_Key, CT.Sort_Code,
	CASE WHEN CRChild.To_Concept_Key IS NULL THEN 0 ELSE 1 END AS HasChildren
FROM VW_ConceptTerm CT
-- Find any parents so we can filter for top level
INNER JOIN Concept_Relation CRParent 
	ON CRParent.To_Concept_Key=CT.Concept_Key
	AND CRParent.Thesaurus_Relation_Type_Key=@HierarchyRelationTypeKey
	AND CRParent.From_Concept_Key = @ParentConceptKey
-- Filter all concepts from concept group
INNER JOIN Concept_Group_Version CGV 
		ON CGV.Concept_Group_Key=CT.Concept_Group_Key
		AND CGV.Concept_Group_Version_Key=@ConceptGroupVersionKey
-- Filter to all concepts where history puts them into the concept group version
INNER JOIN (
	Concept_History CH
	LEFT JOIN Concept_Group_Version CGVFrom ON CGVFrom.Concept_Group_Version_Key=CH.Concept_Group_Version_From
	LEFT JOIN Concept_Group_Version CGVTo ON CGVTo.Concept_Group_Version_Key=CH.Concept_Group_Version_To
		) ON CH.Concept_Key=CT.Concept_Key
-- Find any children so we can return the Has_Children flag
LEFT JOIN (Concept_Relation CRChild 		
	-- Filter to all concepts where history puts them into the concept group version
	INNER JOIN Concept_History CHChild ON CHChild.Concept_Key=CRChild.To_Concept_Key
	LEFT JOIN Concept_Group_Version CGVFromChild ON CGVFromChild.Concept_Group_Version_Key=CHChild.Concept_Group_Version_From
	LEFT JOIN Concept_Group_Version CGVToChild ON CGVToChild.Concept_Group_Version_Key=CHChild.Concept_Group_Version_To
	) ON CRChild.From_Concept_Key=CT.Concept_Key AND CRChild.Thesaurus_Relation_Type_Key=@HierarchyRelationTypeKey
WHERE (CH.Concept_Group_Version_From IS NULL OR CGVFrom.Sequence<=CGV.Sequence)
	AND (CH.Concept_Group_Version_To IS NULL OR CGVTo.Sequence>=CGV.Sequence)
	AND (CHChild.Concept_Group_Version_From IS NULL OR CGVFromChild.Sequence<=CGV.Sequence)
	AND (CHChild.Concept_Group_Version_To IS NULL OR CGVToChild.Sequence>=CGV.Sequence)
ORDER BY CT.Sort_Code

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForVersionParent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForVersionParent'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForVersionParent TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForVersionParent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForVersionParent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForVersionParent TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForVersionParent TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForVersionParent TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForVersionTopLevel]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForVersionTopLevel]
GO

/*===========================================================================*\
  Description: Returns a list of concepts that are the top level for the 
    					 supplied concept group version.

  Parameters:	@ConceptGroupVersionKey
							@HierarchyRelationTypeKey - relationship type used to populate
							hierarchy.

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForVersionTopLevel]
	@ConceptGroupVersionKey char(16),
  @HierarchyRelationTypeKey char(16)
AS


SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

SELECT DISTINCT CT.Concept_Key, CT.Item_Name, CT.Concept_Rank_Key, CT.Sort_Code,
	CASE WHEN CRChild.To_Concept_Key IS NULL THEN 0 ELSE 1 END AS HasChildren
FROM VW_ConceptTerm CT
-- Find any parents so we can filter for top level
LEFT JOIN Concept_Relation CRParent 
	ON CRParent.To_Concept_Key=CT.Concept_Key
	AND CRParent.Thesaurus_Relation_Type_Key=@HierarchyRelationTypeKey
-- Filter all concepts from concept group
INNER JOIN Concept_Group_Version CGV 
		ON CGV.Concept_Group_Key=CT.Concept_Group_Key
		AND CGV.Concept_Group_Version_Key=@ConceptGroupVersionKey
-- Filter to all concepts where history puts them into the concept group version
INNER JOIN (
	Concept_History CH
	LEFT JOIN Concept_Group_Version CGVFrom ON CGVFrom.Concept_Group_Version_Key=CH.Concept_Group_Version_From
	LEFT JOIN Concept_Group_Version CGVTo ON CGVTo.Concept_Group_Version_Key=CH.Concept_Group_Version_To
		) ON CH.Concept_Key=CT.Concept_Key
-- Find any children so we can return the Has_Children flag
LEFT JOIN (Concept_Relation CRChild 		
	-- Filter to all concepts where history puts them into the concept group version
	INNER JOIN Concept_History CHChild ON CHChild.Concept_Key=CRChild.To_Concept_Key
	LEFT JOIN Concept_Group_Version CGVFromChild ON CGVFromChild.Concept_Group_Version_Key=CHChild.Concept_Group_Version_From
	LEFT JOIN Concept_Group_Version CGVToChild ON CGVToChild.Concept_Group_Version_Key=CHChild.Concept_Group_Version_To
	) ON CRChild.From_Concept_Key=CT.Concept_Key AND CRChild.Thesaurus_Relation_Type_Key=@HierarchyRelationTypeKey
WHERE (CH.Concept_Group_Version_From IS NULL OR CGVFrom.Sequence<=CGV.Sequence)
	AND (CH.Concept_Group_Version_To IS NULL OR CGVTo.Sequence>=CGV.Sequence)
	AND (CHChild.Concept_Group_Version_From IS NULL OR CGVFromChild.Sequence<=CGV.Sequence)
	AND (CHChild.Concept_Group_Version_To IS NULL OR CGVToChild.Sequence>=CGV.Sequence)
AND CRParent.From_Concept_Key IS NULL
ORDER BY CT.Sort_Code

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForVersionTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForVersionTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForVersionTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForVersionTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForVersionTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForVersionTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForVersionTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForVersionTopLevel TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConditionChecks_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConditionChecks_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_ConditionChecks_Select_ForSearch] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SearchText VARCHAR(100)

AS
--
--  DESCRIPTION
--  Returns Conservation_Check_Key and Display_Caption when search characters are entered.
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned.
--	@SessionID 		User's SessionID.
--	@SearchText 		Search text used to find collections.
--
--  AUTHOR:			Anthony Simpson, Dorset Software
--  CREATED:			2003-09-08

SET NOCOUNT ON

SELECT CC.Conservation_Check_Key AS Item_Key, Display_Caption AS DisplayTerm

FROM Conservation_Check CC
	INNER JOIN 	CONCEPT C
	ON 		CC.Type_Concept_Key = C.Concept_Key
	AND 		((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
				OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))

WHERE Search_Caption LIKE @SearchText+'%'
ORDER BY Display_Caption
		
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConditionChecks_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConditionChecks_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Department_Get_ForIndividual]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Department_Get_ForIndividual]
GO

/*===========================================================================*\
  Description:	Returns an department name for an individual record.

  Parameters:	@Key		Individual key
		@GetAcronym 	If this is set to 1, then the acronym of the 
				department will be returned. If the department
				does not have an acronym, then the department
				name will be returned instead.
		@Output		Department name (Output parameter)

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Department_Get_ForIndividual]
	@Key char(16),
	@GetAcronym bit = NULL,
	@Output varchar(100) output
AS

SET NOCOUNT ON

SELECT 		@Output = CASE WHEN (@GetAcronym IS NOT NULL) AND (@GetAcronym = 1) 
				THEN IsNull(OD.Acronym, OD.Item_Name)
				ELSE OD.Item_Name
			END 
FROM 		Organisation_Department OD
INNER JOIN 	Organisation O ON O.Name_Key = OD.Name_Key
INNER JOIN 	Individual I ON I.Organisation_Department_Key = OD.Organisation_Department_Key
WHERE 		I.Name_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Department_Get_ForIndividual') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Department_Get_ForIndividual'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DeptOrgPeople_Select_ForNameSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DeptOrgPeople_Select_ForNameSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of names matching a search string.

  Parameters:	@SearchText

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DeptOrgPeople_Select_ForNameSearch]
	@SearchText varchar(100)
AS
	SET NO_BROWSETABLE OFF

	SELECT	Name_Key AS Item_Key, 'Individual' AS Type,
		dbo.ufn_GetFormattedName(Name_Key) AS DisplayTerm, 
		dbo.ufn_GetFormattedName(Name_Key) AS SearchTerm
	FROM	Individual
	WHERE 	Forename LIKE @SearchText + '%'
	OR 	(Initials LIKE @SearchText + '%' AND Forename IS NULL)
	OR 	Surname LIKE @SearchText + '%'
	OR 	Title LIKE @SearchText + '%'
	OR	dbo.ufn_GetFormattedName(Name_Key) LIKE @SearchText + '%'
UNION
	SELECT	Name_Key AS Item_Key, 'Organisation' AS Type,
		dbo.ufn_GetFormattedName(Name_Key) AS DisplayTerm, 
		dbo.ufn_GetFormattedName(Name_Key) AS SearchTerm
	FROM	Organisation
	WHERE	Acronym LIKE @SearchText + '%'
	OR	Full_Name LIKE @SearchText + '%'
	OR	dbo.ufn_GetFormattedName(Name_Key) LIKE @SearchText + '%'
UNION
	SELECT	Organisation_Department_Key + '*' AS Item_Key, 'Department' AS Type,
		Item_Name AS DisplayTerm, 
		Item_Name AS SearchTerm
	FROM	Organisation_Department
	WHERE	Item_Name LIKE @SearchText + '%'
	OR	dbo.ufn_GetFormattedName(Name_Key) LIKE @SearchText + '%'

	-- Set the order here for all
	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeptOrgPeople_Select_ForNameSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DeptOrgPeople_Select_ForNameSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DeptOrgPeople_Select_ForNameSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DeptOrgPeople_Select_ForNameSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DeptOrgPeople_Select_ForNameSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DeptOrgPeople_Select_ForNameSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DeptOrgPeople_Select_ForNameSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DeptOrgPeople_Select_ForNameSearch TO [Dev - JNCC SQL]
END

GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationsEarthSciences_Select_ForSearch') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_DeterminationsEarthSciences_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns Concept_Key and DisplayTerm when search characters are 
		entered.

  Parameters:	@SearchText
		@UserDomainMask

  Created:	October 2003

  Last revision information:
	$Revision: 3 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DeterminationsEarthSciences_Select_ForSearch] 
	@SearchText VARCHAR(100),
	@UserDomainMask INT
AS

	SET NOCOUNT ON

	SELECT 		VW.Concept_Key AS Item_Key,
			VW.Item_Name + ' - ' + CG.Item_Name AS DisplayTerm,
			VW.Item_Name + ' - ' + CG.Item_Name AS SearchTerm, 
			CG.Item_Name

	FROM		VW_ConceptTerm AS VW 
	INNER JOIN 	Concept_Group CG ON CG.Concept_Group_Key = VW.Concept_Group_Key
	INNER JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
	INNER JOIN 	Domain D ON D.Domain_Key = LD.Domain_Key
			AND ((D.Domain_Mask & @UserDomainMask > 0) OR (D.Domain_Mask = 0))
			AND D.Has_Occurrences = 1
	-- Join to find out which concepts are mapped to taxa
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping TDCM ON TDCM.Concept_Key = VW.Concept_Key

	WHERE 		VW.PlainText LIKE @SearchText + '%'
	-- And filter out all concepts that are mapped to any taxon.
	AND		TDCM.Concept_Key IS NULL
	
	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationsEarthSciences_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DeterminationsEarthSciences_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_DeterminationsLifeSciences_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_DeterminationsLifeSciences_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_DeterminationsLifeSciences_Select_ForSearch] 
@SearchText VARCHAR(100),
@UserDomainMask int,
@SearchSize int = NULL
AS
--
--  DESCRIPTION
--  Returns Concept_Key and DisplayTerm when search characters are entered.
--  Uses domain mask if the taxon list item is mapped
--
--	PARAMETERS
--	NAME			
--	@SearchText
--	@UserDomainMask 		
--
--  AUTHOR:			Anthony Simpson, Dorset Software
--  CREATED:			2003-10-20

SET NOCOUNT ON

    SET @SearchSize = ISNULL(@SearchSize, 0)

    SET ROWCOUNT @SearchSize

	SELECT
			ITN.Taxon_List_Item_Key AS Item_Key, 
			ITN.Preferred_Name + ' - ' + TL.Item_Name AS DisplayTerm, 
			ITN.Preferred_Name + ' - ' + TL.Item_Name AS SearchTerm
	FROM		Index_Taxon_Name AS ITN 
	INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
	INNER JOIN Taxon_List TL ON TL.Taxon_List_Key=TLV.Taxon_List_Key
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping TDCM ON TDCM.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
	LEFT JOIN	Concept C ON C.Concept_Key = TDCM.Concept_Key
	LEFT JOIN 	Concept_Group CG ON CG.Concept_Group_Key=C.Concept_Group_Key
	LEFT JOIN 	Local_Domain LD ON LD.Local_Domain_Key=CG.Local_Domain_Key
	LEFT JOIN 	Domain D ON D.Domain_Key=LD.Domain_Key
	WHERE 		ITN.Preferred_Name LIKE @SearchText + '%'
			AND ((((D.Domain_Mask & @UserDomainMask>0) OR (D.Domain_Mask = 0))
			AND D.Has_Occurrences = 1) OR D.Domain_Key IS NULL)

	ORDER BY DisplayTerm

    SET ROWCOUNT 0
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationsLifeSciences_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DeterminationsLifeSciences_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Determination_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Determination_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Determination table.
		Ensures the Domain mask of the specimen is also updated.

  Parameters:	@Key 
		@DeterminedItemKey
		@OccurrenceKey 
		@SpecimenCollectionUnitKey  
		@DeterminationTypeKey  
		@NomenclaturalStatusConceptKey 
		@Confidence
		@DeterminerNameKey 
		@InferredDeterminer 
		@DeterminerRoleKey 
		@VagueDateStart
		@VagueDateEnd 
		@VagueDateType 
		@UsedSpecimen 
		@Preferred
		@Method
		@Notes 
		@SessionID 
		@Timestamp
		@IsForSpecimen		Indicates whether to update preferred 
					flag in Specimen_Unit or Determination.
		@RecordsAffected	OUTPUT Can't rely on correct value to come out,
					so use a parameter instead.

  Created:	July 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Determination_Update]
	@Key char(16), 
	@DeterminedItemKey char(16), 
	@OccurrenceKey char(16), 
	@SpecimenCollectionUnitKey char(16), 
	@DeterminationTypeKey char(16), 
	@NomenclaturalStatusConceptKey char(16),
	@Confidence tinyint,
	@DeterminerNameKey char(16), 
	@InferredDeterminer tinyint,
	@DeterminerRoleKey char(16), 
	@VagueDateStart int, 
	@VagueDateEnd int, 
	@VagueDateType varchar(2),
	@UsedSpecimen bit,
	@Preferred bit,
	@Method text,
	@Notes text,
	@SessionID char(16),
	@Timestamp timestamp,
	@IsForSpecimen bit,
	@RecordsAffected int OUTPUT
AS

	DECLARE @SpecimenKey char(16),
		@CurrentConceptKey char(16),
		@CurrentConceptMask int,
		@CurrentPreferred bit,
		@ConceptMask int

	BEGIN TRANSACTION
		-- Retrieve the mask of the new concept.
		EXECUTE	usp_Get_Concept_Domain_Mask @DeterminedItemKey, @ConceptMask OUTPUT

		/*-------------------------------------------------------------*\
		  Determine if and where to update the preferred states.
		  And also if specimen mask needs to be update.

		  If @Preferred is 0, then we are updating non-preferred
		  determination and therefore, no need to reset the one that is
		  still preferred. This only ensures there is only 1 preferred
		  at any time for the specimen.

		  We only do this if the preferred determination has actually
		  changed determination. This is necessary to avoid a timestamp
		  error. For example - if @Preferred were 1, and there was no
		  checking to see if the preferred determination had changed,
		  the record's Preferred field and the record's Timestamp would
		  be altered. The main update would then fail because the
		  timestamp is different to the one passed into the procedure.
		\*-------------------------------------------------------------*/
		IF @IsForSpecimen = 1 AND @Preferred = 1 BEGIN
			DECLARE	@CurrentPrefDetKey char(16),
				@CurrentPrefTaxonDetKey char(16)

			-- Get existing preferred keys from Specimen_Unit table
			SELECT	@CurrentPrefDetKey = Preferred_Determination_Key,
				@CurrentPrefTaxonDetKey = Preferred_Taxon_Determination_Key
			FROM	Specimen_Unit
			WHERE	Collection_Unit_Key = @SpecimenCollectionUnitKey

			-- Changing to another preferred Determination
			IF (@CurrentPrefDetKey <> @Key) OR (@CurrentPrefDetKey IS NULL) BEGIN
				-- Get existing concept's key
				SELECT	@CurrentConceptKey = Concept_Key
				FROM	Determination
				WHERE	Determination_Key = @Key

				-- We're having a new preferred, so replace the old one.
				UPDATE	Specimen_Unit
				SET	Preferred_Determination_Key = @Key
				WHERE	Collection_Unit_Key = @SpecimenCollectionUnitKey

				IF @@Error <> 0 GOTO RollbackAndExit

				-- Preferred state should NOT change in Determination, so get existing value to
				-- override parameter value.
				SELECT	@Preferred = Preferred
				FROM	Determination
				WHERE	Determination_Key = @Key
			END

			-- Get existing concept's mask
			EXECUTE	usp_Get_Concept_Domain_Mask @CurrentConceptKey, @CurrentConceptMask OUTPUT

			-- Different mask, so switch current one OFF in Collection_Unit
			IF @CurrentConceptMask <> @ConceptMask BEGIN
				EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenCollectionUnitKey, @CurrentConceptMask, 0
				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END ELSE 
		-- Dealing with Determination/Taxon_Determination tables only, for occurrences.
		-- Also means that Specimen_Collection_Unit_Key can be NULL, but that doesn't mean
		-- the value passed in is NULL though.
		IF @IsForSpecimen = 0 AND @Preferred = 1 BEGIN
			-- Not guaranteed there is an associated specimen key for the preferred determination	.
			SELECT	@CurrentConceptKey = Concept_Key,
				@SpecimenKey = Specimen_Collection_Unit_Key
			FROM	Determination
			WHERE	Determination_Key IN (
					SELECT	D.Determination_Key
					FROM	Determination D 
					WHERE	D.Occurrence_Key = @OccurrenceKey
				UNION
					SELECT	D.Determination_Key
					FROM	Specimen_Field_Data SFD 
					JOIN	Specimen_Unit SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
					JOIN	Determination D ON D.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key
					WHERE	SFD.Occurrence_Key = @OccurrenceKey
				)
			AND	Preferred = 1

			-- We're having a new preferred, so switch the old one OFF
			-- But only if not changing preferred, or updating WILL mess up timestamp flag!
			UPDATE	Determination
			SET	Preferred = 0
			WHERE	Determination_Key IN (
					SELECT	D.Determination_Key
					FROM	Determination D 
					WHERE	D.Occurrence_Key = @OccurrenceKey
				UNION
					SELECT	D.Determination_Key
					FROM	Specimen_Field_Data SFD 
					JOIN	Specimen_Unit SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
					JOIN	Determination D ON D.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key
					WHERE	SFD.Occurrence_Key = @OccurrenceKey
				)
			AND	Preferred = 1
			AND	Determination_Key <> @Key 

			IF @@Error <> 0 GOTO RollbackAndExit

			-- Get existing concept's mask.
			EXECUTE	usp_Get_Concept_Domain_Mask @CurrentConceptKey, @CurrentConceptMask OUTPUT

			-- New concept's mask different from current one. Refresh specimen mask is there is one.
			IF @SpecimenKey IS NOT NULL AND @CurrentConceptMask <> @ConceptMask BEGIN
				EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenKey, @CurrentConceptMask, 0
				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END

		/*-------------------------------------------------------------*\
		  Do the table update.
		\*-------------------------------------------------------------*/
		UPDATE	Determination
		SET	Concept_Key = @DeterminedItemKey,
			Occurrence_Key = @OccurrenceKey,
			Specimen_Collection_Unit_Key = @SpecimenCollectionUnitKey,
			Determination_Type_Key = @DeterminationTypeKey,
			Nomenclatural_Status_Concept_Key = @NomenclaturalStatusConceptKey,
			Confidence = @Confidence,
			Determiner_Name_Key = @DeterminerNameKey,
			Inferred_Determiner = @InferredDeterminer,
			Determiner_Role_Key = @DeterminerRoleKey,
			Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Vague_Date_Type = @VagueDateType,
			Used_Specimen = @UsedSpecimen,
			Preferred = @Preferred,
			Method = @Method,
			Notes = @Notes,
			Changed_Session_ID = @SessionID
		WHERE	Determination_Key = @Key
		AND	(@Timestamp = Timestamp)

		SET @RecordsAffected = @@RowCount

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Switch bit of new mask ON in Collection_Unit.
		\*-------------------------------------------------------------*/
		IF @SpecimenCollectionUnitKey IS NOT NULL BEGIN
			EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenCollectionUnitKey, @ConceptMask, 1
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determination_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Determination_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Determination_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Determination_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Determination_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Determination_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Determination_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DomainsForMask_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DomainsForMask_Get]
GO

/*===========================================================================*\
  Description:	Returns a semi-colon delimited list of domain names for the 
		given domain mask.

  Parameters:	@Mask		Mask to decode
		@Domains	List of domains

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DomainsForMask_Get]
	@Mask int,
	@Domains varchar(1000) OUTPUT
AS

	DECLARE	@Name varchar(100)
	
	SET @Domains = ''
	
	DECLARE domain_cursor CURSOR FOR
		SELECT item_name
		FROM Domain D
		WHERE Domain_Mask & @Mask > 0

	OPEN domain_cursor

	FETCH NEXT FROM domain_cursor INTO @Name

	WHILE @@FETCH_STATUS=0 
	BEGIN
		if @Domains<>''
			SET @Domains = @Domains + '; ' + @Name
		ELSE
			SET @Domains = @Name
		FETCH NEXT FROM domain_cursor INTO @Name
	END

CLOSE domain_cursor
DEALLOCATE domain_cursor

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DomainsForMask_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DomainsForMask_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DomainsForMask_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DomainsForMask_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DomainsForMask_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DomainsForMask_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DomainsForMask_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DomainsForMask_Get TO [Dev - JNCC SQL]
END

GO


If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Enquiries_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Enquiries_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_Enquiries_Select_ForSearch] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SearchText VARCHAR(100)

AS
--
--  DESCRIPTION
--  Returns Enquiry_Key and Enquiry caption as search characters are entered.
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned.
--	@SessionID 		User's SessionID.
--	@SearchText 		Search text used to find collections.
--
--  AUTHOR:			Anthony Simpson, Dorset Software
--  CREATED:			2003-09-19

SET NOCOUNT ON

SELECT 		E.Enquiry_Key AS Item_Key, Display_Caption AS DisplayTerm
FROM 		Enquiry E
WHERE 		Search_Caption LIKE @SearchText + '%'
ORDER BY 	Display_Caption

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Enquiries_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Enquiries_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FindFirstConceptGroupForSubject') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_FindFirstConceptGroupForSubject]
GO

/*===========================================================================*\
  Description:	Returns the key of the first concept group for a subject area.

  Parameters:	@SubjectAreaKey	subject area key
		@ConceptGroupKey	OUTPUT

  Created:	July 2008

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FindFirstConceptGroupForSubject]
	@SubjectAreaKey char(16),
	@ConceptGroupKey char(16) OUTPUT
AS
	SET @ConceptGroupKey = (SELECT TOP 1 CG.Concept_Group_Key from Subject_Area SA
	INNER JOIN Domain D ON D.Subject_Area_Key = SA.Subject_Area_Key
	INNER JOIN Local_Domain LD ON LD.Domain_Key = D.Domain_Key
	INNER JOIN Concept_Group CG ON CG.Local_Domain_Key = LD.Local_Domain_Key
	WHERE SA.Subject_Area_Key = @SubjectAreaKey 
	ORDER BY SA.Item_Name, D.Item_Name, CG.Item_Name)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FindFirstConceptGroupForSubject') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FindFirstConceptGroupForSubject'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FindFirstConceptGroupForSubject TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FindFirstConceptGroupForSubject TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FindFirstConceptGroupForSubject TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_FindFirstConceptGroupForSubject TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FindFirstConceptGroupForSubject TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FindFirstConceptGroupForSubject TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_FormattedNameForNameKey_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_FormattedNameForNameKey_Get]
GO

/*===========================================================================*\
  Description:	Wrapper around the GetFormattedName function.
		The rule for individual name format is:
			Title + (Forename | Initials) + Surname

		The rule for organisation name format is
			Acronym + ', ' + Full_Name

		Null fields are omitted.

  Parameters:	@NameKey
		@FormattedName	Output

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FormattedNameForNameKey_Get]
	@NameKey char(16),
	@FormattedName varchar(200) OUTPUT 
AS
	IF @NameKey IS NULL
		SET @FormattedName = 'Unknown'
	ELSE
		SELECT @FormattedName = dbo.ufn_GetFormattedName(@NameKey)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FormattedNameForNameKey_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FormattedNameForNameKey_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FormattedNameForNameKey_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FormattedNameForNameKey_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FormattedNameForNameKey_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_FormattedNameForNameKey_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FormattedNameForNameKey_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FormattedNameForNameKey_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects 
       WHERE  Id = Object_Id(N'[dbo].[usp_InternalReferences_Select_ForSearch]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_InternalReferences_Select_ForSearch]
GO

/*===========================================================================*\
  Description:  Search procedure for Internal References

  Parameters:   @SearchText 

  Created:  November 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_InternalReferences_Select_ForSearch]
    @SearchText VARCHAR(100)
AS
    SET NOCOUNT ON

    SELECT      r.Source_Key                                AS  Item_Key,
                a.Author
                + ' - '
                + dbo.ufn_GetDateFromVagueDate(
                    r.Year_Vague_Date_Start,
                    r.Year_Vague_Date_End,
                    r.Year_Vague_Date_Type)
                + ', '
                + dbo.ufn_RtfToPlainText(r.Full_Reference)  AS  DisplayTerm,
                a.Author
                + ' - '
                + dbo.ufn_GetDateFromVagueDate(
                    r.Year_Vague_Date_Start,
                    r.Year_Vague_Date_End,
                    r.Year_Vague_Date_Type)
                + ', '
                + dbo.ufn_RtfToPlainText(r.Full_Reference)  AS  SearchTerm
    FROM        Reference                                   AS  r
    INNER JOIN  VW_Reference_Authors                        AS  a
    ON          a.Source_Key                                =   r.Source_Key
    WHERE       a.Author                                    LIKE @SearchText + '%'
    OR          dbo.ufn_RtfToPlainText(r.Title)             LIKE @SearchText + '%'
    OR          dbo.ufn_RtfToPlainText(r.Full_Reference)    LIKE @SearchText + '%'
    ORDER BY    a.Author,
                r.Year_Vague_Date_Start,
                dbo.ufn_RtfToPlainText(r.Full_Reference)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_InternalReferences_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_InternalReferences_Select_ForSearch'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
            GRANT EXECUTE ON dbo.usp_InternalReferences_Select_ForSearch TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_InternalReferences_Select_ForSearch TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_InternalReferences_Select_ForSearch TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_InternalReferences_Select_ForSearch TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_InternalReferences_Select_ForSearch TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_InternalReferences_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_JobGeneralActualDuration_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_JobGeneralActualDuration_Get]
GO

/*===========================================================================*\
  Description:	Returns the Actual Duration for the JobGeneral frame.

  Parameters: 	@Key	Conservation_Job_Key
	
  Created:	Setember 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_JobGeneralActualDuration_Get]

@Key CHAR(16),
@Duration VARCHAR(100) OUTPUT
	
AS

DECLARE @Seconds BIGINT
DECLARE @HighestMultiplierRelation CHAR(16)
DECLARE @DurationUnitKey CHAR(16)

SET NOCOUNT ON

	--Find the total number of seconds
	SELECT @Seconds=
 		SUM(CASE 
			WHEN MRTo.Meaning_Relation_Key IS NULL AND MRFrom.Meaning_Relation_Key IS NULL THEN
				CT.Duration -- recorded in seconds
			WHEN MRFrom.Meaning_Relation_Key IS NULL THEN
				CT.Duration * MRTo.Multiplicity
		ELSE
			CT.Duration / MRFrom.Multiplicity
		END)

	FROM Conservation_Task CT
	
	INNER JOIN Concept C on C.Concept_Key=CT.Duration_Unit_Concept_Key
	LEFT JOIN Meaning_Relation MRFrom ON MRFrom.From_Meaning_Key=C.Meaning_Key
		AND MRFrom.To_Meaning_Key='SYSTEM000000008Y'
	LEFT JOIN Meaning_Relation MRTo ON MRTo.To_Meaning_Key=C.Meaning_Key
		AND MRTo.From_Meaning_Key='SYSTEM000000008Y'
	
	WHERE CT.Conservation_Job_Key=@Key

	--Find the meaning key for the highest used unit
	SELECT TOP 1 
		@HighestMultiplierRelation=
		CASE WHEN MRFrom.Meaning_Relation_Key IS NULL THEN
			MRTo.Meaning_Relation_Key
		ELSE
			MRFrom.Meaning_Relation_Key
		END,
		@DurationUnitKey=CT.Duration_Unit_Concept_Key

	FROM Conservation_Task CT

	INNER JOIN Concept C on C.Concept_Key=CT.Duration_Unit_Concept_Key
	LEFT JOIN Meaning_Relation MRFrom ON MRFrom.From_Meaning_Key=C.Meaning_Key
		AND MRFrom.To_Meaning_Key='SYSTEM000000008Y'
	LEFT JOIN Meaning_Relation MRTo ON MRTo.To_Meaning_Key=C.Meaning_Key
		AND MRTo.From_Meaning_Key='SYSTEM000000008Y'

	WHERE CT.Conservation_Job_Key=@Key

	ORDER BY  	
		CASE WHEN MRFrom.Meaning_Relation_Key IS NULL THEN
		MRTo.Multiplicity
		ELSE
		1 / MRFrom.Multiplicity
		END DESC

	--Convert the total seconds back into the required unit.
	IF @HighestMultiplierRelation IS NULL
		--Result required in seconds
		SELECT CAST(@Seconds AS VARCHAR(100)) + ' ' + Plaintext AS Duration
	
		FROM VW_ConceptTerm WHERE Concept_Key='SYSTEM000000008Y'
		
	ELSE BEGIN
		--Convert to a different unit
		SELECT 
			@Duration = CAST(ROUND(
				CASE MR.From_Meaning_Key
					WHEN 'SYSTEM000000008Y' THEN @Seconds / MR.Multiplicity
				ELSE
					@Seconds * MR.Multiplicity
				END ,2)AS VARCHAR(100)) + ' ' + Plaintext
	
		FROM Meaning_Relation MR, VW_ConceptTerm CT
	
		WHERE MR.Meaning_Relation_Key=@HighestMultiplierRelation
			AND CT.Concept_Key=@DurationUnitKey
	END

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_JobGeneralActualDuration_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_JobGeneralActualDuration_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_JobGeneralActualDuration_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_JobGeneralActualDuration_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_JobGeneralActualDuration_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_JobGeneralActualDuration_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_JobGeneralActualDuration_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_JobGeneralActualDuration_Get TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Jobs_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Jobs_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_Jobs_Select_ForSearch] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SearchText VARCHAR(100)

AS
--
--  DESCRIPTION
--  Returns Conservation_Job_Key and DisplayTerm when search characters are entered.
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned.
--	@SessionID 		User's SessionID.
--	@SearchText 		Search text used to find collections.
--
--  AUTHOR:			Anthony Simpson, Dorset Software
--  CREATED:			2003-09-22

SET NOCOUNT ON

SELECT 	Conservation_Job_Key AS Item_Key, Display_Caption AS DisplayTerm
FROM 	Conservation_Job
WHERE 	((Domain_Mask & @UserDomainMask > 0) OR (Entered_Session_ID = @SessionID) 
			OR (Changed_Session_ID = @SessionID) OR (Domain_Mask = 0))
AND 	Search_Caption LIKE @SearchText + '%'
ORDER BY Display_Caption
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Jobs_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Jobs_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Jobs_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Jobs_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Jobs_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Jobs_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Jobs_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Jobs_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Loans_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Loans_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_Loans_Select_ForSearch] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SearchText VARCHAR(100)

AS
--
--  DESCRIPTION
--  Returns Movement_Key and Movement caption as search characters are entered.
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned.
--	@SessionID 		User's SessionID.
--	@SearchText 		Search text used to find collections.
--
--  AUTHOR:			Anthony Simpson, Dorset Software
--  CREATED:			2003-09-23

SET NOCOUNT ON

--DISTINCT removes duplicate records due to two direction types.
SELECT DISTINCT M.Movement_Key AS Item_Key, Display_Caption AS DisplayTerm
FROM 
	MOVEMENT M
		INNER JOIN
			MOVEMENT_DIRECTION MD
		ON M.Movement_Key = MD.Movement_Key 
			AND (M.Movement_Type = 2 OR M.Movement_Type = 3)
		LEFT JOIN 
			(MOVEMENT_COLLECTION_UNIT MCU 
			INNER JOIN 
				COLLECTION_UNIT CU 
			ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
				AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)) 
			)
		ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
		WHERE ((CU.Collection_Unit_Key IS NOT NULL) 
			OR ((MCU.Collection_Unit_Key IS NULL) AND ((M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID))))
			AND Search_Caption LIKE @SearchText + '%'

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Loans_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Loans_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Loans_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Loans_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Loans_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Loans_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Loans_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Loans_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementOfMaterial_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementOfMaterial_Get]
GO
/*===========================================================================*\
  Description: 	Gets a detail for the caption of the Materials to unknown 
		destinations tab page control.	
  Parameters:	@Key	Movement of Material table key.

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementOfMaterial_Get]
	@Key char(16),
	@Caption varchar(100) OUTPUT
AS
	SELECT @Caption = 
		'Material has been ' 	
		+ LOWER(dbo.ufn_GetMovementTypeName(M.Movement_Type))
		+ ' - ' 
		+ dbo.ufn_GetDateFromVagueDate(MOM.Vague_Date_Start, MOM.Vague_Date_End, MOM.Vague_Date_Type)
	FROM Movement_Of_Material MOM
	INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key = MOM.Movement_Direction_Key
	INNER JOIN Movement M ON M.Movement_Key = MD.Movement_Key
	WHERE MOM.Movement_Of_Material_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementOfMaterial_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementOfMaterial_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementOfMaterial_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementOfMaterial_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementOfMaterial_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MovementOfMaterial_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementOfMaterial_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementOfMaterial_Get TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Movements_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Movements_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_Movements_Select_ForSearch] 
@UserDomainMask BIGINT,
@SessionID CHAR(16),
@SearchText VARCHAR(100)

AS
--
--  DESCRIPTION
--  Returns Movement_Key and Movement caption as search characters are entered.
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned.
--	@SessionID 		User's SessionID.
--	@SearchText 		Search text used to find collections.
--
--  AUTHOR:			Anthony Simpson, Dorset Software
--  CREATED:			2003-09-23

SET NOCOUNT ON

	SELECT DISTINCT M.Movement_Key AS Item_Key, M.Display_Caption AS DisplayTerm
	FROM MOVEMENT M
	INNER JOIN MOVEMENT_DIRECTION MD ON M.Movement_Key = MD.Movement_Key 
					 AND (M.Movement_Type >= 4 
					 AND M.Movement_Type <= 9)
	LEFT JOIN 
					MOVEMENT_COLLECTION_UNIT MCU 
				ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
				LEFT JOIN 
					COLLECTION_UNIT CU 
				ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key	
	
	WHERE Search_Caption LIKE @SearchText + '%'
	AND ((CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
				OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID))
			
	ORDER BY M.Display_Caption
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movements_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movements_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Movement_MaterialSummary_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Movement_MaterialSummary_Get]
GO

/*===========================================================================*\
  Description:	Returns counts for the MaterialSummary section
		on FrameMovementGeneral.

  Parameters:	@Key		Movement key
		@Category	Whether we want the count for Collections (0),
				Specimens (1) or Storage (2) to be returned.

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Movement_MaterialSummary_Get]
	@Key char(16),
	@Category tinyint,
	@Count varchar(20) output
AS

SET NOCOUNT ON

	IF @Category = 0 	-- i.e. Collections count
	BEGIN
		SELECT @Count=CAST(Count(Distinct (MCU.Collection_Unit_Key)) AS VARCHAR(20))
		FROM Movement_Collection_Unit MCU
		INNER JOIN Collection U ON U.Collection_Unit_Key = MCU.Collection_Unit_Key
		INNER JOIN Movement_Direction MD on MD.Movement_Direction_Key=MCU.Movement_Direction_Key
		WHERE MD.Movement_Key=@Key

	END
	ELSE IF @Category = 1	-- i.e. Specimens count
	BEGIN
		SELECT @Count=CAST(Count(Distinct (MCU.Collection_Unit_Key)) AS VARCHAR(20))
		FROM Movement_Collection_Unit MCU
		INNER JOIN Specimen_Unit U ON U.Collection_Unit_Key = MCU.Collection_Unit_Key
		INNER JOIN Movement_Direction MD on MD.Movement_Direction_Key=MCU.Movement_Direction_Key
		WHERE MD.Movement_Key=@Key
	END
	ELSE IF @Category = 2	-- i.e. Store count
	BEGIN
		SELECT @Count=CAST(Count(Distinct (S.Collection_Unit_Key)) AS VARCHAR(20))
		FROM Movement_Collection_Unit MCU
		INNER JOIN Specimen_Unit U ON U.Collection_Unit_Key=MCU.Collection_Unit_Key
		INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key=U.Collection_Unit_Key
		INNER JOIN Store S ON S.Collection_Unit_Key=CU.Current_Container_Collection_Unit_Key
		INNER JOIN Movement_Direction MD on MD.Movement_Direction_Key=MCU.Movement_Direction_Key
		WHERE MD.Movement_Key=@Key
	END

SET NOCOUNT OFF

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movement_MaterialSummary_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movement_MaterialSummary_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movement_MaterialSummary_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movement_MaterialSummary_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movement_MaterialSummary_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Movement_MaterialSummary_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movement_MaterialSummary_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movement_MaterialSummary_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Movement_Status_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Movement_Status_Get]
GO

/*===========================================================================*\
  Description:	Returns status for FrameMovementGeneral.

  Parameters:	@Key		Movement key
		@Status		Returns the status of the Movement. This can
				be Completed, Incomplete or Not Started.

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Movement_Status_Get]
	@Key char(16),
	@Status varchar(20) output
AS

SET NOCOUNT ON

	DECLARE @TotalComplete int
	DECLARE @TotalIncomplete int
	
	/*==============================================*\
  	   Count how many have been completed.
	\*==============================================*/
	SELECT @TotalComplete = Count(*)
	FROM Movement_Of_Ownership M
	INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key=M.Movement_Direction_Key
	WHERE MD.Movement_Key=@Key
	AND Completed=1
	
	SELECT @TotalComplete = @TotalComplete + Count(*)
	FROM Movement_Of_Material M
	INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key=M.Movement_Direction_Key
	WHERE MD.Movement_Key=@Key
	AND Completed=1

	/*==============================================*\
  	   Count how many have not been completed.
	\*==============================================*/
	SELECT @TotalIncomplete = Count(*)
	FROM Movement_Of_Ownership M
	INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key=M.Movement_Direction_Key
	WHERE MD.Movement_Key=@Key
	AND Completed=0
	
	SELECT @TotalIncomplete = @TotalIncomplete + Count(*)
	FROM Movement_Of_Material M
	INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key=M.Movement_Direction_Key
	WHERE MD.Movement_Key=@Key
	AND Completed=0

	/*===============================*\
  	   Determine the status.
	\*===============================*/
	IF @TotalIncomplete = 0 AND @TotalComplete <> 0
		SET @Status = 'Completed'
	ELSE IF @TotalIncomplete <> 0 AND @TotalComplete = 0
		SET @Status = 'Not started'
	ELSE
		SET @Status = 'Incomplete'

SET NOCOUNT OFF

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movement_Status_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movement_Status_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movement_Status_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movement_Status_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movement_Status_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Movement_Status_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movement_Status_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movement_Status_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Occurrences_Select_ForSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Occurrences_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of Occurrences

  Parameters:	@SearchText

  Created:	November 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Occurrences_Select_ForSearch]
	@SearchText varchar(150)
AS

SET NOCOUNT ON

	SELECT DISTINCT O.Occurrence_Key AS [Item_Key],
			CT.Item_Name + ' - ' +
			dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) +
			' - ' + 
			CASE 
				WHEN LN.Item_Name IS NULL THEN
					CASE WHEN S.Spatial_Ref IS NULL THEN '' ELSE S.Spatial_Ref END
				ELSE LN.Item_Name + ' (' + 
					CASE 
						WHEN S.Spatial_Ref IS NULL THEN L.Spatial_Ref
						ELSE S.Spatial_Ref END +
					')'
			END
			AS SearchTerm,
			CT.Item_Name + ' - ' +
			dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) +
			' - ' + 
			CASE 
				WHEN LN.Item_Name IS NULL THEN
					CASE WHEN S.Spatial_Ref IS NULL THEN '' ELSE S.Spatial_Ref END
				ELSE LN.Item_Name + ' (' + 
					CASE 
						WHEN S.Spatial_Ref IS NULL THEN L.Spatial_Ref
						ELSE S.Spatial_Ref END +
					')'
			END
			AS DisplayTerm
	FROM		Occurrence O
	INNER JOIN	Determination D ON O.Occurrence_Key = D.Occurrence_Key 
	INNER JOIN	vw_ConceptTermPreferred CT ON D.Concept_Key = CT.Concept_Key
	INNER JOIN 	Sample S ON S.Sample_Key = O.Sample_Key
	LEFT JOIN	Location L ON L.Location_Key = S.Location_Key 
	LEFT JOIN	Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
	WHERE		CT.PlainText LIKE @SearchText + '%'
	ORDER BY	SearchTerm

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Occurrences_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Occurrences_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

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
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

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

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Processes_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Processes_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Searches for Processes from one concept group.

  Parameters:	@SearchText
		@SearchKey   Concept_Group_Key

  Created:	November 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Processes_Select_ForSearch] 
@SearchText varchar(100),
@SearchKey char(16)

AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	SELECT 		Concept_Key AS Item_Key, 
			Item_Name AS DisplayTerm, 
			Item_Name AS SearchTerm
			
	FROM 		VW_ConceptTerm
	WHERE 		Concept_Group_Key = @SearchKey
	AND 		Item_Name LIKE @SearchText + '%'
	ORDER BY 	Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Processes_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Processes_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Processes_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Processes_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Processes_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Processes_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Processes_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Processes_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenDate_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenDate_Get]
GO

/*===========================================================================*\
  Description:	Returns the value of Date from the Sample table.

  Parameters:	@Key			Collection unit key
		@ShortDateFormat	Required Date Format
		@Date			OUTPUT

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenDate_Get]
	@Key char(16),
	@ShortDateFormat varchar(12),
	@Date varchar(50) OUTPUT
AS

	IF EXISTS(SELECT * FROM Specimen_Field_Data WHERE Collection_Unit_Key = @Key AND Gathering_Event = 1)
		SELECT		@Date = dbo.ufn_GetDateFromVagueDate
					(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type)
	
		FROM		Specimen_Field_Data AS SFD
	
		INNER JOIN	Occurrence As O ON O.Occurrence_Key = SFD.Occurrence_Key
		INNER JOIN	Sample AS S ON S.Sample_Key = O.Sample_Key

		WHERE		SFD.Collection_Unit_Key = @Key

	ELSE
		SET @Date = 'Unknown'

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenDate_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenDate_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenDate_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenDate_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenDate_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenDate_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenDate_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenDate_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenLocation_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenLocation_Get]
GO

/*===========================================================================*\
  Description:	Returns the value of Item_Name from the Domain table.

  Parameters:	@Key	Collection unit key
		@Name	OUTPUT

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenLocation_Get]
	@Key char(16),
	@Name varchar(100) OUTPUT
AS

	IF EXISTS(SELECT * FROM Specimen_Field_Data WHERE Collection_Unit_Key = @Key AND Gathering_Event = 1)
		SELECT		@Name = LN.Item_Name
	
		FROM		Specimen_Field_Data AS SFD
	
		LEFT JOIN	Taxon_Occurrence AS XO ON XO.Taxon_Occurrence_Key = SFD.Taxon_Occurrence_Key
		LEFT JOIN	Occurrence AS O ON O.Occurrence_Key = SFD.Occurrence_Key
		INNER JOIN	[Sample] AS S ON (S.Sample_Key = O.Sample_Key OR S.Sample_Key = XO.Sample_Key)
		INNER JOIN	Location_Name AS LN ON LN.Location_Key = S.Location_Key

		WHERE		SFD.Collection_Unit_Key = @Key AND LN.Preferred = 1

	ELSE
		SET @Name = 'Unknown'

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenLocation_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenLocation_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenLocation_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenLocation_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenLocation_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenLocation_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenLocation_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenLocation_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Specimens_Select_ForSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns Collection_Unit_Key and DisplayTerm when search characters 
		are entered. The Specimen_Unit table does not have a Display_Caption 
		or Search_Caption field, so the caption must be constructed through 
		joins to other tables.

  Parameters:	@UserDomainMask		User's Domain Mask restricting which records may be returned.
		@SessionID 		User's SessionID.
		@SearchText 		Search text used to find collections.

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearch] 
	@UserDomainMask int,
	@SessionID char(16),
	@ShowCommonNames BIT,
	@SearchText varchar(100)
AS

SET NOCOUNT ON


--Use temp table to build output, so silly data errors such as duplicate accessions
-- don't duplicate in the list
DECLARE @SpecimensSearch TABLE
(
	[Item_Key] [char] (16)				COLLATE database_default NULL,
	[DisplayTerm] [nvarchar] (150)		COLLATE database_default NULL,
	[SearchTerm] [nvarchar] (150)		COLLATE database_default NULL,
	[Life_Sciences] [bit] NULL
)

--Find all specimens with a determination match
INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, SearchTerm, DisplayTerm) 
SELECT DISTINCT 
	SU.Collection_Unit_Key				COLLATE database_default, 
	SU.Life_Sciences,
	CASE Su.Life_Sciences 
		WHEN 0 THEN TSearch.Plaintext	COLLATE database_default
		ELSE ITN.Actual_Name			COLLATE database_default
	END AS SearchTerm,
	CASE Su.Life_Sciences 
		WHEN 0 THEN TSearch.Item_Name	COLLATE database_default 
		ELSE CASE ITN.Actual_Name_Italic
			WHEN 1 THEN '<i>' + ITN.Actual_Name + '</i>' COLLATE database_default
			ELSE ITN.Actual_Name		COLLATE database_default
		END
	END AS DisplayTerm
	
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
LEFT JOIN VW_SpecimenDetsEarth SDE ON SU.Collection_Unit_Key = SDE.Collection_Unit_Key
LEFT JOIN Concept C ON SDE.Concept_Key = C.Concept_Key
LEFT JOIN Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
LEFT JOIN Term TSearch ON TSearch.Term_Key=CSearch.Term_Key
LEFT JOIN VW_SpecimenDetsLife SDL ON SU.Collection_Unit_Key = SDL.Collection_Unit_Key
LEFT JOIN Index_Taxon_Synonym ITS ON ITS.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key
LEFT JOIN INDEX_TAXON_NAME ITN	ON ITS.Synonym_List_Item_Key = ITN.Taxon_List_Item_Key
WHERE 
	(TSearch.Plaintext LIKE @SearchText + '%' AND SU.Life_Sciences=0) 
	OR 
	(ITN.Actual_Name LIKE @SearchText + '%' AND SU.Life_Sciences=1)

-- Update the number in case there are 2 registrations for a specimen, so we don't duplicate
-- the rows in the output results.
UPDATE @SpecimensSearch
SET 
		SearchTerm = SearchTerm + ' - ' + CUN.Number,
		DisplayTerm = DisplayTerm + ' - ' + CUN.Number
FROM @SpecimensSearch SU
INNER JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 	AND CUN.Preferred = 1

-- Select table and sort appropriately
SELECT * from @SpecimensSearch
ORDER BY SearchTerm

GO




/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByAnyNumber]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByAnyNumber]
GO

/*===========================================================================*\
  Description:
	Returns Specimens data based on the search parameter for any Number.

  Parameters:
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SearchText		Text to be searched on
	@SortOrderIndex		Index determining Sort Order

  Created:
	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByAnyNumber] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ShowCommonNames BIT,
	@SearchText VARCHAR(30),
	@SortOrderIndex TINYINT
AS
	SET NOCOUNT ON
	-- Set of options for better use of vw_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	--Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list
	DECLARE @SpecimensSearch TABLE
	(
		[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Life_Sciences] [bit] NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, CUN.Number
	FROM 	Specimen_Unit SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	   		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
			AND CUN.Number LIKE @SearchText + '%'

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				Preferred_Name,
				Preferred_Name_Italic,
				Common_Name,
				Common_Name_Italic,
				null,
				@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * from @SpecimensSearch
		ORDER BY Det_Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * from @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByAnyNumber') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByAnyNumber'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyNumber TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyNumber TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyNumber TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyNumber TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyNumber TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyNumber TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimen_GetFinderData]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimen_GetFinderData]
GO
    
/*===========================================================================*\
  Description:	Gets all the data needed to add dropped specimens to one of the specimen finder grids
  Parameters:	 

  Created:	October 2007

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimen_GetFinderData]
  @CollectionUnitKey  Char(16),
  @UserDomainMask Int,
  @ShowCommonNames Bit,
  @SessionID      Char(16)
 AS

 SELECT DISTINCT
 SpecUnit.Collection_Unit_Key AS Item_Key, 
 CASE 
 WHEN C.Concept_Key IS NOT NULL THEN C.Concept_Key 
 WHEN ITN.Taxon_List_Item_Key IS NOT NULL THEN ITN.Taxon_List_Item_Key 
 END AS Det_Item_Key, 
 CASE 
 WHEN SpecUnit.Life_Sciences = 0 THEN TP.Item_Name 
 ELSE dbo.ufn_GetFormattedTaxonNameByParams(ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name, 
 ITN.Common_Name_Italic, ITN.Authority, @ShowCommonNames) END AS Item_Name, 
 SpecUnit.Life_Sciences, 
 dbo.ufn_GetPrefNumber(SpecUnit.Collection_Unit_Key) as Number, 
 CU.Current_Location_Code, 
 SpecUnit.Specimen_Type_Concept_Key, 
 STTP.Item_Name AS Specimen_Type,
 C.List_Code, 
 DM.Item_Name as Domain_Name 
 FROM SPECIMEN_UNIT SpecUnit 
 INNER JOIN COLLECTION_UNIT CU ON SpecUnit.Collection_Unit_Key = CU.Collection_Unit_Key 
 AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0) OR (CU.Entered_Session_ID = @SessionID) OR (CU.Changed_Session_ID = @SessionID)) 
 LEFT JOIN COLLECTION_UNIT_NUMBER CUN ON SpecUnit.Collection_Unit_key = CUN.Collection_Unit_Key 
 AND CUN.Preferred = 1 
 LEFT JOIN DETERMINATION D ON SpecUnit.Preferred_Determination_Key = D.Determination_Key 
 LEFT JOIN Concept C ON D.Concept_Key = C.Concept_Key 
 LEFT JOIN Concept CP ON CP.Meaning_Key = C.Meaning_Key AND CP.Concept_Group_Key = C.Concept_Group_Key AND CP.List_Preferred = 1 
 LEFT JOIN Term TP ON CP.Term_Key = TP.Term_Key 
 LEFT JOIN TAXON_DETERMINATION TD ON SpecUnit.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key 
 LEFT JOIN INDEX_TAXON_NAME ITN ON TD.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key 
 LEFT JOIN Concept_Group CG ON C.Concept_Group_Key = CG.Concept_Group_Key 
 LEFT JOIN Local_Domain LD ON CG.Local_Domain_Key = LD.Local_Domain_Key 
 LEFT JOIN Domain DM ON LD.Domain_Key = DM.Domain_Key
 INNER JOIN Concept STC ON SpecUnit.Specimen_Type_Concept_Key = STC.Concept_Key 
 LEFT JOIN Concept STCP ON STCP.Meaning_Key = STC.Meaning_Key 
 AND STCP.Concept_Group_Key = STC.Concept_Group_Key AND STCP.List_Preferred = 1 
 LEFT JOIN Term STTP ON STCP.Term_Key = STTP.Term_Key

WHERE SpecUnit.Collection_Unit_Key = @CollectionUnitKey
GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_GetFinderData') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimen_GetFinderData'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimen_GetFinderData TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimen_GetFinderData TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimen_GetFinderData TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_GetFinderData TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimen_GetFinderData TO [Dev - JNCC SQL]
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
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimen_Insert]
	--for specimen_unit
	@Key char(16) OUTPUT,
	@ExistingCollectionUnitKey char(16) = NULL,
	@ParentCollectionCollectionUnitKey char(16),
	@SpecimenTypeConceptKey char(16),
	@Confidential bit,
	@Dangerous bit,
	@LifeSciences bit,
	@Checked bit,
	--for Collection_Unit
	@CurrentContainerCollectionUnitKey char(16),
	@CurrentLocationCode varchar(30),
	@UsualContainerCollectionUnitKey char(16),
	@UsualLocationCode varchar(30),
	-- Both
	@SessionID char(16)
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
			Life_Sciences,
			Entered_Session_ID,
			Checked)
		 VALUES (
			@Key, 
			@ParentCollectionCollectionUnitKey,
			@SpecimenTypeConceptKey,
			IsNull(@Confidential, 0),
			IsNull(@Dangerous, 0),
			@LifeSciences,
			@SessionID,
			@Checked 
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
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

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
				SU.Checked

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
				SU.Confidential,
				SU.Parent_Collection_Collection_Unit_Key AS Parent_Unit_Key,
				Coll.Item_Name AS ParentCollectionCollectionUnitName,
				CU.Current_Container_Collection_Unit_Key,
				CU.Usual_Container_Collection_Unit_Key,
				SCU.Item_Name + ISNULL(' - ' + CSC.Current_Location_Code, ISNULL(' - ' + CSC.Usual_Location_Code, '')) AS Current_Location_Name,
				SUS.Item_Name + ISNULL(' - ' + CSU.Current_Location_Code, ISNULL(' - ' + CSU.Usual_Location_Code, '')) AS Usual_Location_Name,
				T.Item_Name,
				Specimen_Type_Concept_Key,
				CTType.PlainText AS Type,
				SU.Timestamp AS SUTimeStamp,
				CU.Timestamp AS CUTimeStamp,
				SU.Checked
	
		FROM		Collection_Unit AS CU
		INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key
		INNER JOIN	Collection AS Coll On Coll.Collection_Unit_Key = SU.Parent_Collection_Collection_Unit_Key
		LEFT JOIN	Determination AS D ON D.Determination_Key = SU.Preferred_Determination_Key
		LEFT JOIN	Concept AS C ON C.Concept_Key = D.Concept_Key
		LEFT JOIN	Term AS T ON T.Term_Key = C.Term_Key
	
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
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimen_Update]
	--for specimen_unit
	@Key char(16), 
	@ParentCollectionCollectionUnitKey char(16),
	@SpecimenTypeConceptKey char(16),
	@Confidential bit,
	@Dangerous bit,
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
	@SessionID char(16)
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
			Life_Sciences = @LifeSciences,
			Changed_Session_ID = @SessionID,
			Checked = @Checked 
		WHERE	Collection_Unit_Key = @Key
		AND	(@SUTimestamp =[Timestamp])
	
		IF @@Error <> 0 GOTO RollbackAndExit
	
		UPDATE	Collection_Unit
		SET	Current_Container_Collection_Unit_Key = @CurrentContainerCollectionUnitKey,
			Usual_Container_Collection_Unit_Key = @UsualContainerCollectionUnitKey,
			Current_Location_Code = @CurrentLocationCode,
			Usual_Location_Code = @UsualLocationCode,
			Changed_Session_ID = @SessionID
		WHERE	Collection_Unit_Key = @Key
		AND	(@CUTimestamp = Timestamp)
	
		IF @@Error <> 0 GOTO RollbackAndExit
	
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
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_StoreDetailsMemo_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_StoreDetailsMemo_Get]
GO
 
/*===========================================================================*\
  Description:	Returns a the details memo content for the Store general tab

  Parameters:	@Key	Collection_Unit key
							@Output - output.  Note this is limited to 8000 characters.

  Created:	Oct 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_StoreDetailsMemo_Get]
	@Key char(16),
	@Output varchar(8000) OUTPUT
AS

SET NOCOUNT ON

--Async query to get details memo content
DECLARE @Value VARCHAR(500) 

DECLARE data_cursor CURSOR FORWARD_ONLY FOR 
	SELECT CT.Plaintext COLLATE SQL_Latin1_General_CP1_CI_AS + ' ' + D.Lower_Value AS Value
	FROM Collection_Unit_Data D
	INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=D.Parameter_Concept_Key
	WHERE D.Collection_Unit_Key=@Key
	AND D.Is_Descriptor=1

OPEN data_cursor

FETCH NEXT FROM data_cursor INTO @Value

SET @Output = ''

WHILE @@Fetch_Status=0
BEGIN
  --New line if required
	IF @Output <> '' 
		SET @Output=@Output+CHAR(13)+CHAR(10)
  SET @Output=@Output+@Value	
	FETCH NEXT FROM data_cursor INTO @Value
END

CLOSE data_cursor
DEALLOCATE data_cursor

DECLARE data_cursor CURSOR FORWARD_ONLY FOR 
	SELECT 
		CASE WHEN D.Upper_Value IS NULL THEN
			D.Lower_Value 
		ELSE
			D.Lower_Value + ' - ' + D.Upper_Value
		END +
    CASE WHEN CTU.Plaintext IS NULL THEN 
			''
		ELSE
			' ' + CTU.Plaintext COLLATE SQL_Latin1_General_CP1_CI_AS
		END +
	  ' ' + CTP.Plaintext COLLATE SQL_Latin1_General_CP1_CI_AS +
		' (' + D.Applies_To + ')' AS Value
	FROM Collection_Unit_Data D
	LEFT JOIN VW_ConceptTerm CTU ON CTU.Concept_Key=D.Unit_Concept_Key
	INNER JOIN VW_ConceptTerm CTP ON CTP.Concept_Key=D.Parameter_Concept_Key
	WHERE D.Collection_Unit_Key=@Key
	AND D.Is_Descriptor=0

OPEN data_cursor

FETCH NEXT FROM data_cursor INTO @Value

WHILE @@Fetch_Status=0
BEGIN
  --New line if required
	IF @Output <> '' 
		SET @Output=@Output+CHAR(13)+CHAR(10)
  SET @Output=@Output+@Value	
	FETCH NEXT FROM data_cursor INTO @Value
END

CLOSE data_cursor
DEALLOCATE data_cursor
SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreDetailsMemo_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreDetailsMemo_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreDetailsMemo_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreDetailsMemo_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreDetailsMemo_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreDetailsMemo_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreDetailsMemo_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_StoreDetailsMemo_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Store_Select_ForNameSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Store_Select_ForNameSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of stores matching a search string.

  Parameters:	@SearchText

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Store_Select_ForNameSearch]
	@SearchText varchar(100),
	@UserDomainMask INT,
	@SessionID CHAR(16)
AS
	SELECT		S.Collection_Unit_Key AS Item_Key, 
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS DisplayTerm, 
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS SearchTerm
	FROM		Store S
	INNER JOIN 	COLLECTION_UNIT CU ON S.Collection_Unit_Key = CU.Collection_Unit_Key
     				AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE		Item_Name LIKE @SearchText + '%'
	ORDER BY 	Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_Select_ForNameSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Store_Select_ForNameSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Store_Select_ForNameSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Store_Select_ForNameSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Store_Select_ForNameSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Store_Select_ForNameSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Store_Select_ForNameSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Store_Select_ForNameSearch TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Tasks_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Tasks_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_Tasks_Select_ForSearch] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SearchText VARCHAR(100)

AS
--
--  DESCRIPTION
--  Returns Conservation_Task_Key and DisplayTerm when search characters are entered.
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned.
--	@SessionID 		User's SessionID.
--	@SearchText 		Search text used to find collections.
--
--  AUTHOR:			Anthony Simpson, Dorset Software
--  CREATED:			2003-09-24

SET NOCOUNT ON

SELECT 	CT.Conservation_Task_Key AS Item_Key, CT.Display_Caption AS DisplayTerm
FROM 	Conservation_Task CT
		INNER JOIN
			Conservation_Check CC
		ON CT.Conservation_Check_Key = CC.Conservation_Check_Key 
			AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
				OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
WHERE 	CT.Search_Caption LIKE @SearchText + '%'
ORDER BY CT.Display_Caption

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Tasks_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Tasks_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrences_Select_ForSearch') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_TaxonOccurrences_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of Taxon Occurrences.

  Parameters:	@SearchText

  Created:	November 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonOccurrences_Select_ForSearch]
	@SearchText varchar(150)
AS

SET NOCOUNT ON

	SELECT DISTINCT XO.Taxon_Occurrence_Key AS [Item_Key],
			ITN.Actual_Name + ' - ' +
			dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) +
			' - ' + 
			CASE 
				WHEN LN.Item_Name IS NULL THEN
					CASE WHEN S.Spatial_Ref IS NULL THEN '' ELSE S.Spatial_Ref END
				ELSE LN.Item_Name + ' (' + 
					CASE 
						WHEN S.Spatial_Ref IS NULL THEN L.Spatial_Ref
						ELSE S.Spatial_Ref END +
					')'
			END
			AS SearchTerm,
			CASE ITN.Actual_Name_Italic
				WHEN 1 THEN '<i>' + ITN.Actual_Name + '</i>'
				ELSE ITN.Actual_Name 
			END + ' - ' +
			dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) +
			' - ' + 
			CASE 
				WHEN LN.Item_Name IS NULL THEN
					CASE WHEN S.Spatial_Ref IS NULL THEN '' ELSE S.Spatial_Ref END
				ELSE LN.Item_Name + ' (' + 
					CASE 
						WHEN S.Spatial_Ref IS NULL THEN L.Spatial_Ref
						ELSE S.Spatial_Ref END +
					')'
			END
			AS DisplayTerm
	FROM		Taxon_Occurrence XO
	INNER JOIN	Taxon_Determination TD ON XO.Taxon_Occurrence_Key = TD.Taxon_Occurrence_Key AND TD.Preferred = 1
	INNER JOIN	Taxon_Dictionary_Concept_Mapping TDCM ON TDCM.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
	INNER JOIN	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
	INNER JOIN 	Sample S ON S.Sample_Key = XO.Sample_Key
	LEFT JOIN	Location L ON L.Location_Key = S.Location_Key 
	LEFT JOIN	Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
	WHERE		ITN.Actual_Name LIKE @SearchText + '%'
	ORDER BY	SearchTerm

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrences_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonOccurrences_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSearch TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonRank_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonRank_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon ranks corresponding to concept ranks from the
				specified concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 3 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonRank_ImportConceptGroup]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key		CHAR(16),
				@concept_rank_key		CHAR(16),
				@sequence				SMALLINT,
				@short_name				VARCHAR(20),
				@long_name				VARCHAR(100),
				@entered_by				CHAR(16),
				@entry_date				SMALLDATETIME,
				@changed_by				CHAR(16),
				@changed_date			SMALLDATETIME,
				@system					BIT,
				@taxon_rank_key			CHAR(16),
				@DoUpdate				BIT

	/* determine parameters of job */
	SELECT      @concept_group_key			=	j.Concept_Group_Key
	FROM		Import_Export_Job			AS	j
	WHERE		j.Import_Export_Job_ID		=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting concept ranks'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		ranks	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				cr.Concept_Rank_Key,
				cr.Sort_Order,
				cr.Abbreviation,
				cr.Item_Name,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				cr.System_Supplied_Data
	FROM		Concept				   			AS	c
	INNER JOIN	Concept_Rank					AS	cr
	ON			cr.Concept_Rank_Key				=	c.Concept_Rank_Key
	INNER JOIN	Session							AS	es
	ON			es.Session_ID					=	cr.Entered_Session_ID
	LEFT JOIN	Session							AS	cs
	ON			cs.Session_ID					=	cr.Changed_Session_ID
	WHERE		c.Concept_Group_Key				=	@concept_group_key

	OPEN		ranks

	WHILE 1 = 1
	BEGIN
		FETCH		ranks
		INTO		@concept_rank_key,
					@sequence,
					@short_name,
					@long_name,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@taxon_rank_key							=	Taxon_Rank_Key
		FROM		Taxon_Dictionary_Concept_Rank_Mapping
		WHERE		Concept_Rank_Key						=	@concept_rank_key

		IF @@ROWCOUNT = 0
		BEGIN
			/* No existing mapping, but search for a suitable taxon rank to map to */
			SELECT		@taxon_rank_key						=	TR.Taxon_Rank_Key
			FROM		Taxon_Rank TR
			WHERE		TR.Long_Name=@Long_Name
			AND 		(TR.Sequence=@Sequence or @Sequence IS NULL)
			
			IF @@ROWCOUNT = 0
				SET @DoUpdate=0
			ELSE BEGIN
				SET @DoUpdate=1
				/* If concept rank sequence is null, don't overwrite the good one in the
				dictionary.  Instead, copy the dictionary one back */
				if @Sequence IS NULL BEGIN
					SELECT @Sequence=Sequence
					FROM Taxon_Rank
					WHERE Taxon_Rank_Key=@taxon_rank_key

					UPDATE Concept_Rank
					SET Sort_Order=@Sequence
					WHERE Concept_Rank_Key=@concept_rank_key
				END	
			END

		END
		ELSE
			SET @DoUpdate=1

		IF @DoUpdate=1
		BEGIN
			/* update taxon rank */
			UPDATE		TAXON_RANK
			SET			SEQUENCE				=	@sequence,
						SHORT_NAME				=	@short_name,
						LONG_NAME				=	@long_name,
						ENTERED_BY				=	@entered_by,
						ENTRY_DATE				=	@entry_date,
						CHANGED_BY				=	@changed_by,
						CHANGED_DATE			=	@changed_date,
						SYSTEM_SUPPLIED_DATA	=	@system
			WHERE		TAXON_RANK_KEY			=	@taxon_rank_key

			/* ensure mapping exists */
			IF NOT EXISTS(SELECT 1 FROM Taxon_Dictionary_Concept_Rank_Mapping 
					WHERE Taxon_Rank_Key=@taxon_rank_key
					AND Concept_Rank_Key=@concept_rank_key)
				INSERT		Taxon_Dictionary_Concept_Rank_Mapping (
						Taxon_Rank_Key,
						Concept_Rank_Key)
				VALUES		(@taxon_rank_key,
						@concept_rank_key)
		   IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create taxon rank */
			EXECUTE		spNextKey		'TAXON_RANK',
										@taxon_rank_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_RANK (
						TAXON_RANK_KEY,
						SEQUENCE,
						SHORT_NAME,
						LONG_NAME,
						LIST_FONT_ITALIC,
						DISPLAY_IN_DETAILS,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_rank_key,
						@sequence,
						@short_name,
						@long_name,
						0,
						0,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* ensure mapping exists */
			IF NOT EXISTS(SELECT 1 FROM Taxon_Dictionary_Concept_Rank_Mapping WHERE Taxon_Rank_Key=@taxon_rank_key)
				INSERT		Taxon_Dictionary_Concept_Rank_Mapping (
						Taxon_Rank_Key,
						Concept_Rank_Key)
				VALUES		(@taxon_rank_key,
						@concept_rank_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail

		COMMIT TRANSACTION
	END

	CLOSE		ranks
	DEALLOCATE	ranks
	RETURN

fail_from_cursor:
	CLOSE		ranks
	DEALLOCATE	ranks

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonRank_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonRank_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonRank_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonRank_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonRank_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonRank_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermHTMLDetails_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermHTMLDetails_Select]
GO



/*===========================================================================*\
  Description:	Returns multiple recordsets suitable for populating the HTML
			details of a concept, excluding the name.  
			The following recordsets are returned:
				Designations
				Facts
				Sources
				Hyperlinks

  Parameters:	@Key	Collection key

  Created:	Dec 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermHTMLDetails_Select]
	@ConceptKey char(16)
AS

SET NOCOUNT ON

/*===========================================================================*\	
	Recordset for each designation
\*===========================================================================*/
	SELECT 
		CT.Item_Name, 
  	dbo.ufn_GetDateFromVagueDate(
					CD.From_Vague_Date_Start, 
					CD.From_Vague_Date_End, 
					CD.From_Vague_Date_Type) AS DateFrom,
		dbo.ufn_GetDateFromVagueDate(
					CD.To_Vague_Date_Start, 
					CD.To_Vague_Date_End, 
					CD.To_Vague_Date_Type) AS DateTo,
		MGeo.[Text] AS Geographic_Context,
		MC.[Text] AS Constraints
	FROM Concept_Designation CD
	INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=CD.Designation_Type_Concept_Key
	LEFT JOIN Metadata MGeo ON MGeo.Record_Key=CD.Concept_Designation_Key AND MGeo.Metadata_Type_Key='SYSTEM0000000001'
	LEFT JOIN Metadata MC ON MC.Record_Key=CD.Concept_Designation_Key AND MC.Metadata_Type_Key='SYSTEM0000000002'
	WHERE CD.Concept_Key=@ConceptKey
	

/*===========================================================================*\	
	Temporary table to hold the list of term version keys that we might need
	facts for.  These are all the related terms versions which at least have 
	some overlap with the current term version.
\*===========================================================================*/
	CREATE TABLE #TermVersionKeys (Term_Version_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS)

	INSERT INTO #TermVersionKeys (Term_Version_Key)
		SELECT Term_Version_Key FROM Concept WHERE Concept_Key=@ConceptKey
		
	WHILE @@RowCount>0
		INSERT INTO #TermVersionKeys (Term_Version_Key) 
		SELECT TVR.To_Term_Version_Key 
		FROM Term_Version_Relation TVR
		INNER JOIN #TermVersionKeys TVK ON TVK.Term_Version_Key=TVR.From_Term_Version_Key
		INNER JOIN Thesaurus_Relation_Type TRT on TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
		INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
				AND SR.Forward_Equivalence_Possible=1
		LEFT JOIN #TermVersionKeys TVK2 ON TVK2.Term_Version_Key=TVR.To_Term_Version_Key
		WHERE TVK2.Term_Version_Key IS NULL	

	WHILE @@RowCount>0
		INSERT INTO #TermVersionKeys (Term_Version_Key) 
		SELECT TVR.From_Term_Version_Key 
		FROM Term_Version_Relation TVR
		INNER JOIN #TermVersionKeys TVK ON TVK.Term_Version_Key=TVR.To_Term_Version_Key
		INNER JOIN Thesaurus_Relation_Type TRT on TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
		INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
				AND SR.Reverse_Equivalence_Possible=1
		LEFT JOIN #TermVersionKeys TVK2 ON TVK2.Term_Version_Key=TVR.From_Term_Version_Key
		WHERE TVK2.Term_Version_Key IS NULL


/*===========================================================================*\	
	Temporary table to hold the list of lineage concept keys that we need to 
	look at for inherited facts
\*===========================================================================*/
	CREATE TABLE #InheritedConcepts (Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS)

	DECLARE @LineageID integer
	DECLARE @Lineage varchar(2000)
	DECLARE @CharPos integer
	DECLARE @ConceptGroupKey char(16)

	--First find all the parent lineages
  DECLARE Lineages_Cursor CURSOR LOCAL FORWARD_ONLY FOR
    SELECT Lineage_ID FROM Concept_Lineage WHERE Concept_Key=@ConceptKey

	OPEN Lineages_Cursor

  FETCH NEXT FROM Lineages_Cursor INTO @LineageID
  
  WHILE @@FETCH_STATUS=0
	BEGIN

	  --select the lineage and concept group
		SELECT @Lineage = CL.Lineage, @ConceptGroupKey = C.Concept_Group_Key
		FROM Concept_Lineage CL
		INNER JOIN Concept C on C.Concept_Key=CL.Concept_Key
		WHERE CL.Concept_Key=@ConceptKey
		AND CL.Lineage_ID=@LineageID
		
		SET @CharPos=1
		
		--Find each ancestor, start at top of tree and work down
		WHILE @CharPos<LEN(@Lineage)
		BEGIN
		  IF SUBSTRING(@Lineage, @CharPos, 1)='\'
			  INSERT INTO #InheritedConcepts
			    SELECT DISTINCT C.Concept_Key
					FROM Concept C
		      INNER JOIN Concept_Lineage CL ON CL.Concept_Key=C.Concept_Key
				  WHERE C.Concept_Group_Key=@ConceptGroupKey
			    AND CL.Lineage=Left(@Lineage, @CharPos-1)
		  SET @CharPos=@CharPos+1
		END

		FETCH NEXT FROM Lineages_Cursor INTO @LineageID

	END

	CLOSE Lineages_Cursor
	DEALLOCATE Lineages_Cursor

/*===========================================================================*\	
	Temporary table to hold the list of fact keys that are relevant
\*===========================================================================*/
	CREATE TABLE #Fact (Thesaurus_Fact_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS)
	
	INSERT INTO #Fact
			--Concept facts
			SELECT DISTINCT TF.Thesaurus_Fact_Key
			FROM Thesaurus_Fact TF
			WHERE TF.Concept_Key=@ConceptKey
			UNION
			--Meaning facts
			SELECT TF.Thesaurus_Fact_Key
			FROM Thesaurus_Fact TF
			INNER JOIN Concept C ON C.Meaning_Key=TF.Meaning_Key
			WHERE C.Concept_Key=@ConceptKey
			UNION
			--Term version facts
			SELECT TF.Thesaurus_Fact_Key
			FROM Thesaurus_Fact TF
			INNER JOIN Term_Version TV on TV.Term_Version_Key=TF.Term_Version_Key
			INNER JOIN Concept C ON C.Term_Version_Key=TV.Term_Version_Key
			WHERE C.Concept_Key=@ConceptKey
			UNION
			--Facts for related term versions
			SELECT TF.Thesaurus_Fact_Key
			FROM Thesaurus_Fact TF
			INNER JOIN #TermVersionKeys TVK ON TVK.Term_Version_Key=TF.Term_Version_Key
			WHERE TF.Related_Term_Versions=1
			--Inherited concept facts
			UNION
			SELECT DISTINCT TF.Thesaurus_Fact_Key
			FROM Thesaurus_Fact TF
			INNER JOIN #InheritedConcepts IC ON IC.Concept_Key=TF.Concept_Key
			WHERE TF.Inherited=1
			UNION
			--Inherited meaning facts
			SELECT DISTINCT TF.Thesaurus_Fact_Key
			FROM Thesaurus_Fact TF
			INNER JOIN Concept C ON C.Meaning_Key=TF.Meaning_Key
			INNER JOIN #InheritedConcepts IC ON IC.Concept_Key=C.Concept_Key			
			WHERE TF.Inherited=1

/*===========================================================================*\	
	Recordset for each fact, using a subquery to ensure a distinct list
\*===========================================================================*/
	SELECT Item_Name, Data  
	FROM Thesaurus_Fact TF
	INNER JOIN #Fact ON #Fact.Thesaurus_Fact_Key=TF.Thesaurus_Fact_Key

/*===========================================================================*\	
	Recordset for each source
\*===========================================================================*/
	SELECT 
		R.Source_Key,
		RA.Author + ' - ' +
				dbo.ufn_GetDateFromVagueDate(
						R.Year_Vague_Date_Start, 
						Year_Vague_Date_End, 
						Year_Vague_Date_Type) AS AuthorAndDate,
		CASE WHEN R.TITLE IS NULL THEN
			R.FULL_REFERENCE
		ELSE
			R.Title
		END AS SourceTitle
	FROM Reference R
	INNER JOIN VW_REFERENCE_AUTHORS RA ON RA.Source_Key=R.Source_Key
	INNER JOIN Source_Join SJ ON SJ.Source_Key=R.Source_Key
	INNER JOIN Concept C ON C.Concept_Key=@ConceptKey
	LEFT JOIN Term_Version TV ON TV.Term_Version_Key=C.Term_Version_Key
	LEFT JOIN Concept_Designation CD ON CD.Concept_Key=C.Concept_Key
	WHERE (SJ.Table_Name='Concept' AND SJ.Record_Key=@ConceptKey)
		OR (SJ.Table_Name='Term' AND SJ.Record_Key=C.Term_Key)
		OR (SJ.Table_Name='Term_Version' AND SJ.Record_Key=TV.Term_Version_Key)
		OR (SJ.Table_Name='Concept_Designation' AND SJ.Record_Key=CD.Concept_Designation_Key)
		OR (SJ.Table_Name='Thesaurus_Fact' AND SJ.Record_Key IN (SELECT Thesaurus_Fact_Key FROM #Fact))

	
/*===========================================================================*\	
	Recordset for each web link
\*===========================================================================*/
	SELECT 
		DH.Item_Name, 
		DH.Image_File, 
		CASE DH.Use_Concept_Key
			WHEN 1 THEN DH.URL + C.Concept_Key
			WHEN 0 THEN DH.URL + REPLACE(CT.Plaintext COLLATE SQL_Latin1_General_CP1_CI_AS, ' ', DH.Word_Separator)
		END AS Hyperlink
	FROM Domain_Hyperlink DH
	INNER JOIN Concept_Group CG ON CG.Local_Domain_Key=DH.Local_Domain_Key
	INNER JOIN Concept C ON C.Concept_Group_Key=CG.Concept_Group_Key
	INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=C.Concept_Key
	WHERE C.Concept_Key=@ConceptKey

	--Cleanup the temporary table
	DROP TABLE #Fact
	DROP TABLE #TermVersionKeys
	
	SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermHTMLDetails_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermHTMLDetails_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TermHTMLDetails_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermHTMLDetails_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermHTMLDetails_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TermHTMLDetails_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermHTMLDetails_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermHTMLDetails_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermHTMLNames_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermHTMLNames_Select]
GO



/*===========================================================================*\
  Description:	Returns multiple recordsets suitable for populating the HTML
			details of a concept for the name and synonyms.  This is separated from 
			the rest of the details so that it can be displayed first whilst the 
			rest of the HTML is loaded.
			The following recordsets are returned:
				Title
				List Synonyms
				All Known Synonyms

  Parameters:	@Key	Collection key

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermHTMLNames_Select]
	@ConceptKey char(16)
AS

SET NOCOUNT ON

/*===========================================================================*\	
  Recordset for HTML Title (name + author).  Single record.
\*===========================================================================*/
	SELECT 
		CASE WHEN Author_Copy IS NULL THEN 
			Item_Name 
		ELSE 
			Item_Name + ' ' + Author_Copy
		END AS ItemName 
	FROM VW_ConceptTerm
	WHERE Concept_Key=@ConceptKey


/*===========================================================================*\	
  Recordset for list synonyms, ordered in language priority then alphabetical
\*===========================================================================*/
	SELECT 
		CASE WHEN C2.Author_Copy IS NULL THEN 
			T.Item_Name 
		ELSE 
			T.Item_Name + ' ' + C2.Author_Copy
		END AS ItemName,
		CASE WHEN L.Priority IS NULL THEN 0x7FFF ELSE L.Priority END AS Seq, T.Plaintext
	FROM Concept C1
		INNER JOIN Concept C2 on C2.Meaning_Key=C1.Meaning_Key
				AND C2.Concept_Group_Key=C1.Concept_Group_Key
		INNER JOIN Term T on T.Term_Key=C2.Term_Key
		INNER JOIN Language L on L.Language_Key=T.Language_Key
	WHERE C1.Concept_Key=@ConceptKey
		AND C2.Concept_Key<>@ConceptKey
	ORDER BY Seq, C2.Preferred, T.Plaintext


/*===========================================================================*\	
  Recordset for all synonyms, ordered in language priority then alphabetical
\*===========================================================================*/
	SELECT DISTINCT
		CASE WHEN C2.Author_Copy IS NULL THEN 
			T.Item_Name 
		ELSE 
			T.Item_Name + ' ' + C2.Author_Copy
		END AS ItemName,
		CASE WHEN L.Priority IS NULL THEN 0x7FFF ELSE L.Priority END AS Seq, T.Plaintext
	FROM Concept C1
		INNER JOIN Concept C2 on C2.Meaning_Key=C1.Meaning_Key
		INNER JOIN Term T on T.Term_Key=C2.Term_Key
		INNER JOIN Language L on L.Language_Key=T.Language_Key
	WHERE C1.Concept_Key=@ConceptKey
	ORDER BY Seq, T.Plaintext
	
	SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermHTMLNames_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermHTMLNames_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TermHTMLNames_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermHTMLNames_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermHTMLNames_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TermHTMLNames_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermHTMLNames_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermHTMLNames_Select TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Terms_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Terms_Select_ForSearch]
GO

/*===========================================================================*\
  Description: 	Search proc for Term table.

  Parameters:	@SearchText
		@SearchKey	Language_key

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Terms_Select_ForSearch] 
	@SearchText varchar(100),
	@SearchKey varchar(4) = NULL
AS

SET NOCOUNT ON
	-- NB: This proc does want to search on Item_Name (as opposed to 
	-- Plaintext). This is because as we are dealing with terms, we could
	-- have two terms with the same Plaintext, but different Item_Name
	-- i.e. one is italic, one isn't.

	IF @SearchKey IS NOT NULL 
		SELECT 
				T.Term_Key AS Item_Key,
				Item_Name AS DisplayTerm,
				Item_Name AS SearchTerm,
				Language_Key
		FROM		Term AS T
		WHERE		PlainText LIKE @SearchText + '%'  -- Still want to filter on PlainText though
		AND 		Language_Key = @SearchKey
		ORDER BY 	Plaintext
	ELSE
		SELECT 
				T.Term_Key AS Item_Key,
				Item_Name AS DisplayTerm,
				Item_Name AS SearchTerm,
				Language_Key
		FROM		Term AS T
		WHERE		PlainText LIKE @SearchText + '%'  -- Still want to filter on PlainText though
		ORDER BY 	Plaintext
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Terms_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Terms_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [Dev - JNCC SQL]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'BasicRecorderAccess')
        	GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [BasicRecorderAccess]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusFact_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusFact_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Thesaurus_Fact table.

  Parameters:	@Key		Thesaurus Fact key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFact_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the concept.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_Fact table exists before
			  attempting any of this deletion. 		
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_Fact]')
					AND 	  Type = 'U')
			BEGIN
				DECLARE	@TaxonFactKey char(16)

				SELECT 	@TaxonFactKey = Taxon_Fact_Key
				FROM 	Taxon_Dictionary_Thesaurus_Fact_Mapping
				WHERE	Thesaurus_Fact_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Dictionary_Thesaurus_Fact_Mapping
				WHERE	Thesaurus_Fact_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Fact
				WHERE	Taxon_Fact_Key = @TaxonFactKey

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END	
		ELSE BEGIN
			DELETE	Taxon_Dictionary_Thesaurus_Fact_Mapping
			WHERE	Thesaurus_Fact_Key = @Key

			IF @@Error <> 0 GOTO RollbackAndExit
		END

		DELETE	Thesaurus_Fact
		WHERE	Thesaurus_Fact_Key = @Key
		AND		(@Timestamp = Timestamp OR @Timestamp IS NULL)

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFact_Delete') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_ThesaurusFact_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusFact_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusFact_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Thesaurus_Fact table, for 
                Collections Module or Recorder.

  Parameters:	@Key	Thesaurus_Fact_Key

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFact_Insert]
	@Key char(16) OUTPUT,
	@ItemName varchar(100),
	@Data text,
	@MeaningKey char(16),
	@ConceptKey char(16),
	@TermVersionKey char(16),
	@RelatedTermVersions bit,
	@Inherited bit,
	@LanguageKey varchar(4),
	@FactTypeMeaningKey char(16),
	@FactTypeMeaningName varchar(100),
	@FactVagueDateStart int,
	@FactVagueDateEnd int,
	@FactVagueDateType varchar(2) = NULL,
	@SessionID char(16),
	@SystemSuppliedData bit = NULL
	
AS


	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Thesaurus_Fact', @Key OUTPUT

	DECLARE @FactTypeConceptKey char(16)

	/*-------------------------------------------------------------*\
	  There is no transaction in this proc, as the Delphi code
		wraps the whole lot in a transaction.
	\*-------------------------------------------------------------*/

	-- The combo box stores the meaning key and the item name. This meaning key
	-- needs to be converted to a concept key. Because many concepts can
	-- share the same meaning key, we have to use the meaning key and the item name.
	SELECT 		@FactTypeConceptKey = Concept_Key
	FROM 		Concept AS C
	INNER JOIN 	Term AS T ON T.Term_Key = C.Term_Key
	WHERE 		C.Meaning_Key = @FactTypeMeaningKey
	AND 		T.Item_Name = @FactTypeMeaningName

	/*-------------------------------------------------------------*\
	  Insert in Thesaurus_Fact.
	\*-------------------------------------------------------------*/
	INSERT INTO Thesaurus_Fact (
		Thesaurus_Fact_Key,
		Item_Name,
		Data,
		Meaning_Key,
		Concept_Key,
		Term_Version_Key,
		Related_Term_Versions,
		Inherited,
		Language_Key,
		Fact_Vague_Date_Start,
		Fact_Vague_Date_End,
		Fact_Vague_Date_Type,
		Fact_Type_Concept_Key,
		Entered_Session_ID,
		System_Supplied_Data
	) VALUES (
		@Key,
		@ItemName,
		@Data,
		@MeaningKey,
		@ConceptKey,
		@TermVersionKey,
		@RelatedTermVersions,
		@Inherited,
		@LanguageKey,
		@FactVagueDateStart,
		@FactVagueDateEnd,
		IsNull(@FactVagueDateType, 'U'),
		@FactTypeConceptKey,
		@SessionID,
		IsNull(@SystemSuppliedData, 0)
	)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFact_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusFact_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Insert TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusFact_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusFact_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Thesaurus_Fact table

  Parameters:	@Key	Thesaurus_Fact_Key

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFact_Update]
	@Key char(16),
	@ItemName varchar(100),
	@Data text,
	@MeaningKey char(16),
	@ConceptKey char(16),
	@TermVersionKey char(16),
	@RelatedTermVersions bit,
	@Inherited bit,
	@LanguageKey varchar(4),
	@FactTypeMeaningKey char(16),
	@FactTypeMeaningName varchar(100),
	@FactVagueDateStart int,
	@FactVagueDateEnd int,
	@FactVagueDateType varchar(2) = NULL,
	@SessionID char(16), 
	@SystemSuppliedData bit = NULL,
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		DECLARE @FactTypeConceptKey char(16)

		-- The combo box stores the meaning key and the item name. This meaning key
		-- needs to be converted to a concept key. Because many concepts can
		-- share the same meaning key, we have to use the meaning key and the item name.
		SELECT 		@FactTypeConceptKey = Concept_Key
		FROM 		Concept AS C
		INNER JOIN 	Term AS T ON T.Term_Key = C.Term_Key
		WHERE 		C.Meaning_Key = @FactTypeMeaningKey
		AND 		T.Item_Name = @FactTypeMeaningName

		UPDATE	Thesaurus_Fact
		SET	Item_Name = @ItemName,
			Data = @Data,
			Meaning_Key = @MeaningKey,
			Concept_Key = @ConceptKey,
			Term_Version_Key = @TermVersionKey,
			Related_Term_Versions = @RelatedTermVersions,
			Inherited = @Inherited,
			Language_Key = @LanguageKey,
			Changed_Session_ID = @SessionID,
			Fact_Vague_Date_Start = @FactVagueDateStart,
			Fact_Vague_Date_End = @FactVagueDateEnd,
			Fact_Vague_Date_Type = IsNull(@FactVagueDateType, 'U'),
			Fact_Type_Concept_Key = @FactTypeConceptKey,
			System_Supplied_Data = IsNull(@SystemSuppliedData, 0)
		WHERE	Thesaurus_Fact_Key = @Key
		AND		@Timestamp = Timestamp

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION

	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFact_Update') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ThesaurusFact_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Update TO [Dev - JNCC SQL]
END
GO

