SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_AnyMeasurementParameter_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_AnyMeasurementParameter_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of any concept that can be used as a 
		measurement parameter

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_AnyMeasurementParameter_Select]

AS

SET NOCOUNT ON

SELECT 
		CT.Concept_Key, 
		CT.Plaintext + ' (' + D.Item_Name + ')' COLLATE SQL_Latin1_General_CP1_CI_AS AS Item_Name
FROM Concept_Group CG
INNER JOIN VW_ConceptTerm CT ON CT.Concept_Group_Key=CG.Concept_Group_Key
		AND CT.Is_Current=1
INNER JOIN Local_Domain LD ON LD.Local_Domain_Key=CG.Local_Domain_Key
INNER JOIN Domain D ON D.Domain_Key=LD.Domain_Key
		AND (D.Has_Occurrences=1 OR D.Domain_Key='SYSTEM00000000')
WHERE CG.Item_Name='Measurement Parameters'
ORDER BY D.Item_Name, CT.Plaintext

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_AnyMeasurementParameter_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_AnyMeasurementParameter_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_AnyMeasurementParameter_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_AnyMeasurementParameter_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_AnyMeasurementParameter_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_AnyMeasurementParameter_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_AnyMeasurementParameter_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_AnyMeasurementParameter_Select TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForSearchByGeographicInformation]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForSearchByGeographicInformation]
GO

CREATE PROCEDURE [dbo].[usp_Collections_Select_ForSearchByGeographicInformation] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(100)

AS

--  DESCRIPTION
--  Returns Collections based on the search parameter for Geographic Information
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--	@SearchText			Text to be used for search
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-12
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, MT.Text AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		AND (C.Item_Name LIKE @SearchText + '%')
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

		INNER JOIN
			METADATA MT
		ON C.Collection_Unit_Key = MT.Record_Key
			AND MT.MetaData_Type_Key = 'SYSTEM0000000004'
	ORDER BY Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 1
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, MT.Text AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		AND (C.Item_Name LIKE @SearchText + '%')
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

		INNER JOIN
			METADATA MT
		ON C.Collection_Unit_Key = MT.Record_Key
			AND MT.MetaData_Type_Key = 'SYSTEM0000000004'
	ORDER BY C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 2
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, MT.Text AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		AND (C.Item_Name LIKE @SearchText + '%')
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

		INNER JOIN
			METADATA MT
		ON C.Collection_Unit_Key = MT.Record_Key
			AND MT.MetaData_Type_Key = 'SYSTEM0000000004'
	ORDER BY M.Number

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForSearchByGeographicInformation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForSearchByGeographicInformation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByGeographicInformation TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByGeographicInformation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByGeographicInformation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByGeographicInformation TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByGeographicInformation TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByGeographicInformation TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForTopLevel]
GO

CREATE PROCEDURE [dbo].[usp_Collections_Select_ForTopLevel] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@Key CHAR(16) = NULL

AS

--  DESCRIPTION
--  Returns top level Collections data to the CollectionsBrowser
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@Key 				Optional Key. When specified, only the single top level record is returned with that key
--	@SortOrderIndex		Index determining Sort Order
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-08-12
--
SET NOCOUNT ON

-- Create  a table to hold the items we are looking for
DECLARE @Search TABLE (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)

IF @Key IS NOT NULL
		INSERT INTO @Search VALUES (@Key)
ELSE IF object_id('tempdb..#TempFilter') is not null
	INSERT INTO @Search SELECT DISTINCT ItemKey FROM #TempFilter
ELSE
	INSERT INTO @Search SELECT Collection_Unit_Key FROM Collection

IF @SortOrderIndex = 0
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number
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
		INNER JOIN @Search S ON S.ItemKey=CU.Collection_Unit_Key
	ORDER BY Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 1
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number
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
		INNER JOIN @Search S ON S.ItemKey=CU.Collection_Unit_Key
	ORDER BY C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 2
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number
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
		INNER JOIN @Search S ON S.ItemKey=CU.Collection_Unit_Key
	ORDER BY M.Number

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForTopLevel TO [Dev - JNCC SQL]
END

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
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitData_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitData_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Collection_Unit_Data table.
		The Collection_Unit_Data table hold descriptor and measurement
		information.

  Parameters:	@Key
		@CollectionUnitKey
		@AppliesTo
		@MethodConceptKey
		@Duration
		@Accuracy
		@ParameterConceptKey
		@UnitConceptKey
		@LowerValue
		@UpperValue
		@IsDescriptor
		@SessionID

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitData_Insert]
	-- Required by both Measurements and Descriptors updates.
	@Key char(16) OUTPUT,
	@IsDescriptor bit,
	@ParameterConceptKey char(16),
	@AppliesTo varchar(50),
	@Value varchar(50) = NULL,
	@CollectionUnitKey char(16),
	@SessionID char(16),
	-- Only required for the Measurements update.
	@LowerValue varchar(50) = NULL,
	@UpperValue varchar(50) = NULL,
	@MethodConceptKey char(16) = NULL,
	@Duration varchar(50) = NULL,
	@Accuracy varchar(50) = NULL,
	@UnitConceptKey char(16) = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	IF @IsDescriptor = 1
		SET @LowerValue = @Value

	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Collection_Unit_Data', @Key OUTPUT

	BEGIN TRANSACTION
		/*----------------------------------------------------------------------------------*\
		  If we are inserting measurement data, more information needs to inserted than if 
		  we are inserting descriptor data. Hence, there are two different insert statements.
		\*----------------------------------------------------------------------------------*/
		IF @IsDescriptor = 1
			INSERT INTO Collection_Unit_Data (
				Collection_Unit_Data_Key, Collection_Unit_Key, Applies_To, 
				Parameter_Concept_Key, Lower_Value, Is_Descriptor, Entered_Session_ID
			) VALUES (
				@Key, @CollectionUnitKey, @AppliesTo, @ParameterConceptKey, 
				IsNull(@LowerValue, ' '), @IsDescriptor, @SessionID
			)
		ELSE
			INSERT INTO Collection_Unit_Data (
				Collection_Unit_Data_Key, Collection_Unit_Key, Applies_To,
				Method_Concept_Key, Duration, Accuracy, Parameter_Concept_Key,
				Unit_Concept_Key, Lower_Value, Upper_Value, Is_Descriptor,
				Entered_Session_ID
			) VALUES (
				@Key, @CollectionUnitKey, @AppliesTo, @MethodConceptKey, @Duration,
				@Accuracy, @ParameterConceptKey, @UnitConceptKey, IsNull(@LowerValue, ' '),
				@UpperValue, @IsDescriptor, @SessionID
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitData_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitData_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitData_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitData_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitData_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitData_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of either measurements or descriptors from
		Collection_Unit_Data table record.

  Parameters:	@Key		Collection key
		@IsDescriptor	Flag to indicate whether Measurements or Descriptors
				are requested.
		@DomainConceptGroupName
				Name of the domain concept group where new parameters
				added by users should go. If the concept group exists
				for the domain, its key will be returned, ready for use.

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_CollectionUnitData_Select]
	@Key char(16),
	@IsDescriptor bit
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

	-- Descriptor tab page wants to match on Collection_Unit_Key because
	-- there will be multiple rows.
	IF @IsDescriptor = 1 
	BEGIN
		SELECT 		CUD.Collection_Unit_Data_Key AS Item_Key,
				CUD.Applies_To,
				CUD.Method_Concept_Key,
				CTM.Item_Name AS Method_Term,
				CUD.Duration,
				CUD.Accuracy,
				CUD.Parameter_Concept_Key,
				CTP.Item_Name AS Parameter_Term,
				CUD.Unit_Concept_Key,
				CTU.Item_Name AS Unit_Term,
				CUD.Lower_Value AS Value,
				CUD.Lower_Value, 
				CUD.Upper_Value,				
				CUD.Custodian,
				CUD.[Timestamp],
				S.Date_Time_Start
		FROM 		Collection_Unit_Data CUD
		INNER JOIN 	vw_ConceptTerm CTP ON CTP.Concept_Key = CUD.Parameter_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTM ON CTM.Concept_Key = CUD.Method_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTU ON CTU.Concept_Key = CUD.Unit_Concept_Key
		LEFT JOIN 	Session S ON CUD.Entered_Session_ID = S.Session_ID
		LEFT JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = CUD.Collection_Unit_Key
		WHERE 		CUD.Collection_Unit_Key = @Key 
		AND 		CUD.Is_Descriptor = @IsDescriptor
	END
	ELSE
	-- Measurements wants to match on Collection_Unit_Data_Key because this is
	-- the key that is stored in the nodes.
	BEGIN
		SELECT 		CUD.Collection_Unit_Data_Key AS Item_Key,
				CUD.Applies_To,
				CUD.Method_Concept_Key,
				CTM.Item_Name AS Method_Term,
				CUD.Duration,
				CUD.Accuracy,
				CUD.Parameter_Concept_Key,
				CTP.Item_Name AS Parameter_Term,
				CUD.Unit_Concept_Key,
				CTU.Item_Name AS Unit_Term,
				CUD.Lower_Value AS Value,
				CUD.Lower_Value, 
				CUD.Upper_Value,				
				CUD.Custodian,
				CUD.[Timestamp],
				S.Date_Time_Start
		FROM 		Collection_Unit_Data CUD
		INNER JOIN 	vw_ConceptTerm CTP ON CTP.Concept_Key = CUD.Parameter_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTM ON CTM.Concept_Key = CUD.Method_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTU ON CTU.Concept_Key = CUD.Unit_Concept_Key
		LEFT JOIN 	Session S ON CUD.Entered_Session_ID = S.Session_ID
		LEFT JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = CUD.Collection_Unit_Key
		WHERE 		CUD.Collection_Unit_Data_Key = @Key 
		AND 		CUD.Is_Descriptor = @IsDescriptor
	END

SET NOCOUNT OFF 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitData_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitData_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitData_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitData_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitData_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitData_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Collection_Unit_Data table.
		The Collection_Unit_Data table hold descriptor and measurement
		information.

  Parameters:	@Key
		@CollectionUnitKey
		@AppliesTo
		@MethodConceptKey
		@Duration
		@Accuracy
		@ParameterConceptKey
		@UnitConceptKey
		@LowerValue
		@UpperValue
		@IsDescriptor
		@SessionID
		@Timestamp

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitData_Update]
	-- Required by both Measurements and Descriptors updates.
	@Key char(16),
	@IsDescriptor bit,
	@ParameterConceptKey char(16),
	@AppliesTo varchar(50),
	@Value varchar(50) = NULL,
	@SessionID char(16),
	@Timestamp timestamp,
	-- Only required for the Measurements update.
	@LowerValue varchar(50) = NULL,
	@UpperValue varchar(50) = NULL,
	@CollectionUnitKey char(16) = NULL,
	@MethodConceptKey char(16) = NULL,
	@Duration varchar(50) = NULL,
	@Accuracy varchar(50) = NULL,
	@UnitConceptKey char(16) = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	IF @IsDescriptor = 1
		SET @LowerValue = @Value

	/*----------------------------------------------------------------------------------*\
	  If we are updating measurement data, more information needs to changed than if
	  we are inserting descriptor data. Hence, there are two different update statements.
	\*----------------------------------------------------------------------------------*/
	BEGIN TRANSACTION

		IF @IsDescriptor = 1	
			-- Updating a descriptor.	
			UPDATE	Collection_Unit_Data
			SET	
				Applies_To = @AppliesTo,		
				Parameter_Concept_Key = @ParameterConceptKey,
				Lower_Value = IsNull(@LowerValue, ' '),
				Is_Descriptor = @IsDescriptor,
				Changed_Session_ID = @SessionID
			WHERE	Collection_Unit_Data_Key = @Key
			AND	(@Timestamp = Timestamp)
		ELSE			
			-- Updating a measurement.
			UPDATE	Collection_Unit_Data
			SET	
				Applies_To = @AppliesTo,		
				Parameter_Concept_Key = @ParameterConceptKey,
				Lower_Value = IsNull(@LowerValue, ' '),
				Is_Descriptor = @IsDescriptor,
				Changed_Session_ID = @SessionID,
				Collection_Unit_Key = @CollectionUnitKey,
				Method_Concept_Key = @MethodConceptKey,
				Duration = @Duration,
				Accuracy = @Accuracy,
				Unit_Concept_Key = @UnitConceptKey,
				Upper_Value = @UpperValue
			WHERE	Collection_Unit_Data_Key = @Key
			AND	(@Timestamp = Timestamp)

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitData_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitData_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitData_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitData_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitMaterial_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitMaterial_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of materials used for/with the specified 
		Collection Unit item

  Parameters:	@Key	Collection unit key

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitMaterial_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	  Options to get performance benefits from VW_ConceptTerm.
	\*-------------------------------------------------------------*/
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	SELECT		CUM.Collection_Unit_Material_Key AS Item_Key,			
			CUM.Material_Concept_Key,
			CT1.Item_Name AS Material_Item_Name,
			CUM.Quantity,
			CUM.Unit_Concept_Key,
			CT2.Item_Name AS Unit_Item_Name,
			CUM.Custodian,
			CUM.[Timestamp]

	FROM		Collection_Unit_Material CUM
	INNER JOIN	vw_ConceptTerm CT1 ON CT1.Concept_Key = CUM.Material_Concept_Key
	LEFT JOIN	vw_ConceptTerm CT2 ON CT2.Concept_Key = CUM.Unit_Concept_Key

	WHERE		CUM.Collection_Unit_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitMaterial_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitMaterial_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitNumber_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitNumber_Update]
GO

/*===========================================================================*\
  Description:	Updates a record into the Collection Unit Number table

  Parameters:	@Key

		@Timestamp

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitNumber_Update]
	@Key char(16),
	@CollectionUnitKey char(16),
	@Number varchar(30),
	@TypeConceptKey char(16),
	@Preferred bit,
	@Notes text,
	@SessionID char(16),
	@Timestamp timestamp,
	@RecordsAffected int OUTPUT

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @Error int
	/*---------------------------*\
	  Actually updates the table
	\*---------------------------*/
	BEGIN TRANSACTION

		IF @Preferred = 1
			UPDATE 	Collection_Unit_Number
			SET	Preferred = 0
			WHERE	Collection_Unit_Key = @CollectionUnitKey
			AND	Type_Concept_Key = @TypeConceptKey
			AND	Collection_Unit_Number_Key <> @Key -- So we don't get timestamp problems.
		
		UPDATE 	Collection_Unit_Number
		SET 	Collection_Unit_Key= @CollectionUnitKey,
			Number = @Number,
			Type_Concept_Key = @TypeConceptKey,
			Preferred = @Preferred,
			Notes = @Notes,
			Changed_Session_ID = @SessionID

		WHERE	Collection_Unit_Number_Key = @Key
		AND	(@Timestamp = Timestamp)

		SELECT	@RecordsAffected = @@RowCount,
			@Error = @@Error

		IF @Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitNumber_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitNumber_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitStatus_Get]')
	   AND 	  Type = 'P')
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
	@Status varchar(50) OUTPUT
AS
	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	-- If unknown, set result and exit.
	IF NOT EXISTS(SELECT * FROM Movement_Collection_Unit WHERE Collection_Unit_Key = @Key)
	BEGIN
		SET @Status = 'Unknown'
		RETURN
	END

	/*-------------------------------------------------------------*\
	  Need to work out what it is.
	\*-------------------------------------------------------------*/
	-- Some variables.
	DECLARE	@HoldingOrganisationKey char(16),
		@OwnershipNameKey char(16),
		@OwnershipMovementType int,
		@MaterialNameKey char(16),
		@MaterialMovementType int,
		@DepartmentName varchar(100)

	-- Get the organisation holding software install	
	SELECT	@HoldingOrganisationKey = Data FROM Setting WHERE [Name] = 'HoldingOrg'

	/*-------------------------------------------------------------*\
	  Get most recent movement of ownership.
	\*-------------------------------------------------------------*/
	SELECT	TOP 1	@OwnershipNameKey = Receiver_Name_Key, 
			@OwnershipMovementType = Movement_Type

	FROM		Movement_Collection_Unit MCU
	INNER JOIN 	Movement_Direction MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN 	Movement M ON M.Movement_Key = MD.Movement_Key
	INNER JOIN 	Movement_Of_Ownership MO ON MO.Movement_Direction_Key = MCU.Movement_Direction_Key
	LEFT JOIN 	Movement_Of_Ownership_Exclusion MOE ON MOE.Movement_Of_Ownership_Key = MO.Movement_Of_Ownership_Key

	WHERE		MCU.Collection_Unit_Key = @Key
	AND 		MOE.Movement_Of_Ownership_Key IS NULL

	ORDER BY	MO.Vague_Date_Start DESC
	
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
	LEFT JOIN 	Movement_Of_Material_Exclusion MME ON MME.Movement_Of_Material_Key = MM.Movement_Of_Material_Key
	LEFT JOIN	Organisation_Department OD ON OD.Organisation_Department_Key = MM.Receiver_Organisation_Department_Key

	WHERE		MCU.Collection_Unit_Key = @Key
	AND 		MME.Movement_Of_Material_Key IS NULL

	ORDER BY	MM.Vague_Date_Start DESC

	/*-------------------------------------------------------------*\
	  Now work out what to display.
	\*-------------------------------------------------------------*/

	-- No-one has the stuff
	IF @MaterialNameKey IS NULL
		SET @Status = 	CASE @MaterialMovementType
					WHEN 4 THEN 'Destroyed'
					WHEN 7 THEN 'Lost'
					WHEN 8 THEN 'Sold'
				END
	ELSE
	IF @MaterialMovementType = 5 SET @Status = 'Disposed'
	ELSE
	-- It's here and it's owned here
	IF @MaterialNameKey = @HoldingOrganisationKey AND @OwnershipNameKey = @HoldingOrganisationKey
		SET @Status = 'In ' + @DepartmentName + ' department.'
	ELSE
	-- Got it here but owner by someone else
	IF @MaterialNameKey = @HoldingOrganisationKey 
		SET @Status = 'On loan from ' + dbo.ufn_GetFormattedName(@HoldingOrganisationKey)
	ELSE
	-- Don't have it here and owned by someone else
	IF @OwnershipNameKey <> @HoldingOrganisationKey
		SET @Status = 'Owned by ' + dbo.ufn_GetFormattedName(@OwnershipNameKey)
	ELSE
	-- Don't have it here, but owned here
	IF @OwnershipNameKey <> @MaterialNameKey
	BEGIN
		SET @Status = 'On loan to ' + dbo.ufn_GetFormattedName(@MaterialNameKey)
		IF @DepartmentName IS NOT NULL
			SET @Status = @Status + ' ' + @DepartmentName
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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnits_Select_ForTask]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnits_Select_ForTask]
GO

/*===========================================================================*\
  Description: 	Selects the Collection Units associated with a task.
		The values for Collection_Unit_Type are as follows:
			0 as Collection, 
			1 as Specimen, 
			2 as Store

  Parameters:	@Key		Movement_Of_Material_Key
		@TaskKey

  Created:	January 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnits_Select_ForTask]
	@TaskKey CHAR(16),
	@UserDomainMask INT,
	@SessionID CHAR(16)
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF
	
	/*-----------------------------------------*\
	  Select the Collections.
	\*-----------------------------------------*/
	SELECT 		CUC.Collection_Unit_Key,
			C.Item_Name,
			0 AS Collection_Unit_Type,
			CASE WHEN CUT.Conservation_Task_Key IS NULL 	THEN 0
									ELSE 1
			END AS Included 
	FROM		Conservation_Task AS CT 
	INNER JOIN	Collection_Unit_Check AS CUC ON CUC.Conservation_Check_Key = CT.Conservation_Check_Key
	LEFT JOIN	Collection_Unit_Task AS CUT ON CUT.Conservation_Task_Key = CT.Conservation_Task_Key
							AND CUT.Collection_Unit_Key = CUC.Collection_Unit_Key
	INNER JOIN	Collection AS C ON C.Collection_Unit_Key = CUC.Collection_Unit_Key
  INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key = C.Collection_Unit_Key
        AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE		CT.Conservation_Task_Key = @TaskKey

	UNION

	/*-----------------------------------------*\
	  Select the Specimens.
	\*-----------------------------------------*/
	SELECT 		CUC.Collection_Unit_Key,
			CASE WHEN SU.Life_Sciences = 0 
				THEN ISNULL(CTP.Item_Name, 'No Determination') 
				ELSE ISNULL(dbo.ufn_GetFormattedTaxonNameByParams(ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name, 
							ITN.Common_Name_Italic, ITN.Authority, 1), 'No Determination')
			END AS Item_Name, 
			1 AS Collection_Unit_Type,
			CASE WHEN CUT.Conservation_Task_Key IS NULL 	THEN 0
									ELSE 1
			END AS Included 
	FROM		Conservation_Task AS CT 
	INNER JOIN	Collection_Unit_Check AS CUC ON CUC.Conservation_Check_Key = CT.Conservation_Check_Key
	LEFT JOIN	Collection_Unit_Task AS CUT ON CUT.Conservation_Task_Key = CT.Conservation_Task_Key
							AND CUT.Collection_Unit_Key = CUC.Collection_Unit_Key
	INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = CUC.Collection_Unit_Key
  INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key
        AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	LEFT JOIN 	Determination D ON SU.Preferred_Determination_Key = D.Determination_Key
	LEFT JOIN 	VW_ConceptTermPreferred CTP ON D.Concept_Key = CTP.Concept_Key 
	LEFT JOIN 	Taxon_Determination TD ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
	LEFT JOIN 	Index_Taxon_Name ITN ON TD.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
	WHERE		CT.Conservation_Task_Key = @TaskKey

	UNION

	/*-----------------------*\
	  Select the Stores.
	\*-----------------------*/
	SELECT 		CUC.Collection_Unit_Key,
			S.Item_Name, 
			2 AS Collection_Unit_Type,
			CASE WHEN CUT.Conservation_Task_Key IS NULL 	THEN 0
									ELSE 1
			END AS Included 
	FROM		Conservation_Task AS CT 
	INNER JOIN	Collection_Unit_Check AS CUC ON CUC.Conservation_Check_Key = CT.Conservation_Check_Key
	LEFT JOIN	Collection_Unit_Task AS CUT ON CUT.Conservation_Task_Key = CT.Conservation_Task_Key
							AND CUT.Collection_Unit_Key = CUC.Collection_Unit_Key
	INNER JOIN	Store AS S ON S.Collection_Unit_Key = CUC.Collection_Unit_Key
  INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
        AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE		CT.Conservation_Task_Key = @TaskKey


	ORDER BY Collection_Unit_Type, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnits_Select_ForTask') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnits_Select_ForTask'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnits_Select_ForTask TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnits_Select_ForTask TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnits_Select_ForTask TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnits_Select_ForTask TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnits_Select_ForTask TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnits_Select_ForTask TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck]
GO
/*===========================================================================*\
  Description:	When a Task is being linked to a Condition Check, all of the
		Condition Check's Collection Units need to be linked to the
		Task. This is done by duplicating the records from the
		Collection_Unit_Check table to the Collection_Unit_Task table
		that aren't already there.

  Parameters:	@ConservationTaskKey 
		@ConservationCheckKey

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck]
	@ConservationTaskKey char(16),
	@ConservationCheckKey char(16),
	@SessionID char(16)	
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE	@CollectionUnitKey char(16),
		@CollectionUnitTaskKey char(16)

	DECLARE curCollectionUnitCheck CURSOR LOCAL STATIC FOR
		SELECT 		T1.Collection_Unit_Key
		FROM		Collection_Unit_Check AS T1
		LEFT JOIN	Collection_Unit_Task AS T2 ON T1.Collection_Unit_Key = T2.Collection_Unit_Key
							   AND T2.Conservation_Task_Key = @ConservationTaskKey
		WHERE		T1.Conservation_Check_Key = @ConservationCheckKey
		AND		T2.Collection_Unit_Key IS NULL
	
	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		OPEN curCollectionUnitCheck

		FETCH NEXT
		FROM	curCollectionUnitCheck
		INTO	@CollectionUnitKey

		WHILE @@Fetch_Status = 0
		BEGIN
			EXECUTE spNextKey 'Collection_Unit_Task', @CollectionUnitTaskKey OUTPUT
	
			INSERT INTO Collection_Unit_Task
				(Collection_Unit_Task_Key,
				Conservation_Task_Key,
				Collection_Unit_Key,
				Entered_Session_ID)
			VALUES
				(@CollectionUnitTaskKey,
				@ConservationTaskKey,
				@CollectionUnitKey,
				@SessionID)
		
			IF @@Error <> 0 GOTO RollbackAndExit

			FETCH NEXT
			FROM	curCollectionUnitCheck
			INTO	@CollectionUnitKey
		END

		CLOSE curCollectionUnitCheck
		DEALLOCATE curCollectionUnitCheck
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnit_Update_ForConditionCheck]')
	   AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnit_Update_ForConditionCheck]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Conservation_Unit_Check join table 
		so that there is a relationship between the Condition Check 
		and Collection Unit tables.

  Parameters:	@ParentKey 	The key of the top level (parent) Condition Check node.
		@ChildKey	The key of the added (child) Collection Unit node. 
		@SessionID

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_CollectionUnit_Update_ForConditionCheck] 
	@ParentKey char(16),
	@ChildKey char(16),
	@SessionID char(16)
AS

	SET NOCOUNT ON

	DECLARE @Key char(16),
		@TaskKey char(16)
	EXECUTE spNextKey 'Collection_Unit_Check', @Key OUTPUT

	BEGIN TRANSACTION

		INSERT INTO Collection_Unit_Check (
			Collection_Unit_Check_Key, Conservation_Check_Key, Collection_Unit_Key, Entered_Session_ID
		) VALUES (
			@Key, @ParentKey, @ChildKey, @SessionID
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		/*---------------------------------------------------------------------------------*\
		  If the Conservation Check has one task linked to it, we will want to duplicate 
		  records from the Collection_Unit_Check table into the Collection_Unit_Table.
		  Here we are going to find out how many Tasks are linked to the Conservation_Check.
		  If it is just one, then we have stored this task key in @TaskKey and can run
		  usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck which will duplicate the 
		  records for us.
		\*---------------------------------------------------------------------------------*/

		SELECT 	@TaskKey = Conservation_Task_Key
		FROM	Conservation_Task
		WHERE	Conservation_Check_Key = @ParentKey

		IF @@RowCount = 1
		BEGIN
			EXEC usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck @TaskKey, @ParentKey, @SessionID
		END

		IF @@Error <> 0 GOTO RollbackAndExit
	
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnit_Update_ForConditionCheck') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnit_Update_ForConditionCheck'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForConditionCheck TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForConditionCheck TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForConditionCheck TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForConditionCheck TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForConditionCheck TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_CollectorsAndDeterminers_Select_ForCollection]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_CollectorsAndDeterminers_Select_ForCollection]
GO

CREATE PROCEDURE [dbo].[usp_CollectorsAndDeterminers_Select_ForCollection] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ParentKey CHAR(16)

AS
--  DESCRIPTION
--  Returns Collectors And Determiners data to the CollectionsBrowser for a given Collection Unit
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@ParentKey 			When specified, only the records associated with the parent key are returned
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-25
--
SET NOCOUNT ON

SELECT DISTINCT	I.Name_Key AS Item_Key,
		dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname) AS Item_Name

FROM 		Specimen_Unit AS SU
INNER JOIN 	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
LEFT JOIN	(Specimen_Field_Data SFD 
			LEFT JOIN	Occurrence AS O ON O.Occurrence_Key = SFD.Occurrence_Key
			LEFT JOIN	Taxon_Occurrence AS XO ON XO.Taxon_Occurrence_Key = SFD.Taxon_Occurrence_Key
			INNER JOIN	[Sample] AS S ON (S.Sample_Key = O.Sample_Key
							OR S.Sample_Key = XO.Sample_Key)
			INNER JOIN	Sample_Recorder AS SR ON SR.Sample_Key = S.Sample_Key
			INNER JOIN	Survey_Event_Recorder AS SER ON SER.SE_Recorder_Key = SR.SE_Recorder_Key) 
		ON SFD.Collection_Unit_Key = SU.Collection_Unit_Key
		AND SFD.Gathering_Event = 1
LEFT JOIN 	Determination AS D ON SU.Collection_Unit_Key = D.Specimen_Collection_Unit_Key
LEFT JOIN	Taxon_Determination TD 	ON SU.Collection_Unit_Key = TD.Specimen_Collection_Unit_Key
INNER JOIN	Individual AS I ON (I.Name_Key = SER.Name_Key)
				OR (D.Determiner_Name_Key = I.Name_Key) 
				OR (TD.Determiner = I.Name_Key)
WHERE		SU.Parent_Collection_Collection_Unit_Key = @ParentKey
ORDER BY	Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectorsAndDeterminers_Select_ForCollection') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectorsAndDeterminers_Select_ForCollection'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectorsAndDeterminers_Select_ForCollection TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectorsAndDeterminers_Select_ForCollection TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectorsAndDeterminers_Select_ForCollection TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectorsAndDeterminers_Select_ForCollection TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectorsAndDeterminers_Select_ForCollection TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectorsAndDeterminers_Select_ForCollection TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_CollectorsAndDeterminers_Select_ForSpecimen]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_CollectorsAndDeterminers_Select_ForSpecimen]
GO

CREATE PROCEDURE [dbo].[usp_CollectorsAndDeterminers_Select_ForSpecimen] 
@ParentKey CHAR(16)

AS
--  DESCRIPTION
--  Returns Collectors And Determiners data to the CollectionsBrowser for a given Collection Unit
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@ParentKey 			When specified, only the records associated with the parent key are returned
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-25
--
SET NOCOUNT ON

SELECT DISTINCT	I.Name_Key AS Item_Key,
		dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname) AS Item_Name

FROM 		Specimen_Unit AS SU
LEFT JOIN	(Specimen_Field_Data SFD 
			LEFT JOIN	Occurrence AS O ON O.Occurrence_Key = SFD.Occurrence_Key
			LEFT JOIN	Taxon_Occurrence AS XO ON XO.Taxon_Occurrence_Key = SFD.Taxon_Occurrence_Key
			INNER JOIN	[Sample] AS S ON (S.Sample_Key = O.Sample_Key
							OR S.Sample_Key = XO.Sample_Key)
			INNER JOIN	Sample_Recorder AS SR ON SR.Sample_Key = S.Sample_Key
			INNER JOIN	Survey_Event_Recorder AS SER ON SER.SE_Recorder_Key = SR.SE_Recorder_Key) 
		ON SFD.Collection_Unit_Key = SU.Collection_Unit_Key
		AND SFD.Gathering_Event = 1
LEFT JOIN 	Determination AS D ON SU.Collection_Unit_Key = D.Specimen_Collection_Unit_Key
LEFT JOIN	Taxon_Determination TD 	ON SU.Collection_Unit_Key = TD.Specimen_Collection_Unit_Key
INNER JOIN	Individual AS I ON (I.Name_Key = SER.Name_Key)
				OR (D.Determiner_Name_Key = I.Name_Key) 
				OR (TD.Determiner = I.Name_Key)
WHERE		SU.Collection_Unit_Key = @ParentKey
ORDER BY	Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectorsAndDeterminers_Select_ForSpecimen') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectorsAndDeterminers_Select_ForSpecimen'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectorsAndDeterminers_Select_ForSpecimen TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectorsAndDeterminers_Select_ForSpecimen TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectorsAndDeterminers_Select_ForSpecimen TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectorsAndDeterminers_Select_ForSpecimen TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectorsAndDeterminers_Select_ForSpecimen TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectorsAndDeterminers_Select_ForSpecimen TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptDesignation_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptDesignation_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Concept_Designation table

  Parameters:	

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptDesignation_Insert]
	@Key char(16) OUTPUT,
	@ConceptKey char(16),
	@DesignationTypeConceptKey char(16),
	@FromVagueDateStart int,
	@FromVagueDateEnd int,
	@FromVagueDateType varchar(2) = NULL,
	@ToVagueDateStart int,
	@ToVagueDateEnd int,
	@ToVagueDateType varchar(2) = NULL,
	@SessionID char(16),
	@SystemSuppliedData bit = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Concept_Designation', @Key OUTPUT

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Concept_Designation.
		\*-------------------------------------------------------------*/
		INSERT INTO Concept_Designation (
			Concept_Designation_Key,
			Concept_Key,
			Designation_Type_Concept_Key,
			From_Vague_Date_Start,
			From_Vague_Date_End,
			From_Vague_Date_Type,
			To_Vague_Date_Start,
			To_Vague_Date_End,
			To_Vague_Date_Type,
			Entered_Session_ID,
			System_Supplied_Data			
		) VALUES (
			@Key,
			@ConceptKey,
			@DesignationTypeConceptKey,
			@FromVagueDateStart,
			@FromVagueDateEnd,
			IsNull(@FromVagueDateType, 'U'),
			@ToVagueDateStart,
			@ToVagueDateEnd,
			@ToVagueDateType,
			@SessionID,
			IsNull(@SystemSuppliedData, 0)
		)
		IF @@Error <> 0 GOTO RollbackAndExit

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION

RETURN 0

RollBackAndExit: 
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptDesignation_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptDesignation_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptDesignation_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptDesignation_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptDesignation_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptDesignation_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptDesignation_Insert TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupKey_Get_ForConceptGroupVersion]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroupKey_Get_ForConceptGroupVersion]
GO

/*===========================================================================*\
  Description:	Outputs the concept group key for a concept.

  Parameters:	@Key	ConceptGroupVersion key

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupKey_Get_ForConceptGroupVersion]
	@Key char(16),
	@ConceptGroupKey char(16) OUTPUT
AS
	SELECT 		@ConceptGroupKey = Concept_Group_Key
	FROM		Concept_Group_Version
	WHERE		Concept_Group_Version_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupKey_Get_ForConceptGroupVersion') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupKey_Get_ForConceptGroupVersion'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupKey_Get_ForConceptGroupVersion TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupKey_Get_ForConceptGroupVersion TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupKey_Get_ForConceptGroupVersion TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupKey_Get_ForConceptGroupVersion TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupKey_Get_ForConceptGroupVersion TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupKey_Get_ForConceptGroupVersion TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupVersions_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroupVersions_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Returns Concept Group Version records.

  Parameters:	@ConceptKey	Concept_Key

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupVersions_Select_ForConcept]
	@ConceptKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT DISTINCT
			CGV.Concept_Group_Version_Key AS [Key],
		  	CGV.Version AS Item_Name,
			'('	+ dbo.ufn_GetDateFromVagueDate(CGV.From_Vague_Date_Start,
								CGV.From_Vague_Date_End,
								CGV.From_Vague_Date_Type) 
				+ IsNull((' - ' + dbo.ufn_GetDateFromVagueDate(CGV.To_Vague_Date_Start,
								CGV.To_Vague_Date_End,
								CGV.To_Vague_Date_Type)), '')
				+ ')'
			AS Dates,
			CGV.[Sequence]
	FROM 	  	Concept AS C
	INNER JOIN	Concept_Group_Version AS CGV ON CGV.Concept_Group_Key = C.Concept_Group_Key
	WHERE	  	C.Concept_Key = @ConceptKey
	ORDER BY  	Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupVersions_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupVersions_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConcept TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupVersion_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroupVersion_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a Concept_Group_Version record.

  Parameters:	@Key	Concept_Group_Version key
		@Timestamp,
		@RecordsAffected

  Created:	November 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupVersion_Delete]
	@Key char(16),
	@Timestamp timestamp,
	@RecordsAffected int OUTPUT
AS

SET NOCOUNT ON

	BEGIN TRANSACTION
		DECLARE 
			@ConceptGroupKey CHAR(16),
			@Error INT

		SELECT @ConceptGroupKey=Concept_Group_Key
		FROM 	Concept_Group_Version
		WHERE	Concept_Group_Version_Key = @Key

		DELETE 
		FROM 	Concept_Group_Version
		WHERE	Concept_Group_Version_Key = @Key
		AND	(@Timestamp = Timestamp)

		SELECT @RecordsAffected=@@ROWCOUNT, @Error = @@Error

		IF @Error <> 0 GOTO RollbackAndExit

		IF @RecordsAffected>0 
		BEGIN
			DECLARE @CurrentConceptKey CHAR(16)
			-- Update all the concepts to check if they are current
			DECLARE curConcepts CURSOR LOCAL FAST_FORWARD FOR
				SELECT	C.Concept_Key
				FROM	Concept AS C
				WHERE	Concept_Group_Key = @ConceptGroupKey
		
			OPEN curConcepts
		
			FETCH NEXT
			FROM	curConcepts
			INTO	@CurrentConceptKey
		
			WHILE @@Fetch_Status = 0
			BEGIN
				EXEC usp_Concept_UpdateIsCurrent @CurrentConceptKey, @ConceptGroupKey
				IF @@Error <> 0 GOTO RollbackAndExit
		
				FETCH NEXT
				FROM	curConcepts
				INTO	@CurrentConceptKey
			END
			CLOSE	curConcepts
			DEALLOCATE curConcepts
		END
	
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupVersion_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupVersion_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupVersion_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroupVersion_Insert]
GO

/*===========================================================================*\
  Description:	Adds a record to the ConceptGroupVersion table.

  Parameters:	@Key (output),	
		@ConceptGroupKey 
		@Version 
		@FromVagueDateStart (optional)
		@FromVagueDateEnd (optional)
		@FromVagueDateType (optional)
		@ToVagueDateStart (optional)
		@ToVagueDateEnd (optional)
		@ToVagueDateType (optional)
		@AcqVagueDateStart (optional)
		@AcqVagueDateEnd (optional)
		@AcqVagueDateType (optional)
		@URL (optional)
		@SessionID
		@SystemSuppliedData bit (optional)

  Created:	November 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_ConceptGroupVersion_Insert]
	@Key char(16) OUTPUT,	
	@ConceptGroupKey char(16),
	@Version varchar(100),
	@FromVagueDateStart int = NULL,
	@FromVagueDateEnd int = NULL,
	@FromVagueDateType varchar(2) = NULL,
	@ToVagueDateStart int = NULL,
	@ToVagueDateEnd int = NULL,
	@ToVagueDateType varchar(2) = NULL,	
	@AcqVagueDateStart int = NULL,
	@AcqVagueDateEnd int = NULL,
	@AcqVagueDateType varchar(2) = NULL,
	@URL varchar(255) = NULL,
	@SessionID char(16),
	@SystemSuppliedData bit = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @SequenceNumber int

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Calculate a new sequence number for the concept group version
		  record that is to be added.
		\*-------------------------------------------------------------*/		
		SELECT 	@SequenceNumber = Max([Sequence]) + 1
		FROM	Concept_Group_Version
		WHERE	Concept_Group_Key = @ConceptGroupKey

		-- If Null is returned, the sequence number should be 1
		SET @SequenceNumber = IsNull(@SequenceNumber, 1)

		/*-------------------------------------------------------------*\
		  Insert in Concept_Group_Version.
		\*-------------------------------------------------------------*/
		EXECUTE spNextKey 'Concept_Group_Version', @Key OUTPUT		

		INSERT INTO Concept_Group_Version (
			Concept_Group_Version_Key, 
			Concept_Group_Key, 
			Version,
			[Sequence], 
			From_Vague_Date_Start,
			From_Vague_Date_End,
			From_Vague_Date_Type,
			To_Vague_Date_Start,
			To_Vague_Date_End,
			To_Vague_Date_Type,	
			Acq_Vague_Date_Start,
			Acq_Vague_Date_End,
			Acq_Vague_Date_Type,	
			URL,
			Entered_Session_ID,
			System_Supplied_Data
		) VALUES (
			@Key,
			@ConceptGroupKey,
			@Version,
			@SequenceNumber,
			@FromVagueDateStart,
			@FromVagueDateEnd,
			IsNull(@FromVagueDateType, 'U'),
			@ToVagueDateStart,
			@ToVagueDateEnd,
			@ToVagueDateType,	
			@AcqVagueDateStart,
			@AcqVagueDateEnd,
			IsNull(@AcqVagueDateType, 'U'),	
			@URL,
			@SessionID,
			IsNull(@SystemSuppliedData, 0)
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*------------------------------------------------------------------*\
		  When a concept group version is added to a concept group for the 
			first time, all of the existing concepts should have a version 
			history record added for the new version. See IR 4918
		\*------------------------------------------------------------------*/
		IF @SequenceNumber=1
		BEGIN
			DECLARE @CurrentConceptKey char(16),
				@NewConceptHistoryKey char(16)
		
			-- Get all the Concepts in the Concept Group
			DECLARE curConcepts CURSOR LOCAL FAST_FORWARD FOR
				SELECT	C.Concept_Key
				FROM	Concept AS C
				WHERE	Concept_Group_Key = @ConceptGroupKey
		
			OPEN curConcepts
		
			FETCH NEXT
			FROM	curConcepts
			INTO	@CurrentConceptKey
		
			WHILE @@Fetch_Status = 0
			BEGIN
				IF NOT EXISTS (SELECT 1 FROM Concept_History WHERE Concept_Key=@CurrentConceptKey)
					EXEC usp_ConceptHistory_Insert 
						@Key = 'Unwanted Key',
						@ConceptKey = @CurrentConceptKey,
						@ConceptGroupVersionFromKey = @Key,
						@SessionID = @SessionID
		
				IF @@Error <> 0 GOTO RollbackAndExit
		
				FETCH NEXT
				FROM	curConcepts
				INTO	@CurrentConceptKey
			END
			CLOSE	curConcepts
			DEALLOCATE curConcepts
		END
		ELSE BEGIN
			/*------------------------------------------------------------------*\
			  When adding further concept group versions, make sure all old 
				concepts now expire if they were set to expire in the last version
			\*------------------------------------------------------------------*/
			UPDATE C
			SET Is_Current=0
			FROM Concept C
			INNER  JOIN Concept_History	CH ON CH.Concept_Key=C.Concept_Key
			INNER JOIN Concept_Group_Version CGVExpire ON CGVExpire.Concept_Group_Version_Key=CH.Concept_Group_Version_To
				AND CGVExpire.Sequence = @SequenceNumber-1
			WHERE C.Concept_Group_Key=@ConceptGroupKey
		END
		
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupVersion_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupVersion_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupVersion_Select_ForDomain]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroupVersion_Select_ForDomain]
GO

/*===========================================================================*\
  Description:	Retrieves a list of all versions of concept groups for a supplied domain.
		Includes the version in the caption and the date information.

  Parameters:	@Domain - key of the domain

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupVersion_Select_ForDomain]
	@Domain char(16)
AS

SELECT CGV.Concept_Group_Version_Key as Item_Key, CG.Item_Name + ' - ' + CGV.Version as Item_Name, 
CGV.From_Vague_Date_Start, CGV.From_Vague_Date_End, CGV.From_Vague_Date_Type,
CGV.To_Vague_Date_Start, CGV.To_Vague_Date_End, CGV.To_Vague_Date_Type,
CG.Hierarchy_Relation_Type_Key
FROM Local_Domain LD
INNER JOIN Concept_Group CG ON CG.Local_Domain_Key=LD.Local_Domain_Key
INNER JOIN Concept_Group_Version CGV on CGV.Concept_Group_Key=CG.Concept_Group_Key
WHERE LD.Domain_Key=@Domain
ORDER BY CGV.Sequence, CG.Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupVersion_Select_ForDomain') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupVersion_Select_ForDomain'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Select_ForDomain TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Select_ForDomain TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Select_ForDomain TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Select_ForDomain TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Select_ForDomain TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Select_ForDomain TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptHistory_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptHistory_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Concept_History table.

  Parameters:	@Key
		@Timestamp,
		@RecordsAffected

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptHistory_Delete]
	@Key char(16),
	@Timestamp timestamp,
	@RecordsAffected INT OUTPUT
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DECLARE @ConceptGroupKey CHAR(16)
		DECLARE @ConceptKey CHAR(16)

		-- Find the concept group key, because we need this to check if the concept 
		-- remains current
		SELECT @ConceptGroupKey=CGV.Concept_Group_Key, @ConceptKey=CH.Concept_Key
		FROM Concept_History CH
		INNER JOIN Concept_Group_Version CGV ON CGV.Concept_Group_Version_Key=CH.Concept_Group_Version_To
		WHERE CH.Concept_History_Key=@Key

		-- Delete record from Concept_History table.
		DELETE	Concept_History
		WHERE	Concept_History_Key = @Key
		AND	(@Timestamp = Timestamp)

		IF @@Error <> 0 GOTO RollbackAndExit

		SET @RecordsAffected=@@ROWCOUNT
		
		IF @RecordsAffected>0
		BEGIN
			--Ensure Concept's current status is set correctly.
			EXEC usp_Concept_UpdateIsCurrent @ConceptKey, @ConceptGroupKey
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptHistory_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptHistory_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptHistory_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptHistory_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Concept_History table

  Parameters:	@Key
		@ConservationCheckKey 
		@ConservationJobKey 
		@SetVagueDateStart
		@SetVagueDateEnd
		@SetVagueDateType
		@Status
		@TypeConceptKey
		@Priority
		@Duration
		@DurationUnitConceptKey
		@IdentifierNameKey
		@TaskAction 
		@Comment
		@SessionID
	

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptHistory_Insert]
	@Key char(16) OUTPUT,
	@ConceptKey char(16),
	@ConceptGroupVersionFromKey char(16) = NULL,
	@ConceptGroupVersionToKey char(16) = NULL,
	@FromVagueDateStart int = NULL,
	@FromVagueDateEnd int = NULL,
	@FromVagueDateType varchar(2) = NULL,
	@ToVagueDateStart int = NULL,
	@ToVagueDateEnd int = NULL,
	@ToVagueDateType varchar(2) = NULL,
	@SessionID char(16),
	@SystemSuppliedData bit = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Concept_History', @Key OUTPUT

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Concept_History.
		\*-------------------------------------------------------------*/
		INSERT INTO Concept_History (
			Concept_History_Key,
			Concept_Key,
			Concept_Group_Version_From,
			Concept_Group_Version_To,
			From_Vague_Date_Start,
			From_Vague_Date_End,
			From_Vague_Date_Type,
			To_Vague_Date_Start,
			To_Vague_Date_End,
			To_Vague_Date_Type,
			Entered_Session_ID,
			System_Supplied_Data
		) VALUES (
			@Key, 
			@ConceptKey,
			@ConceptGroupVersionFromKey,
			@ConceptGroupVersionToKey,
			@FromVagueDateStart,
			@FromVagueDateEnd,
			@FromVagueDateType,
			@ToVagueDateStart,
			@ToVagueDateEnd,
			@ToVagueDateType,
			@SessionID,
			IsNull(@SystemSuppliedData, 0)
		)	
		IF @@Error <> 0 GOTO RollbackAndExit

	--Ensure Concept's current status is set correctly.
		EXEC usp_Concept_UpdateIsCurrent @ConceptKey
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptHistory_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptHistory_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptHistory_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptHistory_Select]
GO

/*===========================================================================*\
  Description:	Returns data from the Concept_History table

  Parameters:	@Key	Concept_History_Key

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptHistory_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT 	
			CH.From_Vague_Date_Start,
			CH.From_Vague_Date_End,
			CH.From_Vague_Date_Type,
			CH.Concept_Group_Version_From AS Concept_Group_Version_From_Key,
			FromCGV.Version + ' (' + 
				IsNull(dbo.ufn_GetDateFromVagueDate(FromCGV.From_Vague_Date_Start, FromCGV.From_Vague_Date_End, FromCGV.From_Vague_Date_Type), '') +
				IsNull(' - ' + dbo.ufn_GetDateFromVagueDate(FromCGV.To_Vague_Date_Start, FromCGV.To_Vague_Date_End, FromCGV.To_Vague_Date_Type), '') + ')' AS Concept_Group_Version_From_Name,
			CH.To_Vague_Date_Start,
			CH.To_Vague_Date_End,
			CH.To_Vague_Date_Type,
			CH.Concept_Group_Version_To AS Concept_Group_Version_To_Key,
			ToCGV.Version + ' (' + 
				IsNull(dbo.ufn_GetDateFromVagueDate(ToCGV.From_Vague_Date_Start, ToCGV.From_Vague_Date_End, ToCGV.From_Vague_Date_Type), '') +
				IsNull(' - ' + dbo.ufn_GetDateFromVagueDate(ToCGV.To_Vague_Date_Start, ToCGV.To_Vague_Date_End, ToCGV.To_Vague_Date_Type), '') + ')' AS Concept_Group_Version_To_Name,
			CH.[Timestamp]
	FROM		Concept_History AS CH
	LEFT JOIN	Concept_Group_Version AS FromCGV ON FromCGV.Concept_Group_Version_Key = CH.Concept_Group_Version_From
	LEFT JOIN	Concept_Group_Version AS ToCGV ON ToCGV.Concept_Group_Version_Key = CH.Concept_Group_Version_To
	WHERE 		Concept_History_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptHistory_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptHistory_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptHistory_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptHistory_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Returns Concept_History records for a given concept key.

  Parameters:	@Key	Concept_Key

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptHistory_Select_ForConcept]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT
			CH.Concept_History_Key AS Item_Key,
			IsNull(
 				IsNull(CGV1.Version, '') + 
				IsNull(' (' + dbo.ufn_GetDateFromVagueDate(
						CH.From_Vague_Date_Start, CH.From_Vague_Date_End, CH.From_Vague_Date_Type) + ')', '') + ' -> ', '') 
			+ 
			IsNull(CGV2.Version, '') + 
			IsNull(' (' + dbo.ufn_GetDateFromVagueDate(
					CH.To_Vague_Date_Start, CH.To_Vague_Date_End, CH.To_Vague_Date_Type) + ') ', '')
			AS Item_Name
	FROM		Concept_History AS CH	
	LEFT JOIN	Concept_Group_Version AS CGV1 ON CGV1.Concept_Group_Version_Key = CH.Concept_Group_Version_From
	LEFT JOIN	Concept_Group_Version AS CGV2 ON CGV2.Concept_Group_Version_Key = CH.Concept_Group_Version_To
	WHERE		CH.Concept_Key = @Key
	ORDER BY	CH.From_Vague_Date_Start, CH.From_Vague_Date_End

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptHistory_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptHistory_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Select_ForConcept TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptHistory_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptHistory_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Concept_History table

  Parameters:	@Key
		@ConceptKey 
		@ConceptGroupVersionFromKey
		@ConceptGroupVersionToKey 
		@FromVagueDateStart
		@FromVagueDateEnd 
		@FromVagueDateType
		@ToVagueDateStart 
		@ToVagueDateEnd
		@ToVagueDateType
		@SessionID 
		@Timestamp 

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptHistory_Update]
	@Key char(16),
	@ConceptKey char(16),
	@ConceptGroupVersionFromKey char(16),
	@ConceptGroupVersionToKey char(16),
	@FromVagueDateStart int,
	@FromVagueDateEnd int,
	@FromVagueDateType varchar(2),
	@ToVagueDateStart int,
	@ToVagueDateEnd int,
	@ToVagueDateType varchar(2),
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Concept_History
		SET 	Concept_Key = @ConceptKey,
			Concept_Group_Version_From = @ConceptGroupVersionFromKey,
			Concept_Group_Version_To = @ConceptGroupVersionToKey,
			From_Vague_Date_Start = @FromVagueDateStart,
			From_Vague_Date_End = @FromVagueDateEnd,
			From_Vague_Date_Type = @FromVagueDateType,
			To_Vague_Date_Start = @ToVagueDateStart,
			To_Vague_Date_End = @ToVagueDateEnd,
			To_Vague_Date_Type = @ToVagueDateType,
			Changed_Session_ID = @SessionID
		WHERE	Concept_History_Key = @Key
		AND	(@Timestamp = Timestamp)

		IF @@Error <> 0 GOTO RollbackAndExit

		--Ensure Concept's current status is set correctly.
		EXEC usp_Concept_UpdateIsCurrent @ConceptKey
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptHistory_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptHistory_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_CreateSubtree]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_CreateSubtree]
GO

/*===========================================================================*\
  Description:	Create lineage records beneath the given parent lineage for
				the specified concept and its descendants.

				If the concept is not list preferred or the group has no
				hierarchical relation then no lineage records are created. 

  Parameters:	@concept_key			Concept key
				@parent_lineage			Parent lineage
				@job_id					[optional] Import job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 3 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_CreateSubtree]
	@concept_key		CHAR(16),
	@parent_lineage		VARCHAR(900),
	@job_id				INT				=	NULL
AS
	SET NOCOUNT ON

	DECLARE     @list_preferred			BIT,
				@concept_group_key		CHAR(16),
				@relation_type_key		CHAR(16),
				@lineage				VARCHAR(900),
				@child_concept_key		CHAR(16)

	SELECT      @list_preferred			=	c.List_Preferred,
				@concept_group_key		=	c.Concept_Group_Key,
				@relation_type_key		=	g.Hierarchy_Relation_Type_Key
	FROM		Concept					AS	c
	INNER JOIN	Concept_Group			AS	g
	ON			g.Concept_Group_Key		=	c.Concept_Group_Key
	WHERE		c.Concept_Key			=	@concept_key

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Concept does not exist', 16, 1)
		RETURN
	END

	IF @list_preferred = 1 AND @relation_type_key IS NOT NULL
	BEGIN
		IF @job_id IS NULL BEGIN TRANSACTION

		DECLARE @pending_lineage TABLE	(
						Concept_Key		CHAR(16)		PRIMARY KEY,
						Parent_Lineage  VARCHAR(900)	NULL)

		INSERT		@pending_lineage (
					Concept_Key,
					Parent_Lineage)
		VALUES		(@concept_key,
					@parent_lineage)

		WHILE @concept_key IS NOT NULL
		BEGIN
			IF @job_id IS NOT NULL BEGIN TRANSACTION

			/* create lineage for current concept */
			SET			@lineage		=	dbo.ufn_NextChildLineage(
													@parent_lineage,
													@concept_group_key)

			INSERT		Concept_Lineage (
						Concept_Key,
						Lineage_ID,
						Lineage)
			SELECT		@concept_key,
						ISNULL(MAX(Lineage_ID), 0) + 1,
						@lineage
			FROM		Concept_Lineage
			WHERE		Concept_Key			=	@concept_key

			IF @@ERROR <> 0 GOTO fail

			/* record lineage sequence number */
			IF @parent_lineage IS NULL
			BEGIN
				UPDATE		Concept_Group
				SET			Last_Sequence_Number	=	@lineage
				WHERE		Concept_Group_Key		=	@concept_group_key

				IF @@ERROR <> 0 GOTO fail
			END
			ELSE
			BEGIN
				UPDATE		Concept_Lineage
				SET			Last_Sequence_Number	=	dbo.ufn_LineageSequenceNumber(@lineage)
				FROM		Concept_Lineage			AS	l
				INNER JOIN	Concept					AS	c
				ON			c.Concept_Key			=	l.Concept_Key
				WHERE		l.Lineage				=	@parent_lineage
				AND			c.Concept_Group_Key		=	@concept_group_key

				IF @@ERROR <> 0 GOTO fail
			END

			/* remove current concept from pending list */
			DELETE		@pending_lineage
			WHERE		Concept_Key			=	@concept_key

			IF @@ERROR <> 0 GOTO fail

			/* add offspring of current concept to pending list */
			INSERT		@pending_lineage (
						Concept_Key,
						Parent_Lineage)
			SELECT		r.To_Concept_Key,
						@lineage
			FROM		Concept_Relation				AS	r
			INNER JOIN	Concept							AS	c
			ON			c.Concept_Key					=	r.To_Concept_Key
			WHERE       r.From_Concept_Key				=	@concept_key
			AND			r.Thesaurus_Relation_Type_Key	=	@relation_type_key
			AND			c.Concept_Group_Key				=	@concept_group_key
			AND			c.List_Preferred				=	1

			IF @@ERROR <> 0 GOTO fail

			/* select the next concept (if any) for processing */
			SET ROWCOUNT 1

			SELECT		@concept_key		=	Concept_Key,
						@parent_lineage		=	Parent_Lineage
			FROM		@pending_lineage

			IF @@ROWCOUNT = 0
				SET			@concept_key		=	NULL

			SET ROWCOUNT 0

			IF @job_id IS NOT NULL
			BEGIN
				EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
				IF @@ERROR <> 0 GOTO fail

				COMMIT TRANSACTION
			END
		END  /* WHILE @concept_key IS NOT NULL */

		IF @job_id IS NULL COMMIT TRANSACTION
	END /* IF @list_preferred = 1 AND ... */
	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptLineage_CreateSubtree failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
/* TODO: correct permissions */
GRANT EXECUTE ON [dbo].[usp_ConceptLineage_CreateSubtree] TO [Public]
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_GenerateForGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_GenerateForGroup]
GO

/*===========================================================================*\
  Description:	Generate (or re-generate) concept lineage for a specified
				concept group.

				Either @job_id or @concept_group_key must be supplied.

  Parameters:	@job_id					Job identifier
				@concept_group_key		Concept group key

  Created:		Dec 2003

  Last revision information:
	$Revision: 3 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_GenerateForGroup]
	@job_id				INT			=	NULL,
	@concept_group_key	CHAR(16)	=	NULL
AS
	SET NOCOUNT ON

	DECLARE     @hierarchy_relation_type_key	CHAR(16)

	IF @job_id IS NULL
	BEGIN
		IF @concept_group_key IS NULL
		BEGIN
			RAISERROR ('Concept group or Job must be specified', 16, 1)
			RETURN
		END
	END
	ELSE
	BEGIN
		/* determine parameters of job */
		SELECT		@concept_group_key		=	Concept_Group_Key
		FROM		Import_Export_Job
		WHERE		Import_Export_Job_ID	=	@job_id

		IF @@ROWCOUNT = 0
		BEGIN
			RAISERROR ('Job does not exist or has not been configured', 16, 1)
			RETURN
		END

		EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
														'Generating lineage'
		IF @@ERROR <> 0 RETURN
	END

	/* determine whether the group has a hierarchical relation */
	SELECT		@hierarchy_relation_type_key	=	Hierarchy_Relation_Type_Key
	FROM		Concept_Group
	WHERE		Concept_Group_Key				=	@concept_group_key

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Concept group does not exist', 16, 1)
		RETURN
	END

	/* remove existing concept lineage records */
	EXECUTE		usp_ConceptLineage_DeleteSubtree	@concept_group_key,
													NULL
	IF @@ERROR <> 0 RETURN

	IF @hierarchy_relation_type_key IS NOT NULL
	BEGIN
		DECLARE		@concept_key	CHAR(16)

		/* create/update concept lineage */
		DECLARE		root_concepts			CURSOR LOCAL STATIC FOR
		SELECT		c.Concept_Key
		FROM		Concept					AS	c
		WHERE		c.Concept_Group_Key		=	@concept_group_key
		AND			c.List_Preferred		=	1
		AND			NOT EXISTS (
							SELECT		1
							FROM		Concept_Relation				AS	r
							INNER JOIN	Concept							AS	p
							ON			p.Concept_Key					=	r.From_Concept_Key
							WHERE		r.To_Concept_Key				=	c.Concept_Key
							AND			r.Thesaurus_Relation_Type_Key	=	@hierarchy_relation_type_key
							AND			p.Concept_Group_Key				=	@concept_group_key
							AND			p.List_Preferred				=	1)

		OPEN		root_concepts

		WHILE 1 = 1
		BEGIN
			FETCH		root_concepts
			INTO		@concept_key

			IF @@FETCH_STATUS <> 0 BREAK

			EXECUTE		usp_ConceptLineage_CreateSubtree	@concept_key,
															NULL,
															@job_id
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		CLOSE		root_concepts
	END
	RETURN

fail_from_cursor:
	CLOSE		root_concepts

fail:
	RAISERROR ('usp_ConceptLineage_GenerateForGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
/* TODO: correct permissions */
GRANT EXECUTE ON [dbo].[usp_ConceptLineage_GenerateForGroup] TO [Public]
GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRelations_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRelations_Select]
GO

/*===========================================================================*\
  Description:	Retrieves a list of all the related concepts for a concept.
	              Includes concept and meaning relations, synonyms and term versions.

  Parameters:	@ConceptKey	
 	            @IncludeSynonyms 

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRelations_Select]
	@ConceptKey char(16),
  @IncludeSynonyms bit
AS

SET NOCOUNT ON

--Create a temp table to store the output
DECLARE @Output TABLE (
  Relationship_Name varchar(100),
  Concept_Key char(16),
  Plaintext nvarchar(150),
  Item_Name nvarchar(150),
  Author_And_Date varchar(100),
  Concept_Group_Name varchar(100),
  Details text
)   

--Create a temp table to store the concept keys we are scanning
DECLARE @ConceptKeys TABLE (
  Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
	Ancestor BIT   -- is this a concept in the lineage above the selected concept?
)

--Find the current concept group.  The output only lists the concept group if different to the current item
DECLARE @ConceptGroupKey char(16)
SELECT @ConceptGroupKey=Concept_Group_Key FROM Concept WHERE Concept_Key=@ConceptKey

--Store the appropriate concept key
INSERT INTO @ConceptKeys (Concept_Key, Ancestor) 
VALUES (@ConceptKey, 0)

/*===========================================================================*\	
	Retrieve the list of lineage concept keys that we need to look at for 
	inherited relationships
\*===========================================================================*/
	DECLARE @LineageID integer
	DECLARE @Lineage varchar(2000)
	DECLARE @CharPos integer

	--First find all the parent lineages
  DECLARE Lineages_Cursor CURSOR LOCAL FORWARD_ONLY FOR
    SELECT Lineage_ID, Lineage FROM Concept_Lineage WHERE Concept_Key=@ConceptKey

	OPEN Lineages_Cursor

  FETCH NEXT FROM Lineages_Cursor INTO @LineageID, @Lineage
  
  WHILE @@FETCH_STATUS=0
	BEGIN
		
		SET @CharPos=1
		
		--Find each ancestor, start at top of tree and work down
		WHILE @CharPos<LEN(@Lineage)
		BEGIN
		  IF SUBSTRING(@Lineage, @CharPos, 1)='\'
			  INSERT INTO @ConceptKeys (Concept_Key, Ancestor)
			    SELECT DISTINCT C.Concept_Key, 1
					FROM Concept C
		      INNER JOIN Concept_Lineage CL ON CL.Concept_Key=C.Concept_Key
					LEFT JOIN @ConceptKeys CK ON CK.Concept_Key=C.Concept_Key
				  WHERE C.Concept_Group_Key=@ConceptGroupKey
			    AND CL.Lineage=Left(@Lineage, @CharPos-1)
					AND CK.Concept_Key IS NULL
		  SET @CharPos=@CharPos+1
		END

    FETCH NEXT FROM Lineages_Cursor INTO @LineageID, @Lineage

	END

	CLOSE Lineages_Cursor
	DEALLOCATE Lineages_Cursor

--Now get the synonyms we need to scan for
IF @IncludeSynonyms=1
  INSERT INTO @ConceptKeys (Concept_Key, Ancestor)
      SELECT C2.Concept_Key, CK1.Ancestor
      FROM @ConceptKeys CK1
			INNER JOIN Concept C1 ON C1.Concept_Key=CK1.Concept_Key
      INNER JOIN Concept C2 ON C2.Meaning_Key=C1.Meaning_Key
			-- Join to ensure no duplicates
			LEFT JOIN @ConceptKeys CK2 ON CK2.Concept_Key=C2.Concept_Key
			WHERE CK2.Concept_Key IS NULL

--Insert forward terms for concept relations.  The odd nesting of queries
--allows distinct to be used before the text field (comment) is obtained)
--Note that concept relations cannot be inherited.
INSERT INTO @Output (Relationship_Name, Concept_Key, Plaintext, 
      Item_Name, Author_And_Date, Concept_Group_Name, Details)
	SELECT 
		Data.Forward_Term, 
		Data.Concept_Key, 
		Data.Plaintext, 
		Data.Item_Name, 
		Data.Author_Copy, 
		Data.Concept_Group_Name,
		CR.Comment
	FROM (
		SELECT DISTINCT CR.Concept_Relation_Key, TRT.Forward_Term, C.Concept_Key, T.Plaintext, 
	      T.Item_Name, C.Author_Copy, 
	      CASE WHEN CG.Concept_Group_Key=@ConceptGroupKey THEN NULL ELSE CG.Item_Name END AS Concept_Group_Name
		FROM Concept_Relation CR
	    	INNER JOIN Concept C ON C.Concept_Key=CR.To_Concept_Key
	    	INNER JOIN Term T ON T.Term_Key=C.Term_Key
				INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=CR.Thesaurus_Relation_Type_Key
			  INNER JOIN Concept_Group CG ON CG.Concept_Group_Key=C.Concept_Group_Key
				INNER JOIN @ConceptKeys CK ON CK.Concept_Key=CR.From_Concept_Key
						AND (CK.Ancestor=0 OR CR.Inherited=1)
	) Data INNER JOIN Concept_Relation CR ON CR.Concept_Relation_Key=Data.Concept_Relation_Key
  ORDER BY PlainText, Author_Copy


--Insert reverse terms for concept relations
INSERT INTO @Output (Relationship_Name, Concept_Key, Plaintext, 
      Item_Name, Author_And_Date, Concept_Group_Name, Details)
	SELECT 
		Data.Reverse_Term, 
		Data.Concept_Key, 
		Data.Plaintext, 
		Data.Item_Name, 
		Data.Author_Copy, 
		Data.Concept_Group_Name,
		CR.Comment
	FROM (
		SELECT DISTINCT CR.Concept_Relation_Key, TRT.Reverse_Term, C.Concept_Key, T.Plaintext, 
	      T.Item_Name, C.Author_Copy, 
	      CASE WHEN CG.Concept_Group_Key=@ConceptGroupKey THEN NULL ELSE CG.Item_Name END AS Concept_Group_Name
		FROM Concept_Relation CR
	    	INNER JOIN Concept C ON C.Concept_Key=CR.From_Concept_Key
	    	INNER JOIN Term T ON T.Term_Key=C.Term_Key
				INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=CR.Thesaurus_Relation_Type_Key
			  INNER JOIN Concept_Group CG ON CG.Concept_Group_Key=C.Concept_Group_Key
				INNER JOIN @ConceptKeys CK ON CK.Concept_Key=CR.To_Concept_Key
						AND (CK.Ancestor=0 OR CR.Inherited=1)
	) Data INNER JOIN Concept_Relation CR ON CR.Concept_Relation_Key=Data.Concept_Relation_Key
  ORDER BY PlainText, Author_Copy


--Insert forward terms for meaning relations
INSERT INTO @Output (Relationship_Name, Concept_Key, Plaintext, 
      Item_Name, Author_And_Date, Concept_Group_Name, Details)
	SELECT 
		Data.Forward_Term, 
		Data.Concept_Key, 
		Data.Plaintext, 
		Data.Item_Name, 
		Data.Author_Copy, 
		Data.Concept_Group_Name,
		MR.Comment
	FROM (
		SELECT DISTINCT MR.Meaning_Relation_Key, TRT.Forward_Term, C2.Concept_Key, T.Plaintext, 
	      T.Item_Name, C2.Author_Copy, 
	      CASE WHEN CG.Concept_Group_Key=@ConceptGroupKey THEN NULL ELSE CG.Item_Name END AS Concept_Group_Name
		FROM Concept C1
	      INNER JOIN Meaning_Relation MR ON MR.From_Meaning_Key=C1.Meaning_Key
				INNER JOIN Concept C2 ON C2.Meaning_Key=MR.To_Meaning_Key
					AND C2.List_Preferred=1
	    	INNER JOIN Term T ON T.Term_Key=C2.Term_Key
				INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=MR.Thesaurus_Relation_Type_Key
			  INNER JOIN Concept_Group CG ON CG.Concept_Group_Key=C2.Concept_Group_Key
				INNER JOIN @ConceptKeys CK ON CK.Concept_Key=C1.Concept_Key
						AND (CK.Ancestor=0 OR MR.Inherited=1)
	) Data INNER JOIN Meaning_Relation MR ON MR.Meaning_Relation_Key=Data.Meaning_Relation_Key
  ORDER BY Plaintext, Author_Copy


--Insert reverse terms for meaning relations
INSERT INTO @Output (Relationship_Name, Concept_Key, Plaintext, 
      Item_Name, Author_And_Date, Concept_Group_Name, Details)
	SELECT 
		Data.Reverse_Term, 
		Data.Concept_Key, 
		Data.Plaintext, 
		Data.Item_Name, 
		Data.Author_Copy, 
		Data.Concept_Group_Name,
		MR.Comment
	FROM (
		SELECT DISTINCT MR.Meaning_Relation_Key, TRT.Reverse_Term, C2.Concept_Key, T.Plaintext, 
	      T.Item_Name, C2.Author_Copy, 
	      CASE WHEN CG.Concept_Group_Key=@ConceptGroupKey THEN NULL ELSE CG.Item_Name END AS Concept_Group_Name
		FROM Concept C1
	      INNER JOIN Meaning_Relation MR ON MR.To_Meaning_Key=C1.Meaning_Key
				INNER JOIN Concept C2 ON C2.Meaning_Key=MR.From_Meaning_Key
					AND C2.List_Preferred=1
	    	INNER JOIN Term T ON T.Term_Key=C2.Term_Key
				INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=MR.Thesaurus_Relation_Type_Key
			  INNER JOIN Concept_Group CG ON CG.Concept_Group_Key=C2.Concept_Group_Key
				INNER JOIN @ConceptKeys CK ON CK.Concept_Key=C1.Concept_Key
						AND (CK.Ancestor=0 OR MR.Inherited=1)
	) Data INNER JOIN Meaning_Relation MR ON MR.Meaning_Relation_Key=Data.Meaning_Relation_Key
  ORDER BY Plaintext, Author_Copy


--Insert all known synonyms
INSERT INTO @Output (Relationship_Name, Concept_Key, Plaintext, 
      Item_Name, Author_And_Date, Concept_Group_Name)
	SELECT '#Synonyms', C2.Concept_Key, T.Plaintext, T.Item_Name, C2.Author_Copy, 
      CASE WHEN CG.Concept_Group_Key=@ConceptGroupKey THEN NULL ELSE CG.Item_Name END 
	FROM Concept C1
	    INNER JOIN Concept C2 ON C2.Meaning_Key=C1.Meaning_Key
			INNER JOIN Term T ON T.Term_Key=C2.Term_Key
			INNER JOIN Concept_Group CG on CG.Concept_Group_Key=C2.Concept_Group_Key
  WHERE C1.Concept_Key=@ConceptKey
  ORDER BY T.Plaintext, C2.Author_Copy


--Table to hold all term versions
DECLARE @TermVersions TABLE (
		Concept_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
		Term_Version_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
		Sort_Order INT
)

--Insert the selected concept into the version list
INSERT INTO @TermVersions(Concept_Key, Term_Version_Key, Sort_Order) 
		SELECT C.Concept_Key, C.Term_Version_Key, C.Sort_Code
		FROM Concept C
				INNER JOIN Term_Version TV ON TV.Term_Version_Key=C.Term_Version_Key
		WHERE C.Concept_Key=@ConceptKey

--Recursively find all the versions, first get the nearest set of versions
INSERT INTO @TermVersions(Concept_Key, Term_Version_Key, Sort_Order)
		SELECT TVR.From_Concept_Key, TVR.From_Term_Version_Key, TV.Sort_Order-1
		FROM @TermVersions TV
				INNER JOIN Term_Version_Relation TVR ON TVR.To_Term_Version_Key=TV.Term_Version_Key
				INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
				INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
						AND SR.Reverse_Equivalence_Possible=1
 				--Join to exclude items already in the list
				LEFT JOIN @TermVersions TVExist ON TVExist.Term_Version_Key=TVR.From_Term_Version_Key
		WHERE TVExist.Concept_Key IS NULL
		UNION
		SELECT TVR.To_Concept_Key, TVR.To_Term_Version_Key, TV.Sort_Order+1
		FROM @TermVersions TV
				INNER JOIN Term_Version_Relation TVR ON TVR.From_Term_Version_Key=TV.Term_Version_Key
				INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
				INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
						AND SR.Forward_Equivalence_Possible=1
 				--Join to exclude items already in the list
				LEFT JOIN @TermVersions TVExist ON TVExist.Term_Version_Key=TVR.To_Term_Version_Key
		WHERE TVExist.Concept_Key IS NULL

--Now recurse to find all other versions
WHILE @@ROWCOUNT<>0
	INSERT INTO @TermVersions(Concept_Key, Term_Version_Key, Sort_Order)
			SELECT TVR.From_Concept_Key, TVR.From_Term_Version_Key, TV.Sort_Order-1
			FROM @TermVersions TV
					INNER JOIN Term_Version_Relation TVR ON TVR.To_Term_Version_Key=TV.Term_Version_Key
					INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
					INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
							AND SR.Reverse_Equivalence_Possible=1
	 				--Join to exclude items already in the list
					LEFT JOIN @TermVersions TVExist ON TVExist.Term_Version_Key=TVR.From_Term_Version_Key
			WHERE TVExist.Concept_Key IS NULL
			UNION
			SELECT TVR.To_Concept_Key, TVR.To_Term_Version_Key, TV.Sort_Order+1
			FROM @TermVersions TV
					INNER JOIN Term_Version_Relation TVR ON TVR.From_Term_Version_Key=TV.Term_Version_Key
					INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
					INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
							AND SR.Forward_Equivalence_Possible=1
	 				--Join to exclude items already in the list
					LEFT JOIN @TermVersions TVExist ON TVExist.Term_Version_Key=TVR.To_Term_Version_Key
			WHERE TVExist.Concept_Key IS NULL

	
INSERT INTO @Output (Relationship_Name, Concept_Key, Plaintext, 
    Item_Name, Author_And_Date, Concept_Group_Name)
SELECT '#TermVersions', 
		C.Concept_Key,
		T.Plaintext,
		T.Item_Name,
		CASE WHEN TV.Version_Label IS NULL THEN 
			C.Author_Copy
		ELSE
			C.Author_Copy + ' (' + TV.Version_Label + ')'
		END,
    CASE WHEN CG.Concept_Group_Key=@ConceptGroupKey THEN NULL ELSE CG.Item_Name END 
FROM Concept C
		INNER JOIN Term T ON T.Term_Key=C.Term_Key
		INNER JOIN Concept_Group CG on CG.Concept_Group_Key=C.Concept_Group_Key
		INNER JOIN Term_Version TV on TV.Term_Version_Key=C.Term_Version_Key
		INNER JOIN @TermVersions TVLIST ON TVLIST.Concept_Key=C.Concept_Key
ORDER BY TVLIST.Sort_Order ASC


--Select the output
SELECT * FROM @Output

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRelations_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRelations_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRelations_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRelations_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRelations_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRelations_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRelations_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRelations_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptSimple_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptSimple_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the concept table.

  Parameters:	@Key
		@TermKey 
		@ConceptGroupKey 
		@Preferred
		@ConceptRankKey 
		@NameTypeConceptKey
		@SortCode 
		@ListCode 
		@SessionID 
		@RecordsAffected 
		@Timestamp 

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptSimple_Update]
	@Key char(16),
	@TermKey char(16),
	@ConceptGroupKey char(16),
	@ListPreferred bit = NULL,
	@Preferred bit,
	@ConceptRankKey char(16),
	@NameTypeConceptKey char(16) = NULL,
	@SortCode int,
	@ListCode varchar(50),
	@SessionID char(16),
	@RecordsAffected int OUTPUT,
	@Timestamp timestamp
AS
SET NOCOUNT OFF
	
	BEGIN TRANSACTION
		/*-------------------*\
		  Update the Concept.
		\*-------------------*/
		DECLARE @old_concept_group_key CHAR(16),
			@old_list_preferred BIT,
			@OldConceptRankKey char(16),
			@error INT
		
		UPDATE	Concept
		SET 	@old_concept_group_key = Concept_Group_Key,
			@old_list_preferred = List_Preferred,
			@OldConceptRankKey = Concept_Rank_Key,
			List_Preferred = IsNull(@ListPreferred, List_Preferred),
			Concept_Group_Key = @ConceptGroupKey,
			Term_Key = @TermKey,
			Concept_Rank_Key = @ConceptRankKey,
			Preferred = @Preferred,
			Name_Type_Concept_Key = @NameTypeConceptKey,
			Sort_Code = @SortCode,
			List_Code = @ListCode,
			Changed_Session_ID = @SessionID			
		WHERE	Concept_Key = @Key
		AND	(@Timestamp = Timestamp)

		SELECT		@error				=	@@ERROR,
					@RecordsAffected	=	@@ROWCOUNT

		IF @error <> 0 GOTO RollbackAndExit

		/*----------------------------------------------------------------------------*\
		  Make corresponding changes in Concept_Lineage
		\*----------------------------------------------------------------------------*/
		EXECUTE		usp_ConceptLineage_ConceptUpdated	@Key,
									@old_concept_group_key,
									@old_list_preferred
		IF @@ERROR <> 0 GOTO RollbackAndExit
		
		/*----------------------------------------------------------------------------*\
		  If @Preferred = 1, then make sure the updated concept is the 
		  only Preferred synonym with the same language key and name type concept key.
		\*----------------------------------------------------------------------------*/
		IF @Preferred = 1 
			UPDATE		CSynonyms
			SET		Preferred = 0
			FROM 		Concept AS CSource
			INNER JOIN	Term 	AS TSource 	ON TSource.Term_Key = CSource.Term_Key
			INNER JOIN	Concept AS CSynonyms 	ON CSynonyms.Meaning_Key = CSource.Meaning_Key
								AND CSynonyms.Name_Type_Concept_Key = CSource.Name_Type_Concept_Key
								AND CSynonyms.Concept_Key <> CSource.Concept_Key
			INNER JOIN	Term 	AS TSynonyms 	ON TSynonyms.Term_Key = CSynonyms.Term_Key
								AND TSynonyms.Language_Key = TSource.Language_Key
			WHERE		CSource.Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		/*----------------------------------------------------------------------------*\
		  If the Concept Rank Key has changed, then all synonyms of the changed 
		  Concept need to get this new Concept Rank Key.
		\*----------------------------------------------------------------------------*/
		IF (@ConceptRankKey <> @OldConceptRankKey) 
		OR ((@OldConceptRankKey IS NULL) AND (@ConceptRankKey IS NOT NULL))
		OR ((@OldConceptRankKey IS NOT NULL) AND (@ConceptRankKey IS NULL))
			UPDATE		CSynonyms
			SET		Concept_Rank_Key = @ConceptRankKey
			FROM		Concept AS CSource
			INNER JOIN	Concept AS CSynonyms ON CSynonyms.Meaning_Key = CSource.Meaning_Key
			WHERE		CSource.Concept_Key = @Key
		
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptSimple_Update failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSimple_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptSimple_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_ChooseListPreferred_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_ChooseListPreferred_Update]
GO

/*===========================================================================*\
  Description:	Takes a concept key from a concept that is about to stop
		being a synonym, selects all the other synonyms and makes the
		first one list preferred.

  Parameters:	@ConceptGroupKey - key of the concept group
		@ConceptKey - concept key

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_ChooseListPreferred_Update]
	@Key varchar(16)
AS
	DECLARE	@ConceptKeyMakePreferred char(16)

	SELECT TOP 1	@ConceptKeyMakePreferred = C1.Concept_Key
	FROM		Concept AS C1
	INNER JOIN 	Concept AS C2 ON C2.Meaning_Key = C1.Meaning_Key
	WHERE		C2.Concept_Key = @Key
	AND		C2.Concept_Key <> C1.Concept_Key
	ORDER BY	C1.Sort_Code

	IF @ConceptKeyMakePreferred IS NOT NULL 
	BEGIN
		UPDATE	Concept
		SET	List_Preferred = 1
		WHERE	Concept_Key = @ConceptKeyMakePreferred
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_ChooseListPreferred_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_ChooseListPreferred_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_ChooseListPreferred_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_ChooseListPreferred_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_ChooseListPreferred_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_ChooseListPreferred_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_ChooseListPreferred_Update TO [Dev - JNCC SQL]
END

GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a new concept and term if necessary.

  Parameters:	@Key	
		@ConceptGroupKey
		@TermName
		@PlainText 
		@LanguageKey 
		@SessionID 
		@NameTypeConceptKey 
		@IsSystem bit
			

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Concept_Insert]
	@Key char(16) OUTPUT,
	@ConceptGroupKey char(16),
	@TermName nvarchar(100),
	@PlainText nvarchar(100) = NULL,
	@LanguageKey varchar(4) = NULL,
	@SessionID char(16),
	@NameTypeConceptKey char(16) = NULL,
	@IsSystem bit = NULL
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF
	SET XACT_ABORT ON

	BEGIN TRANSACTION

	DECLARE @NewTermKey char(16), 
		@NewConceptHistoryKey char(16),
		@SiteID char(8),
		@ConceptGroupVersionKey char(16),
		@SystemSuppliedData bit,
		@NewMeaningKey char(16)

	IF @IsSystem = 1
	BEGIN
		SET @SiteID = 'SYSTEM00'
		SET @SystemSuppliedData = 1
	END
	ELSE
	BEGIN
		SELECT @SiteID = Data FROM Setting WHERE [Name] = 'SiteID'
		SET @SystemSuppliedData = 0
	END

	IF NOT EXISTS(SELECT * FROM Concept_Group WHERE Concept_Group_Key = @ConceptGroupKey)
	BEGIN
		RAISERROR('Invalid CONCEPT_GROUP_KEY, Concept Group does not exist', 16, 1)
		GOTO RollbackAndExit
	END
	ELSE
	BEGIN
		-- if no NameTypeConcept Key, use default one.
		IF @NameTypeConceptKey IS NULL SET @NameTypeConceptKey = 'SYSTEM00000000AN'

		-- if plaintext is null, set it equal to TermName.
		IF @PlainText IS NULL SET @PlainText = @TermName

		-- If Language not specified, get the one with Priority of 1.
		IF @LanguageKey IS NULL
			SELECT	@LanguageKey = Language_Key
			FROM	Language
			WHERE	Priority = 1

		/*-------------------------------------------------------------*\
		  Find out if new Term is required and create if needed.
		\*-------------------------------------------------------------*/
		SELECT 	@NewTermKey = Term_Key 
		FROM 	Term 
		WHERE 	Item_Name = @TermName
		AND 	Language_Key = @LanguageKey

		IF @NewTermKey IS NULL
		BEGIN
			EXEC spNextKey 'Term', @NewTermKey OUTPUT, @SiteID
			IF @@Error <> 0 GOTO RollbackAndExit
			INSERT INTO Term (
				Term_Key, Language_Key, Item_Name, Plaintext, Entered_Session_ID, 
				System_Supplied_Data
			) VALUES (
				@NewTermKey, @LanguageKey, @TermName, @PlainText, @SessionID, @SystemSuppliedData
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		  Create new Concept and Meaning.
		\*-------------------------------------------------------------*/
		EXEC spNextKey 'Concept', @Key OUTPUT, @SiteID
		IF @@Error <> 0 GOTO RollbackAndExit

		EXEC spNextKey 'Meaning', @NewMeaningKey OUTPUT, @SiteID
		IF @@Error <> 0 GOTO RollbackAndExit

		INSERT INTO [dbo].[Meaning] (Meaning_Key) VALUES (@NewMeaningKey)
		IF @@Error <> 0 GOTO RollbackAndExit

		-- Now insert the Concept.
		INSERT INTO Concept (
			Concept_Key, Term_Key, Concept_Group_Key, List_Preferred, Preferred, Is_Current, 
			Name_Type_Concept_Key, Meaning_Key, Entered_Session_ID, System_Supplied_Data
		) VALUES (
			@Key, @NewTermKey, @ConceptGroupKey, 1, 1, 1, 
			@NameTypeConceptKey, @NewMeaningKey, @SessionID, @SystemSuppliedData
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Create new Concept_History.
		\*-------------------------------------------------------------*/
		EXEC spNextKey 'Concept_History', @NewConceptHistoryKey OUTPUT, @SiteID
		IF @@Error <> 0 GOTO RollbackAndExit
		INSERT INTO Concept_History (
			Concept_History_Key, Concept_Key, Concept_Group_Version_From,
			Entered_Session_ID, System_Supplied_Data
		)
			SELECT TOP 1 	@NewConceptHistoryKey, @Key, Concept_Group_Version_Key ,
					@SessionID, @SystemSuppliedData
			FROM 		Concept_Group_Version
			WHERE 		Concept_Group_Key = @ConceptGroupKey
			ORDER BY 	From_Vague_Date_Start DESC
		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Create Concept_Lineage
		\*-------------------------------------------------------------*/
		EXECUTE		usp_ConceptLineage_NewConcept	@Key
		IF @@ERROR <> 0 GOTO RollbackAndExit
	END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_Insert failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Insert TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Concept_RecursionCheck_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Concept_RecursionCheck_Get]
GO

CREATE PROCEDURE [dbo].[usp_Concept_RecursionCheck_Get] 
@PotentialChildKey CHAR(16),
@PotentialParentKey CHAR(16),
@RecursionExists BIT OUTPUT
AS

/*===========================================================================*\
  Description:	Checks that the user isn't trying to create a circular
		Concept_Relation.

  Parameters:	@PotentialChildKey - key of dragged node.
		@PotentialParentKey - key of target node.
		@RecursionExists - if recursion exists (i.e. a problem) return 1
				   else return 0 (i.e. OK)

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

SET NOCOUNT ON

DECLARE @ParentConcepts TABLE
(
	[Concept_Key] [char] (16) COLLATE database_default NOT NULL
)

DECLARE @ParentConceptKey CHAR(16)

-- Check the user hasn't dragged and dropped on to the same node.
IF @PotentialParentKey = @PotentialChildKey 
	SET @RecursionExists = 1
ELSE
-- So long as they haven't, set up local variables so 'while' loop will start.
BEGIN
	SET 	@ParentConceptKey = @PotentialParentKey
	SET 	@RecursionExists = 0
END

--Obtain successive parents
WHILE @ParentConceptKey IS NOT NULL
BEGIN
	SELECT 	@ParentConceptKey = From_Concept_Key
	FROM	Concept_Relation
	WHERE	To_Concept_Key = @ParentConceptKey

	--At top of Concept hierarchy. No match found.
	IF (@ParentConceptKey IS NULL) OR (@@RowCount = 0)
	BEGIN
		SET @RecursionExists = 0
		BREAK
	END

	--Match found.
	ELSE IF @ParentConceptKey = @PotentialChildKey
	BEGIN
		SET @RecursionExists = 1
		BREAK
	END

	--Recursive Store hierarchy found.
	ELSE IF EXISTS(	SELECT 	* 
			FROM 	@ParentConcepts 
			WHERE 	Concept_Key = @ParentConceptKey)
	BEGIN 
		SET @RecursionExists = 1
		BREAK
	END

	--Log current Store 
	ELSE 
		INSERT INTO @ParentConcepts (Concept_Key) VALUES (@ParentConceptKey)
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_RecursionCheck_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_RecursionCheck_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForConceptGroupSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupSearch]
GO

/*===========================================================================*\
  Description:	Retrieves a list of concepts that match a search string, in a 
 								specified concept group.

  Parameters:	@ConceptGroup - key of the concept group
							@SearchText - search text

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupSearch]
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
WHERE Concept_Group_Key=@SearchKey
AND (Plaintext like @SearchText + '%'
OR Author_Copy like @SearchText + '%')
AND Is_Current=1
ORDER BY SearchTerm, Author_Copy
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
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForParent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForParent]
GO

/*===========================================================================*\
  Description: Returns a list of concepts that are with the supplied parent	

  Parameters:	@ParentConceptKey
		@HierarchyRelationTypeKey - relationship type used to populate
						hierarchy.

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForParent]
	@ParentConceptKey varchar(100),
  	@HierarchyRelationTypeKey char(16)
AS

SELECT distinct C.Concept_Key, 
		T.Item_Name, 
		C.Sort_Code, 
  		CASE WHEN CR2.Concept_Relation_Key IS NULL THEN 0 
							   ELSE 1 
		END AS HasChildren,
  		C.Concept_Rank_Key
FROM 		Concept_Relation CR1
INNER JOIN 	Concept C ON C.Concept_Key = CR1.To_Concept_Key
INNER JOIN 	Term T ON T.Term_Key = C.Term_Key
LEFT JOIN 	(Concept_Relation CR2 
			INNER JOIN Concept AS C2 ON C2.Concept_Key = CR2.To_Concept_Key
						AND C2.List_Preferred = 1
						AND C2.Is_Current = 1)
		ON CR2.From_Concept_Key = C.Concept_Key
       		AND CR2.Thesaurus_Relation_Type_Key = @HierarchyRelationTypeKey
WHERE 		CR1.From_Concept_Key = @ParentConceptKey
AND 		CR1.Thesaurus_Relation_Type_Key = @HierarchyRelationTypeKey
AND 		C.List_Preferred = 1
AND 		C.Is_Current = 1
ORDER BY 	C.Sort_Code, 
		T.Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForParent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForParent'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForTopLevel]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForTopLevel]
GO

/*===========================================================================*\
  Description: Returns a list of concepts that are the top level for the 
    					 supplied concept group.

  Parameters:	@ConceptGroupKey
		@HierarchyRelationTypeKey - relationship type used to populate
						hierarchy.

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForTopLevel]
	@ConceptGroupKey char(16),
  	@HierarchyRelationTypeKey char(16)
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF


SELECT DISTINCT 
		CT.Concept_Key, 
		CT.Item_Name, 
  		CASE WHEN CR2.Concept_Relation_Key IS NULL THEN 0 ELSE 1 END AS HasChildren,
  		CT.Concept_Rank_Key, 
		CT.Sort_Code
FROM 		VW_ConceptTerm CT
LEFT JOIN 	Concept_Relation CR1 on CR1.To_Concept_Key=CT.Concept_Key
      				    AND CR1.Thesaurus_Relation_Type_Key=@HierarchyRelationTypeKey
		-- This is used to get the children. Join into Concept table to get only
		-- List_Preferred and Is_Current because these are the only one that will be 
		-- visible when expanded.
LEFT JOIN 	(Concept_Relation CR2 
			INNER JOIN Concept AS C2 ON C2.Concept_Key = CR2.To_Concept_Key
						AND C2.List_Preferred = 1
						AND C2.Is_Current = 1) 
		ON CR2.From_Concept_Key=CT.Concept_Key
       		AND CR2.Thesaurus_Relation_Type_Key=@HierarchyRelationTypeKey 
WHERE 		CT.List_Preferred = 1
AND 		CT.Is_Current = 1
AND 		CT.Concept_Group_Key = @ConceptGroupKey
AND 		CR1.From_Concept_Key is null   -- i.e. No parents, therefore top level concept. 
ORDER BY 	CT.Sort_Code

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [Dev - JNCC SQL]
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

SELECT DISTINCT CT.Concept_Key, CT.Item_Name, 
  CASE WHEN (CGV1Child.Concept_Group_Version_Key IS NULL OR CGV1Child.Sequence<=CGVChild.Sequence)
      AND (CGV2Child.Concept_Group_Version_Key IS NULL OR CGV2Child.Sequence>=CGVChild.Sequence) 
      AND CChild.Concept_Key IS NOT NULL THEN
    1
  ELSE
    0 
  END AS HasChildren,
  CT.Concept_Rank_Key
FROM Concept_Relation CR1
  INNER JOIN VW_ConceptTerm CT on CT.Concept_Key=CR1.To_Concept_Key
  LEFT JOIN Concept_Relation CR2 on CR2.From_Concept_Key=CT.Concept_Key
       AND CR2.Thesaurus_Relation_Type_Key=@HierarchyRelationTypeKey
  --Ensure concept is in the correct concept group version
  INNER JOIN Concept_Group_Version CGV on CGV.Concept_Group_Key=CT.Concept_Group_Key
      AND CGV.Concept_Group_Version_Key=@ConceptGroupVersionKey
  INNER JOIN Concept_History CH on CH.Concept_Key=CT.Concept_Key
  LEFT JOIN Concept_Group_Version CGV1 ON CGV1.Concept_Group_Version_Key=CH.Concept_Group_Version_From
  LEFT JOIN Concept_Group_Version CGV2 ON CGV2.Concept_Group_Version_Key=CH.Concept_Group_Version_To
  --And ensure the children are checked in the correct concept group version
  LEFT JOIN Concept CChild on CChild.Concept_Key=CR2.To_Concept_Key
  LEFT JOIN Concept_Group_Version CGVChild on CGVChild.Concept_Group_Key=CChild.Concept_Group_Key
      AND CGVChild.Concept_Group_Version_Key=@ConceptGroupVersionKey
  LEFT JOIN Concept_History CHChild on CHChild.Concept_Key=CChild.Concept_Key
  LEFT JOIN Concept_Group_Version CGV1Child ON CGV1Child.Concept_Group_Version_Key=CHChild.Concept_Group_Version_From
  LEFT JOIN Concept_Group_Version CGV2Child ON CGV2Child.Concept_Group_Version_Key=CHChild.Concept_Group_Version_To
WHERE CR1.From_Concept_Key=@ParentConceptKey
  AND CR1.Thesaurus_Relation_Type_Key=@HierarchyRelationTypeKey
  AND CT.List_Preferred=1
  AND (CGV1.Concept_Group_Version_Key IS NULL OR CGV1.Sequence<=CGV.Sequence)
  AND (CGV2.Concept_Group_Version_Key IS NULL OR CGV2.Sequence>=CGV.Sequence)
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
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

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

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConditionChecks_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConditionChecks_Select_ForTopLevel]
GO

CREATE PROCEDURE [dbo].[usp_ConditionChecks_Select_ForTopLevel] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@Key CHAR(16) = NULL

AS

--  DESCRIPTION
--  Returns top level Condition Checks data to the CollectionsBrowser
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@Key 				Optional Key. When specified, only the single top level record is returned with that key
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-18
--
SET NOCOUNT ON

-- Create  a table to hold the items we are looking for
DECLARE @Search TABLE (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)

IF @Key IS NOT NULL
	INSERT INTO @Search VALUES (@Key)
ELSE IF object_id('tempdb..#TempFilter') is not null
	INSERT INTO @Search SELECT DISTINCT ItemKey FROM #TempFilter
ELSE
	INSERT INTO @Search SELECT Conservation_Check_Key FROM Conservation_Check

IF @SortOrderIndex = 0
BEGIN
	SELECT CC.Conservation_Check_Key AS Item_Key, CC.Display_Caption
	FROM 
	CONSERVATION_CHECK CC
		INNER JOIN 
			CONCEPT C
		ON CC.Type_Concept_Key = C.Concept_Key
			AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
				OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
		INNER JOIN TERM T	ON C.Term_Key = T.Term_Key
		INNER JOIN @Search S ON S.ItemKey=CC.Conservation_Check_Key
	ORDER BY CC.Vague_Date_Start DESC, CC.Vague_Date_End DESC, Item_Name, CC.Ref_Number
END
ELSE IF @SortOrderIndex = 1
BEGIN
	SELECT CC.Conservation_Check_Key AS Item_Key, CC.Display_Caption
	FROM 
	CONSERVATION_CHECK CC
	INNER JOIN 
		CONCEPT C
	ON CC.Type_Concept_Key = C.Concept_Key
		AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
			OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
	INNER JOIN TERM T	ON C.Term_Key = T.Term_Key
	INNER JOIN @Search S ON S.ItemKey=CC.Conservation_Check_Key
	ORDER BY CC.Ref_Number
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConditionChecks_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConditionChecks_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConditionCheck_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConditionCheck_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the General tab of the Condition Check screen.

  Parameters:	@Key	Collection key

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConditionCheck_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	/*-----------------------------------------------------------*\
	  Need to find out it ConditionCheck is associated with any
	  Collections or Stores, so we know whether to display the
	  AppliesToAllSpecimens check box (See TSD - 4.2.7.4).
	\*-----------------------------------------------------------*/
	DECLARE @HasStores bit,
		@HasCollections bit

	SELECT		@HasStores = CASE Count(*) WHEN 0 THEN 0 ELSE 1 END
	FROM		Collection_Unit_Check AS CUC
	INNER JOIN 	Store AS S ON S.Collection_Unit_Key = CUC.Collection_Unit_Key
	WHERE		CUC.Conservation_Check_Key = @Key

	SELECT		@HasCollections = CASE Count(*) WHEN 0 THEN 0 ELSE 1 END
	FROM		Collection_Unit_Check AS CUC
	INNER JOIN 	Collection AS C ON C.Collection_Unit_Key = CUC.Collection_Unit_Key
	WHERE		CUC.Conservation_Check_Key = @Key


	SELECT 	TType.Item_Name AS Check_Type_Name, 
		C.Type_Concept_Key,
		C.Ref_Number, 
		C.Vague_Date_Start, 
		C.Vague_Date_End, 
		C.Vague_Date_Type, 
		C.Checked_By_Name_Key, 
		dbo.ufn_GetFormattedName(C.Checked_By_Name_Key) AS Checked_By_Name,
		TCondition.Item_Name AS Condition_Name, 
		C.Condition_Concept_Key,
		C.Applies_To_Contained_Specimens, 
		C.Details,
		C.Domain_Mask,
		@HasCollections AS Has_Collections,
		@HasStores AS Has_Stores,
		C.[Timestamp]
	
	FROM 		Conservation_Check AS C
	INNER JOIN 	VW_ConceptTerm AS TType ON C.Type_Concept_Key = TType.Concept_Key
	INNER JOIN 	VW_ConceptTerm AS TCondition ON C.Condition_Concept_Key = TCondition.Concept_Key
	WHERE C.Conservation_Check_Key = @Key

SET NOCOUNT OFF

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConditionCheck_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConditionCheck_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConditionCheck_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConditionCheck_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConditionCheck_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConditionCheck_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConditionCheck_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConditionCheck_Select TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_DeterminationsLifeSciences_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_DeterminationsLifeSciences_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_DeterminationsLifeSciences_Select_ForSearch] 
@SearchText VARCHAR(100),
@UserDomainMask int

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

	SELECT 		ITN.Taxon_List_Item_Key AS Item_Key, ITN.Preferred_Name AS DisplayTerm, ITN.Preferred_Name AS SearchTerm
	FROM		Index_Taxon_Name AS ITN 
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping TDCM ON TDCM.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
	LEFT JOIN	Concept C ON C.Concept_Key = TDCM.Concept_Key
	LEFT JOIN 	Concept_Group CG ON CG.Concept_Group_Key=C.Concept_Group_Key
	LEFT JOIN 	Local_Domain LD ON LD.Local_Domain_Key=CG.Local_Domain_Key
	LEFT JOIN 	Domain D ON D.Domain_Key=LD.Domain_Key
			AND ((D.Domain_Mask & @UserDomainMask>0) OR (D.Domain_Mask = 0))
			AND D.Has_Occurrences = 1
	WHERE 		ITN.Preferred_Name LIKE @SearchText + '%'

	ORDER BY DisplayTerm
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
	   WHERE  Id = Object_Id(N'[dbo].[usp_Determination_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Determination_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record in the Determination table.
		Ensures the Domain mask of the specimen is also updated.

  Parameters:	@DeterminationKey
		@IsForSpecimen
		@Timestamp timestamp

  Created:	July 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Determination_Delete]
	@DeterminationKey char(16),
	@IsForSpecimen bit,
	@Timestamp timestamp
AS
	DECLARE @SpecimenKey char(16),
		@ConceptKey char(16),
		@ConceptMask bigint,
		@OccurrenceKey char(16),
		@WasPreferredForOccurrence bit,
		@WasPreferredForSpecimen bit

	SET @WasPreferredForSpecimen = 0
	SET @WasPreferredForOccurrence = 0

	BEGIN TRANSACTION
		/*-------------------------------------------------------------*\
		  Set some values to search for new preferred, if deleting 
		  current preferred.
		\*-------------------------------------------------------------*/
		SELECT	@ConceptKey = Concept_Key,
			@OccurrenceKey = Occurrence_Key,
			@WasPreferredForOccurrence = Preferred,
			@SpecimenKey = Specimen_Collection_Unit_Key
		FROM	Determination
		WHERE	Determination_Key = @DeterminationKey

		-- Check for Specimen.
		IF EXISTS(SELECT * FROM Specimen_Unit WHERE Preferred_Determination_Key = @DeterminationKey) BEGIN
			SELECT	@WasPreferredForSpecimen = 1,
				@SpecimenKey = Collection_Unit_Key
			FROM	Specimen_Unit
			WHERE	Preferred_Determination_Key = @DeterminationKey

			-- Need to clear the field because of referential integrity.
			UPDATE 	Specimen_Unit
			SET	Preferred_Determination_Key = NULL
			WHERE	Collection_Unit_Key = @SpecimenKey
			IF @@Error <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		  Delete either from Determination table, or just the link.
		\*-------------------------------------------------------------*/
		IF @OccurrenceKey IS NOT NULL AND @SpecimenKey IS NOT NULL 
			-- Determination linked to both a specimen AND an occurrence
			IF @IsForSpecimen = 1
				-- Deleting for specimen, clear the link and keep Occurrence key
				UPDATE	Determination
				SET	Specimen_Collection_Unit_Key = NULL
				WHERE	Determination_Key = @DeterminationKey
				AND	(@Timestamp = Timestamp)
			ELSE
				-- Deleting for occurrence, clear the link and keep Spceimen key
				UPDATE	Determination
				SET	Occurrence_Key = NULL
				WHERE	Determination_Key = @DeterminationKey
				AND	(@Timestamp = Timestamp)
		ELSE
			-- Only either specimen or occurrence key, safe to delete.
			DELETE	Determination
			WHERE	Determination_Key = @DeterminationKey
			AND	(@Timestamp = Timestamp)

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  See if preferred flags need to be updated, and where.
		\*-------------------------------------------------------------*/
		-- Retrieve the mask of the concept from deleted determination.
		EXECUTE	usp_Get_Concept_Domain_Mask @ConceptKey, @ConceptMask OUTPUT

		-- Need to look for a new preferred determination. Default to first one found.
		IF @WasPreferredForOccurrence = 1 OR @WasPreferredForSpecimen = 1 BEGIN
			DECLARE	@NewConceptKey char(16),
				@NewConceptMask bigint,
				@NewPreferredDeterminationKey char(16)

			-- Get new preferred first, if there are determinations left to choose from.
			IF EXISTS(SELECT * FROM Determination WHERE Occurrence_Key = @OccurrenceKey)
			BEGIN
				-- For occurrences, it all happens in Determination table.
				IF @WasPreferredForOccurrence = 1 BEGIN
					SELECT	TOP 1 @NewPreferredDeterminationKey = Determination_Key
					FROM	Determination
					WHERE	Occurrence_Key = @OccurrenceKey

					UPDATE 	Determination
					SET	Preferred = 1
					WHERE	Determination_Key = @NewPreferredDeterminationKey
				END

				-- For specimen, it happens in Specimen_Unit table.
				IF @WasPreferredForSpecimen = 1 BEGIN
					SELECT	TOP 1 @NewPreferredDeterminationKey = Determination_Key
					FROM	Determination
					WHERE	Specimen_Collection_Unit_Key = @SpecimenKey

					UPDATE 	Specimen_Unit
					SET	Preferred_Determination_Key = @NewPreferredDeterminationKey
					WHERE	Collection_Unit_Key = @SpecimenKey
				END
				IF @@Error <> 0 GOTO RollbackAndExit

				-- Get concept key of new preferred determination.
				SELECT	@NewConceptKey = Concept_Key
				FROM	Determination
				WHERE	Determination_Key = @NewPreferredDeterminationKey
			END

			-- Retrieve the mask of the concept from deleted determination.
			EXECUTE	usp_Get_Concept_Domain_Mask @NewConceptKey, @NewConceptMask OUTPUT

			-- Different mask, so switch old one OFF and new one ON.
			IF @SpecimenKey IS NOT NULL AND @ConceptMask <> @NewConceptMask 
			BEGIN
				EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenKey, @ConceptMask, 0
				IF @@Error <> 0 GOTO RollbackAndExit

				EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenKey, @NewConceptMask, 1
				IF @@Error <> 0 GOTO RollbackAndExit
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determination_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Determination_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Determination_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Determination_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Determination_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Determination_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Determination_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Determination_IsPreferred_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Determination_IsPreferred_Get]
GO

/*===========================================================================*\
  Description:	Determines if the Determination is a preferred one.

  Parameters:	@DeterminationKey	Either a Determination_Key or Taxon_Determinaton_Key
		@IsLifeSciences		Specifies whether the record to be found is Life or Earth Sciences
		@SpecimenUnitKey	Specimen Collection_Unit_Key
		@IsPreferred		Output value

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Determination_IsPreferred_Get] 
	@DeterminationKey char(16),
	@SpecimenUnitKey char(16),
	@IsLifeSciences bit,
	@IsSpecimenUnit bit,
	@IsPreferred bit OUTPUT
AS
	IF @IsSpecimenUnit = 1
		SELECT	@IsPreferred = CASE 
				WHEN (Preferred_Determination_Key = @DeterminationKey) AND (@IsLifeSciences = 0) THEN 1 
				WHEN (Preferred_Taxon_Determination_Key = @DeterminationKey) AND (@IsLifeSciences = 1) THEN 1
				ELSE 0 
			END
		FROM	Specimen_Unit
		WHERE 	Collection_Unit_Key = @SpecimenUnitKey
	ELSE
	IF @IsLifeSciences = 1
		SELECT	@IsPreferred = Preferred
		FROM	Taxon_Determination
		WHERE	Taxon_Determination_Key = @DeterminationKey
	ELSE
		SELECT	@IsPreferred = Preferred
		FROM	Determination
		WHERE	Determination_Key = @DeterminationKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determination_IsPreferred_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Determination_IsPreferred_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Determination_IsPreferred_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Determination_IsPreferred_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Determination_IsPreferred_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Determination_IsPreferred_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Determination_IsPreferred_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Determination_IsPreferred_Get TO [Dev - JNCC SQL]
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
			IF @CurrentPrefDetKey <> @Key BEGIN
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
			-- Not guaranteed there is an associated specimen key.
			SELECT	@CurrentConceptKey = Concept_Key,
				@SpecimenKey = Specimen_Collection_Unit_Key
			FROM	Determination
			WHERE	Occurrence_Key = @OccurrenceKey
			AND	Preferred = 1

			-- We're having a new preferred, so switch the old one OFF
			-- But only if not changing preferred, or updating WILL mess up timestamp flag!
			UPDATE	Determination
			SET	Preferred = 0
			WHERE	Occurrence_Key = @OccurrenceKey
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

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Enquiries_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Enquiries_Select_ForTopLevel]
GO

CREATE PROCEDURE [dbo].[usp_Enquiries_Select_ForTopLevel] 
@SortOrderIndex TINYINT,
@Key CHAR(16) = NULL

AS

--  DESCRIPTION
--  Returns top level Enquiries data to the CollectionsBrowser
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@Key 				Optional Key. When specified, only the single top level record is returned with that key
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-14
--
SET NOCOUNT ON

-- Create  a table to hold the items we are looking for
DECLARE @Search TABLE (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)

IF @Key IS NOT NULL
	INSERT INTO @Search VALUES (@Key)
ELSE IF object_id('tempdb..#TempFilter') is not null
	INSERT INTO @Search SELECT DISTINCT ItemKey FROM #TempFilter
ELSE
	INSERT INTO @Search SELECT Enquiry_Key FROM Enquiry

IF @SortOrderIndex = 0
BEGIN
		SELECT Enquiry_Key AS Item_Key, E.Display_Caption
		FROM ENQUIRY E
			INNER JOIN CONCEPT C ON E.Enquiry_Type_Concept_Key = C.Concept_Key
			INNER JOIN TERM T	ON C.Term_Key = T.Term_Key
			INNER JOIN @Search S ON S.ItemKey=E.Enquiry_Key
		ORDER BY Vague_Date_Start DESC, Vague_Date_End DESC, T.PlainText, Vague_Date_Type
END
ELSE IF @SortOrderIndex = 1
BEGIN
		SELECT Enquiry_Key AS Item_Key, E.Display_Caption
		FROM ENQUIRY E
			INNER JOIN CONCEPT C ON E.Enquiry_Type_Concept_Key = C.Concept_Key
			INNER JOIN TERM T	ON C.Term_Key = T.Term_Key
			INNER JOIN @Search S ON S.ItemKey=E.Enquiry_Key
		ORDER BY T.PlainText
END


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Enquiries_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Enquiries_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Enquiry_Update_ForMovement]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Enquiry_Update_ForMovement]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Movement_Enquiry join table 
		so that there is a relationship between the Enquiry and 
		Movement tables.

  Parameters:	@ParentKey 	The key of the top level (parent) Movement node.
		@ChildKey	The key of the added (child) Enquiry node. 
		@SessionID	The SessionID.

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Enquiry_Update_ForMovement] 
	@ParentKey CHAR(16),
	@ChildKey CHAR(16),
	@SessionID char(16)
AS

SET NOCOUNT ON

	DECLARE @Key char(16)

	EXECUTE spNextKey 'Movement_Enquiry', @Key OUTPUT

	BEGIN TRANSACTION

		INSERT INTO Movement_Enquiry (
			Movement_Enquiry_Key,
			Movement_Key,
			Enquiry_Key,
			Entered_Session_ID
		) VALUES (
			@Key,
			@ParentKey,
			@ChildKey,
			@SessionID
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Enquiry_Update_ForMovement') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Enquiry_Update_ForMovement'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Enquiry_Update_ForMovement TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Enquiry_Update_ForMovement TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Enquiry_Update_ForMovement TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Enquiry_Update_ForMovement TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Enquiry_Update_ForMovement TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_FieldDataValid_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_FieldDataValid_Get]
GO

/*===========================================================================*\
  Description:	Used to insert a join record into the Specimen_Field_Data 
		table. This proc is used when the user is in the Field Data
		folder for a Specimen and they click Link To Existing.

  Parameters:	@SpecimenKey
	       	@OccTaxonOccKey		Either the Occurrence or then 
					Taxon_Occurrence key
               	@IsLifeScience		Whether it is a life sciences 
					Specimen or not.

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FieldDataValid_Get]
	@SpecimenKey char(16),
	@TaxonOccurrenceKey char(16),
	@OccurrenceKey char(16),
	@IsValid bit OUTPUT
AS
	SET NOCOUNT ON

	IF @TaxonOccurrenceKey IS NOT NULL
	BEGIN
		IF EXISTS 	(SELECT *
				FROM	Specimen_Field_Data
				WHERE	Collection_Unit_Key = @SpecimenKey
				AND	Taxon_Occurrence_Key = @TaxonOccurrenceKey)
			SET @IsValid = 0
		ELSE 	SET @IsValid = 1
	END
	ELSE
	BEGIN
		IF EXISTS 	(SELECT *
				FROM	Specimen_Field_Data
				WHERE	Collection_Unit_Key = @SpecimenKey
				AND	Occurrence_Key = @OccurrenceKey)
			SET @IsValid = 0
		ELSE 	SET @IsValid = 1
	END

	SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldDataValid_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldDataValid_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldDataValid_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldDataValid_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldDataValid_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_FieldDataValid_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldDataValid_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldDataValid_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_FieldData_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_FieldData_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Specimen_Field_Data table, and
		updates records in others. This stored proc. differs from 
		usp_SpecimenFieldData insert because this updates records in 
		the Sample and Survey_Event tables.

  Parameters:	@Key OUTPUT
		@SurveyEventKey 
	        @SurveyKey 
	        @LocationKey 
		@SampleKey 
	        @SpatialRefQualifier 
	        @SpatialRef 
	        @GatheringMethod 
	        @VagueDateStart 
	        @VagueDateEnd 
	        @VagueDateType
		@CollectionUnitKey 
		@OccurrenceKey 
		@TaxonOccurrenceKey 
		@InferredSurvey 
		@InferredLocation 
		@InferredSpatialRef 
		@InferredSampleType 
		@InferredDate 
		@InferredCollectors 
		@GatheringEvent
		@SessionID 

  Created:	January 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FieldData_Insert]
	@Key char(16) OUTPUT,
	@SurveyEventKey char(16) = NULL,
        @SurveyKey char(16),
        @LocationKey char(16),
	@SampleKey char(16),
        @SpatialRefQualifier varchar(20),
        @SpatialRef varchar(20),
	@Lat float,
	@Long float,
        @GatheringMethod char(16),
        @VagueDateStart int,
        @VagueDateEnd int,
        @VagueDateType varchar(2) = NULL,
	@CollectionUnitKey char(16),
	@OccurrenceKey char(16),
	@TaxonOccurrenceKey char(16),
	@InferredSurvey tinyint,
	@InferredLocation tinyint,
	@InferredSpatialRef tinyint,
	@InferredSampleType tinyint,
	@InferredDate tinyint,
	@InferredCollectors tinyint,
	@GatheringEvent bit,
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	[Sample]
		SET 	Location_Key = @LocationKey,
			Vague_Date_Start = @VagueDateStart, 
			Vague_Date_End = @VagueDateEnd, 
			Vague_Date_Type = IsNull(@VagueDateType, 'U'),
			Spatial_Ref_Qualifier = @SpatialRefQualifier,
			Spatial_Ref = @SpatialRef,
			Sample_Type_Key = @GatheringMethod,
			Lat = @Lat,
			Long = @Long
		WHERE	Sample_Key = @SampleKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Update the Survey Event table so that is points to the
		-- correct Survey record.

		IF @SurveyEventKey = NULL 
			SET @SurveyEventKey = (SELECT	Survey_Event_Key
						FROM	[Sample]
						WHERE	Sample_Key = @SampleKey)
		UPDATE	Survey_Event
		SET	Survey_Key = @SurveyKey
		WHERE	Survey_Event_Key = @SurveyEventKey

		IF @@Error <> 0 GOTO RollbackAndExit

		EXECUTE	spNextKey 'Specimen_Field_Data', @Key OUTPUT

		INSERT INTO Specimen_Field_Data (
			Specimen_Field_Data_Key, 
			Collection_Unit_Key, 
			Occurrence_Key, 
			Taxon_Occurrence_Key,
			Inferred_Survey, 
			Inferred_Location, 
			Inferred_Spatial_Ref, 
			Inferred_Sample_Type,
			Inferred_Date, 
			Inferred_Collectors, 
			Gathering_Event, 
			Entered_Session_ID
		) VALUES (
			@Key, 
			@CollectionUnitKey, 
			@OccurrenceKey, 
			@TaxonOccurrenceKey,
			@InferredSurvey, 
			@InferredLocation, 
			@InferredSpatialRef,
			@InferredSampleType, 
			@InferredDate, 
			@InferredCollectors,
			@GatheringEvent, 
			@SessionID
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldData_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldData_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldData_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldData_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldData_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_FieldData_Insert_ForSpecimen]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_FieldData_Insert_ForSpecimen]
GO

/*===========================================================================*\
  Description:	Used to insert a join record into the Specimen_Field_Data 
		table. This proc is used when the user is in the Field Data
		folder for a Specimen and they click Link To Existing.

  Parameters:	@Key 	Specimen_Field_Data key
		@SpecimenKey
		@TaxonOccurrenceKey
		@OccurrenceKey
		@SessionID

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FieldData_Insert_ForSpecimen]
	@Key char(16) OUTPUT,
	@SpecimenKey char(16),
	@TaxonOccurrenceKey char(16),
	@OccurrenceKey char(16),
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		EXECUTE spNextKey 'Specimen_Field_Data', @Key OUTPUT

		/*-------------------------------------------------------------*\
		  Insert in Specimen_Field_Data table.
		\*-------------------------------------------------------------*/
		INSERT INTO Specimen_Field_Data (
			Specimen_Field_Data_Key, Collection_Unit_Key, Occurrence_Key, 
			Taxon_Occurrence_Key, Inferred_Survey, Inferred_Location, 
			Inferred_Spatial_Ref, Inferred_Sample_Type, Inferred_Date, 
			Inferred_Collectors, Gathering_Event, Entered_Session_ID
		) VALUES (
			@Key, @SpecimenKey, @OccurrenceKey, 
			@TaxonOccurrenceKey, 0, 0, 
			0, 0, 0, 
			0, 0, @SessionID
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_Insert_ForSpecimen') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldData_Insert_ForSpecimen'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldData_Insert_ForSpecimen TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldData_Insert_ForSpecimen TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldData_Insert_ForSpecimen TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Insert_ForSpecimen TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldData_Insert_ForSpecimen TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_FieldData_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_FieldData_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the Field Data frame.

  Parameters:	@Key	Collection key

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FieldData_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT
		SV.Survey_Key,
		SV.Item_Name + ' - ' + dbo.ufn_GetFormattedName(SV.Run_By) AS Survey_Name,
		SV.Run_By, 
		SFD.Inferred_Survey,
		S.Sample_Key,
		SE.Survey_Event_Key,
		S.Location_Key,
		LN.Item_Name AS Location_Name,
		SFD.Inferred_Location,
		S.Spatial_Ref_Qualifier,
		S.Spatial_Ref,
		S.Lat, 
		S.Long,
		SFD.Inferred_Spatial_Ref,
		S.Sample_Type_Key,
		ST.Short_Name AS Sample_Type,
		S.Vague_Date_Start,
		S.Vague_Date_End,
		S.Vague_Date_Type,
		SFD.Taxon_Occurrence_Key,
		SFD.Occurrence_Key,
		SFD.Inferred_Sample_Type,
		SFD.Inferred_Date,
		SFD.Inferred_Collectors,
		SFD.Gathering_Event,
	  	SFD.Custodian,
		SFD.[Timestamp]
	FROM Specimen_Field_Data SFD
	LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	LEFT JOIN Occurrence O ON O.Occurrence_Key=SFD.Occurrence_Key
	INNER JOIN [Sample] S ON S.Sample_Key=XO.Sample_Key OR S.Sample_Key=O.Sample_Key
	INNER JOIN Survey_Event SE ON SE.Survey_Event_Key=S.Survey_Event_Key
	INNER JOIN Survey SV ON SV.Survey_Key=SE.Survey_Key
	LEFT JOIN Location_Name LN ON LN.Location_Key=S.Location_Key
				AND LN.Preferred = 1
	INNER JOIN Sample_Type ST ON ST.Sample_Type_Key=S.Sample_Type_Key
	WHERE SFD.Specimen_Field_Data_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldData_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldData_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldData_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldData_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldData_Select TO [Dev - JNCC SQL]
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
	   WHERE  Id = Object_Id(N'[dbo].[usp_HoldingOrg_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_HoldingOrg_Get]
GO

/*===========================================================================*\
  Description:	Get the HoldingOrg from the Setting table.

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_HoldingOrg_Get]
	@HoldingOrgKey char(16) OUTPUT 
AS
	SELECT 	@HoldingOrgKey = Data
	FROM 	Setting
	Where 	[Name] = 'HoldingOrg'
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_HoldingOrg_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_HoldingOrg_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_HoldingOrg_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_HoldingOrg_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_HoldingOrg_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_HoldingOrg_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_HoldingOrg_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_HoldingOrg_Get TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Jobs_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Jobs_Select_ForTopLevel]
GO

CREATE PROCEDURE [dbo].[usp_Jobs_Select_ForTopLevel] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@Key CHAR(16) = NULL

AS
--  DESCRIPTION
--  Returns Jobs to the top level of the CollectionsBrowser
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@Key 				Optional Key. When specified, only the single top level record is returned with that key
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-19
--
SET NOCOUNT ON

-- Create  a table to hold the items we are looking for
DECLARE @Search TABLE (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)

IF @Key IS NOT NULL
		INSERT INTO @Search VALUES (@Key)
ELSE IF object_id('tempdb..#TempFilter') is not null
	INSERT INTO @Search SELECT DISTINCT ItemKey FROM #TempFilter
ELSE
	INSERT INTO @Search SELECT Conservation_Job_Key FROM Conservation_Job

IF @SortOrderIndex = 0
BEGIN
	SELECT Conservation_Job_Key AS Item_Key, Display_Caption
	FROM CONSERVATION_JOB C
	INNER JOIN @Search S ON S.ItemKey=C.Conservation_Job_Key
	WHERE ((Domain_Mask & @UserDomainMask > 0) OR (Entered_Session_ID = @SessionID) 
			OR (Changed_Session_ID = @SessionID) OR (Domain_Mask = 0))
	ORDER BY From_Vague_Date_Start DESC, From_Vague_Date_End DESC, From_Vague_Date_Type, Item_Name
END
ELSE IF @SortOrderIndex = 1
BEGIN
	SELECT Conservation_Job_Key AS Item_Key, Display_Caption
	FROM CONSERVATION_JOB C
	INNER JOIN @Search S ON S.ItemKey=C.Conservation_Job_Key
	WHERE ((Domain_Mask & @UserDomainMask > 0) OR (Entered_Session_ID = @SessionID) 
			OR (Changed_Session_ID = @SessionID) OR (Domain_Mask = 0))
	ORDER BY Item_Name
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Jobs_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Jobs_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Jobs_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Jobs_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Jobs_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Jobs_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Jobs_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Jobs_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocalDomain_GetDefaultHierarchyRelationType]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LocalDomain_GetDefaultHierarchyRelationType]
GO


/*===========================================================================*\
  Description:	Determine the default hierarchy relation type associated
				with a local domain (if any).

  Parameters:   @local_domain_key	Local domain key
		@hierarchy_relation_type_key
				[on exit] Hierarchy relation type key

  Created:	Feb 2004

  Last revision information:
	$Revision: 3 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocalDomain_GetDefaultHierarchyRelationType]
	@local_domain_key CHAR(16),
	@hierarchy_relation_type_key CHAR(16) OUTPUT
AS
	SET NOCOUNT ON

	SELECT		@hierarchy_relation_type_key = d.Default_Hierarchy_Relation_Type_Key
	FROM		Local_Domain AS	ld
	INNER JOIN	Domain AS d
	ON		d.Domain_Key		=	ld.Domain_Key
	WHERE		ld.Local_Domain_Key	=	@local_domain_key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocalDomain_GetDefaultHierarchyRelationType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocalDomain_GetDefaultHierarchyRelationType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_GetDefaultHierarchyRelationType TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocalDomain_GetDefaultHierarchyRelationType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocalDomain_GetDefaultHierarchyRelationType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_LocalDomain_GetDefaultHierarchyRelationType TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocalDomain_GetDefaultHierarchyRelationType TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_GetDefaultHierarchyRelationType TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MakeConceptsSynonyms_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MakeConceptsSynonyms_Update]
GO

/*===========================================================================*\
  Description:	Take two Concept keys and make them synonyms.

  Parameters:	@ConceptKeySelected
		@ConceptKeyPasted

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MakeConceptsSynonyms_Update]
	@ConceptKeySelected char(16),
	@ConceptKeyPasted char(16)
AS

SET NOCOUNT ON

	DECLARE @MeaningKeySelected char(16),
		@MeaningKeyPasted char(16),	
		@MeaningKeyChosen char(16),
		@MeaningKeyDiscarded char(16),
		@ConceptGroupKeySource char(16),
		@ConceptGroupKeyDest char(16),
		@ConceptRankKey char(16),
		@SiteID char(8)

	BEGIN TRANSACTION

		/*=====================================================================*\
		  Get the meaning keys, and the Concept_Rank_Key of the target Concept.
		\*=====================================================================*/
		SELECT	@MeaningKeySelected = Meaning_Key,
			@ConceptRankKey = Concept_Rank_Key
		FROM	Concept
		WHERE	Concept_Key = @ConceptKeySelected
	
		SELECT	@MeaningKeyPasted = Meaning_Key
		FROM	Concept
		WHERE	Concept_Key = @ConceptKeyPasted
	
		/*================================================================================*\
		  2 concepts that are in the custody of SYSTEM00 cannot be made synonyms of each 
		  other as the software expects both meaning keys to be present. 
		\*================================================================================*/
		IF NOT ((@ConceptKeySelected LIKE 'SYSTEM00%') AND (@ConceptKeyPasted LIKE 'SYSTEM00%'))
		BEGIN
			SELECT	@SiteID = Data
			FROM	Setting
			WHERE	[Name] = 'SiteID'

			/*==================================*\
			  Choose which meaning key to keep. 
			\*==================================*/	

			/*================================================================================*\
			  If 1 concept has a meaning key starting SYSTEM00, then both concepts are 
			  assigned to this key.
			\*================================================================================*/
			IF @MeaningKeySelected LIKE 'SYSTEM00%'
			BEGIN			
				SET @MeaningKeyChosen = @MeaningKeySelected
				SET @MeaningKeyDiscarded = @MeaningKeyPasted
			END
			ELSE IF @MeaningKeyPasted LIKE 'SYSTEM00%'
			BEGIN
				SET @MeaningKeyChosen = @MeaningKeyPasted
				SET @MeaningKeyDiscarded = @MeaningKeySelected
			END
	
			/*================================================================================*\
			  If neither concept has a meaning key starting SYSTEM00, but one of the meaning 
			  keys starts with the Site ID of the current site, then this is the meaning 
			  key adopted.
			\*================================================================================*/
			ELSE IF @MeaningKeySelected LIKE @SiteID + '%'
			BEGIN
				SET @MeaningKeyChosen = @MeaningKeySelected
				SET @MeaningKeyDiscarded = @MeaningKeyPasted
			END
			ELSE IF @MeaningKeyPasted LIKE @SiteID + '%'
			BEGIN
				SET @MeaningKeyChosen = @MeaningKeyPasted
				SET @MeaningKeyDiscarded = @MeaningKeySelected
			END
	
			/*================================================================================*\
			  If a single meaning key is still not selected by these rules, then the meaning 
			  key that belongs to the concept that will be list preferred after the synonymy 
			  is applied is selected.
			\*================================================================================*/
			
			-- Make sure usp_Concept_Update_ForPreferred is run before this stored proc.
			-- That way the preferred and list_preferred fields for the pasted concept
			-- will already have been updated.

			ELSE IF EXISTS (SELECT 	* 	
					FROM 	Concept
					WHERE 	Concept_Key = @ConceptKeySelected
					AND 	List_Preferred = 1)
	 		BEGIN
				SET @MeaningKeyChosen = @MeaningKeySelected
				SET @MeaningKeyDiscarded = @MeaningKeyPasted
			END
	
			ELSE IF EXISTS (SELECT 	* 	
					FROM 	Concept
					WHERE 	Concept_Key = @ConceptKeyPasted
					AND 	List_Preferred = 1)
	 		BEGIN
				SET @MeaningKeyChosen = @MeaningKeyPasted
				SET @MeaningKeyDiscarded = @MeaningKeySelected
			END
	
			/*================================================================================*\
			  If this still does not resolve a single meaning key, then the meaning key 
			  adopted is the one associated with the concept selected at the time of 
			  application of synonymy.
			\*================================================================================*/
			ELSE
			BEGIN
				SET @MeaningKeyChosen = @MeaningKeySelected
				SET @MeaningKeyDiscarded = @MeaningKeyPasted
			END
			
			/*===========================*\
			  Actually do the update.
			\*===========================*/		
			UPDATE	Concept
			SET	Meaning_Key = @MeaningKeyChosen,
				Concept_Rank_Key = @ConceptRankKey
			WHERE	Meaning_Key = @MeaningKeyDiscarded

			/*=======================================================================*\
			  References to @MeaningKeyDiscarded need to be replaced with references
			  to @MeaningKeyChosen before the meaning record can be deleted.
			\*=======================================================================*/	
			UPDATE	Thesaurus_Fact
			SET	Meaning_Key = @MeaningKeyChosen
			WHERE	Meaning_Key = @MeaningKeyDiscarded

			UPDATE	Meaning_Relation
			SET	From_Meaning_Key = @MeaningKeyChosen
			WHERE	From_Meaning_Key = @MeaningKeyDiscarded

			UPDATE	Meaning_Relation
			SET	To_Meaning_Key = @MeaningKeyChosen
			WHERE	To_Meaning_Key = @MeaningKeyDiscarded

			UPDATE	Taxon_Dictionary_Meaning_Mapping
			SET	Meaning_Key = @MeaningKeyChosen
			WHERE	Meaning_Key = @MeaningKeyDiscarded

			/*========================================================================*\
			  Now all references to the old meaning key have gone, we can delete it 
			  (providing the two concepts aren't already synonyms. If that is the case, 
			  don't delete the meaing key.
			\*========================================================================*/
			IF @MeaningKeySelected <> @MeaningKeyPasted
				DELETE	Meaning
				WHERE	Meaning_Key = @MeaningKeyDiscarded
		
			/*===========================*\
			  Get the concept group keys.
			\*===========================*/
			SELECT	@ConceptGroupKeySource = Concept_Group_Key
			FROM	Concept
			WHERE	Concept_Key = @ConceptKeySelected
		
			SELECT	@ConceptGroupKeyDest = Concept_Group_Key
			FROM	Concept
			WHERE	Concept_Key = @ConceptKeyPasted
		
			/*===============================================================*\
			  If the source concept is in the same list as the dest concept, 
			  make it not list preferred.
			\*===============================================================*/
			IF @ConceptGroupKeySource = @ConceptGroupKeyDest
			BEGIN
				UPDATE	Concept
				SET	List_Preferred = 0
				WHERE	Concept_Key = @ConceptGroupKeySource
				AND	List_Preferred = 1
	
				DELETE	Concept_Lineage
				WHERE	Concept_Key = @ConceptGroupKeySource
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MakeConceptsSynonyms_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MakeConceptsSynonyms_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MakeConceptsSynonyms_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MakeConceptsSynonyms_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MakeConceptsSynonyms_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MakeConceptsSynonyms_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MakeConceptsSynonyms_Update TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Measurements_Select_ForCollectionUnit]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Measurements_Select_ForCollectionUnit]
GO

CREATE PROCEDURE [dbo].[usp_Measurements_Select_ForCollectionUnit] 
@ParentKey CHAR(16)

AS
--  DESCRIPTION
--  Returns Measurements data to the CollectionsBrowser for a given Collection Unit
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@ParentKey 			When specified, only the records associated with the parent key are returned
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-22
--
SET NOCOUNT ON

SELECT CUD.Collection_Unit_Data_Key As Item_Key, CUD.Applies_To As Item_Name, S.Date_Time_Start
FROM COLLECTION_UNIT_DATA CUD
INNER JOIN SESSION S ON CUD.Entered_Session_ID = S.Session_ID AND CUD.Collection_Unit_Key = @ParentKey
								AND CUD.Is_Descriptor = 0
ORDER BY CUD.Applies_To


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Measurements_Select_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Measurements_Select_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Measurements_Select_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Measurements_Select_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Measurements_Select_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Measurements_Select_ForCollectionUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Measurements_Select_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Measurements_Select_ForCollectionUnit TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MemberOfHoldingOrg_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MemberOfHoldingOrg_Get]
GO

/*===========================================================================*\
  Description:	Takes a Name key and returns 1 if the person is a 
		member of the Holding Organisation and 0 if they aren't.
		If the Name key belongs to an Organisation, then 1 is also
		returned.

  Parameters:	@Key 	Individual_Key
		@IsMember bit (output)

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MemberOfHoldingOrg_Get]
	@Key char(16),
	@IsMember bit OUTPUT 
AS
	DECLARE	@HoldingOrgKey char(16)

	/*-----------------------------------------*\
	  Get the Holding Organisation key.
	\*-----------------------------------------*/
	SELECT 	@HoldingOrgKey = Data
	FROM 	Setting
	WHERE	[Name] = 'HoldingOrg'


	/*-----------------------------------------------------------------*\
	  See if the Individual is a member of the Holding Organisation.
	\*-----------------------------------------------------------------*/
	SELECT @IsMember = CASE WHEN Count(*) = 0 THEN 0 ELSE 1 END
	FROM		Individual AS I
	INNER JOIN	Organisation_Department AS OD ON OD.Organisation_Department_Key = I.Organisation_Department_Key 
	INNER JOIN	Organisation AS O ON O.Name_Key = OD.Name_Key
					 AND O.Name_Key = @HoldingOrgKey
	WHERE		I.Name_Key = @Key
	OR		O.Name_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MemberOfHoldingOrg_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MemberOfHoldingOrg_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MemberOfHoldingOrg_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MemberOfHoldingOrg_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MemberOfHoldingOrg_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MemberOfHoldingOrg_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MemberOfHoldingOrg_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MemberOfHoldingOrg_Get TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_MovementNumberExists_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_MovementNumberExists_Get]
GO

/*===========================================================================*\
  Description:	Checks to see if a Movement number already exists.

  Parameters:	@Number
		@Exists OUTPUT	

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_MovementNumberExists_Get] 
	@Number varchar(30),
	@Exists bit OUTPUT
AS
	SET NOCOUNT ON

	SELECT @Exists = CASE WHEN Count(*) > 0 
				THEN 1
				ELSE 0
			END
	FROM		Movement
	WHERE		Number = @Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementNumberExists_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementNumberExists_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementNumberExists_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementNumberExists_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementNumberExists_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MovementNumberExists_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementNumberExists_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementNumberExists_Get TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Movements_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Movements_Select_ForTopLevel]
GO

CREATE PROCEDURE [dbo].[usp_Movements_Select_ForTopLevel] 
@UserDomainMask BIGINT,
@SessionID CHAR(16),
@Key CHAR(16) = NULL,
@MovementGroupType TINYINT,
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Movements data to the top level of the CollectionsBrowser
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@Key 				Optional Key. When specified, only the single top level record is returned with that key
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-15
--
SET NOCOUNT ON

DECLARE @MovementTypeLow INT, @MovementTypeHigh INT

-- Create  a table to hold the items we are looking for
DECLARE @Search TABLE (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)

IF @MovementGroupType=0 BEGIN -- AccessionsAndExchanges
	SET @MovementTypeLow=0
	SET @MovementTypeHigh=1
END
ELSE IF @MovementGroupType=1 BEGIN -- Loans
	SET @MovementTypeLow=2
	SET @MovementTypeHigh=3
END	
ELSE IF @MovementGroupType=2 BEGIN -- Other movements
	SET @MovementTypeLow=4
	SET @MovementTypeHigh=9
END


IF @Key IS NOT NULL
		INSERT INTO @Search VALUES (@Key)
ELSE IF object_id('tempdb..#TempFilter') is not null
	-- Use the temporary filter table to provide list of keys
	INSERT INTO @Search 
		SELECT DISTINCT ItemKey 
		FROM #TempFilter 
		INNER JOIN Movement M ON M.Movement_Key=#TempFilter.ItemKey
		INNER JOIN MOVEMENT_DIRECTION MD ON M.Movement_Key = MD.Movement_Key 
		LEFT JOIN MOVEMENT_COLLECTION_UNIT MCU ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
		LEFT JOIN COLLECTION_UNIT CU ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
		WHERE (Movement_Type BETWEEN @MovementTypeLow AND @MovementTypeHigh)
			AND ((CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
			OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID))
ELSE BEGIN
	-- Select all items
	INSERT INTO @Search 
		SELECT DISTINCT M.Movement_Key 
		FROM Movement M
		INNER JOIN MOVEMENT_DIRECTION MD ON M.Movement_Key = MD.Movement_Key 
		LEFT JOIN MOVEMENT_COLLECTION_UNIT MCU ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
		LEFT JOIN COLLECTION_UNIT CU ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
		WHERE (Movement_Type BETWEEN @MovementTypeLow AND @MovementTypeHigh)
			AND ((CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
			OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID))
		
END

IF @SortOrderIndex = 0
BEGIN
	SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, M.Display_Caption,
		 Exp_Vague_Date_Start, Exp_Vague_Date_End --DISTINCT removes duplicate records due to two direction types
	FROM MOVEMENT M
	INNER JOIN @Search S ON S.ItemKey=M.Movement_Key
	ORDER BY Exp_Vague_Date_Start DESC, Exp_Vague_Date_End DESC, M.Movement_Type, Number
END
ELSE IF @SortOrderIndex = 1
BEGIN
	SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, M.Display_Caption,
		 Exp_Vague_Date_Start, Exp_Vague_Date_End --DISTINCT removes duplicate records due to two direction types
	FROM MOVEMENT M
	INNER JOIN @Search S ON S.ItemKey=M.Movement_Key
	ORDER BY Number
END
ELSE IF @SortOrderIndex = 2
BEGIN
	SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, M.Display_Caption,
		 Exp_Vague_Date_Start, Exp_Vague_Date_End --DISTINCT removes duplicate records due to two direction types
	FROM MOVEMENT M
	INNER JOIN @Search S ON S.ItemKey=M.Movement_Key
	ORDER BY M.Movement_Type, M.Number
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movements_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movements_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movements_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movements_Select_ForTopLevel TO [Dev - JNCC SQL]
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
		@ContactNameKey
		@VagueDateStart
		@VagueDateEnd
		@VagueDateType
		@Number
		@Notes
		@SessionID

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Movement_Insert]
	@Key char(16) OUTPUT,
	@MovementType tinyint,
	@WithAcquisition bit, 
	@OtherPartyNameKey char(16),
	@StaffResponsibleNameKey char(16),
	@ContactNameKey char(16),
	@VagueDateStart int, 
	@VagueDateEnd int, 
	@VagueDateType varchar(2),
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

	DECLARE @MovementDirectionKey char(16)
	DECLARE @MovementOfOwnershipKey char(16)
	DECLARE @MovementOfMaterialKey char(16)
	DECLARE @HoldingOrg char(16)

	SELECT @HoldingOrg = (SELECT Data FROM Setting WHERE Name = 'HoldingOrg')

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
					Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key
				) VALUES (
					@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
					@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @StaffResponsibleNameKey
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
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @StaffResponsibleNameKey
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
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @OtherPartyNameKey
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

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @StaffResponsibleNameKey
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
			
			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @OtherPartyNameKey
			)	
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

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @OtherPartyNameKey
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

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
			
			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @StaffResponsibleNameKey
			)	
			IF @@Error <> 0 GOTO RollbackAndExit
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
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID 
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, 
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID 
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
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @OtherPartyNameKey
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
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @StaffResponsibleNameKey	
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
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @OtherPartyNameKey
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
	   WHERE  Id = Object_Id(N'[dbo].[usp_Movement_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Movement_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the General tab of the Movement screen.

  Parameters:	@Key	Collection key

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

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
			--Following fields are used to get async controls
			M.Movement_Key,
			0 AS CollectionIndex,
			1 AS SpecimenIndex,
			2 AS StoreIndex
	FROM 		Movement AS M
	LEFT JOIN	[Name] AS N ON N.Name_Key = M.Other_Party_Name_Key
	
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
	   WHERE  Id = Object_Id(N'[dbo].[usp_Multimedia_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Multimedia_Delete]
GO

/*===========================================================================*\
  Description:	Deletes records from the Source, Source_File and Source_Join
		tables given the Source_Key to delete.

  Parameters:	@Key		Source_Key
		@RecordKey	

  Created:	November 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Multimedia_Delete]
	@Key char(16),
	@RecordKey char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		DELETE	Source_Join
		WHERE	Source_Key = @Key
		AND 	Record_Key = @RecordKey

		IF @@Error <> 0 GOTO RollbackAndExit

		/*---------------------------------------------------------*\
		  See if there are any other records that link to this
		  Sources record. If there are, we can't delete it. If there
		  aren't, it can go. Multiple Source_Join records pointing
		  to the same Source record can come about when duplicating
		  Valuations, for example.
		\*---------------------------------------------------------*/
		DECLARE @SourceJoinRecords int
	
		SELECT	@SourceJoinRecords = Count(Source_Join_Key)
		FROM	Source_Join 
		WHERE	Source_Key = @Key

		IF @SourceJoinRecords = 0 
		BEGIN
			DELETE 	Source_File
			WHERE	Source_Key = @Key

			IF @@Error <> 0 GOTO RollbackAndExit
	
			DELETE	Source
			WHERE 	Source_Key = @Key

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Multimedia_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Multimedia_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Multimedia_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Multimedia_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Multimedia_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Multimedia_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Multimedia_Delete TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_NameRelation_Exists_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_NameRelation_Exists_Get]
GO

CREATE PROCEDURE [dbo].[usp_NameRelation_Exists_Get] 
@NameKey1 CHAR(16),
@NameKey2 CHAR(16),
@RelationExists BIT OUTPUT

AS

--  DESCRIPTION
--  Returns whether or not a Relationship exists between two Name_Keys
--
--	PARAMETERS
--	NAME					DESCRIPTION
--	@NameKey1				First Name_Key
--	@NameKey2				Second Name_Key
--	@RelationExists			Output
--
--
--  AUTHOR:					Ben Collier, Dorset Software
--  CREATED:				01/03/2004
--

IF EXISTS(
	SELECT * 
	FROM Name_Relation 
	WHERE ((Name_Key_1 = @NameKey1) AND (Name_Key_2 = @NameKey2))
		OR ((Name_Key_1 = @NameKey2) AND (Name_Key_2 = @NameKey1)))
	SET @RelationExists = 1
ELSE
	SET @RelationExists = 0

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_NameRelation_Exists_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_NameRelation_Exists_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_NameRelation_Exists_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_NameRelation_Exists_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_NameRelation_Exists_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_NameRelation_Exists_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_NameRelation_Exists_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_NameRelation_Exists_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Name_Select_ForNameSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Name_Select_ForNameSearch]
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
CREATE PROCEDURE [dbo].[usp_Name_Select_ForNameSearch]
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
	SELECT	Name_Key + '*' AS Item_Key, 'Department' AS Type,
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Name_Select_ForNameSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Name_Select_ForNameSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Name_Select_ForNameSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Name_Select_ForNameSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Name_Select_ForNameSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Name_Select_ForNameSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Name_Select_ForNameSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Name_Select_ForNameSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OccurrenceData_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OccurrenceData_Insert]
GO

/*===========================================================================*\
  Description: Inserts a record into Occurrence_Data.

  Parameters:  Fields of Occurrence_Data

  Created:     September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_OccurrenceData_Insert]
	-- Required by both Measurements and Descriptors updates.
	@Key char(16) OUTPUT,
       	@AppliesTo varchar(50),
       	@ParameterConceptKey char(16),
	@Value varchar(50) = NULL,
	@IsDescriptor bit,
	@SessionID char(16),
	-- Only required for the Measurements update.
	@LowerValue varchar(50) = NULL,
	@UpperValue varchar(50) = NULL,
	@OccurrenceKey char(16) = NULL,
	@MethodConceptKey char(16) = NULL,
	@Duration varchar(50) = NULL,
	@Accuracy varchar(50) = NULL,
	@UnitConceptKey char(16) = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	IF @IsDescriptor = 1
		SET @LowerValue = @Value
	
	BEGIN TRANSACTION

		EXECUTE spNextKey 'Occurrence_Data', @Key OUTPUT

		/*----------------------------------------------------------------------------------*\
		  If we are inserting measurement data, more information needs to inserted than if 
		  we are inserting descriptor data. Hence, there are two different insert statements.
		\*----------------------------------------------------------------------------------*/
		IF @IsDescriptor = 1
			INSERT INTO Occurrence_Data (
				Occurrence_Data_Key, Occurrence_Key, Applies_To, Parameter_Concept_Key, 
				Lower_Value, Is_Descriptor, Entered_Session_ID
			) VALUES (
				@Key, @OccurrenceKey, @AppliesTo, @ParameterConceptKey, IsNull(@LowerValue, ' '), 
				1, @SessionID
			)
		ELSE
			INSERT INTO Occurrence_Data (
				Occurrence_Data_Key, Occurrence_Key, Applies_To, Method_Concept_Key, Duration,
				Accuracy, Parameter_Concept_Key, Unit_Concept_Key, Lower_Value, Upper_Value,
				Is_Descriptor, Entered_Session_ID
			) VALUES (
				@Key, @OccurrenceKey, @AppliesTo, @MethodConceptKey, @Duration,
				@Accuracy, @ParameterConceptKey, @UnitConceptKey, IsNull(@LowerValue, ' '), @UpperValue,
				0, @SessionID
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceData_Insert') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_OccurrenceData_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OccurrenceData_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OccurrenceData_Select]
GO

/*===========================================================================*\
  Description:	Returns a measurement or descriptor record from
		Occurrence_Data table.

  Parameters:	@Key		Occurrence Data key
		@IsDescriptor	Flag to indicate whether Measurements or Descriptors
				are requested.

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_OccurrenceData_Select]
	@Key char(16),
	@IsDescriptor bit
AS

SET NOCOUNT ON

	-- Set of options for better use of vw_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	SELECT 	  	OD.Occurrence_Data_Key AS Item_Key,
			OD.Applies_To,
			OD.Method_Concept_Key,
			CTM.Plaintext AS Method_Term,
			OD.Duration,
			OD.Accuracy,
			OD.Parameter_Concept_Key,
			CTP.Plaintext AS Parameter_Term,
			OD.Unit_Concept_Key,
			CTU.Plaintext AS Unit_Term,
			OD.Lower_Value AS Value,
			OD.Lower_Value, 
			OD.Upper_Value,			
			OD.Custodian,
			OD.[Timestamp],
			S.Date_Time_Start
	FROM 		Occurrence_Data OD
	INNER JOIN 	vw_ConceptTerm CTP ON CTP.Concept_Key = OD.Parameter_Concept_Key
	LEFT JOIN 	vw_ConceptTerm CTM ON CTM.Concept_Key = OD.Method_Concept_Key
	LEFT JOIN 	vw_ConceptTerm CTU ON CTU.Concept_Key = OD.Unit_Concept_Key
	LEFT JOIN 	Session S ON OD.Entered_Session_ID = S.Session_ID
	WHERE 		OD.Occurrence_Data_Key = @Key
	AND		OD.Is_Descriptor = @IsDescriptor

SET NOCOUNT OFF 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceData_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_OccurrenceData_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_OccurrenceData_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_OccurrenceData_Select TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OccurrenceData_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OccurrenceData_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Occurrence_Data table.
		The Occurrence_Data table hold descriptor and measurement
		information.

  Parameters:	@Key
		@CollectionUnitKey
		@AppliesTo
		@MethodConceptKey
		@Duration
		@Accuracy
		@ParameterConceptKey
		@UnitConceptKey
		@LowerValue
		@UpperValue
		@IsDescriptor
		@SessionID
		@Timestamp

  Created:	November 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_OccurrenceData_Update]
	-- Required by both Measurements and Descriptors updates.
	@Key char(16),
	@IsDescriptor bit,
	@ParameterConceptKey char(16),
	@AppliesTo varchar(50),
	@Value varchar(50),
	@SessionID char(16),
	@Timestamp timestamp,

	-- Only required for the Measurements update.
	@LowerValue varchar(50) = NULL,
	@UpperValue varchar(50) = NULL,
	@OccurrenceKey char(16) = NULL,
	@MethodConceptKey char(16) = NULL,
	@Duration varchar(50) = NULL,
	@Accuracy varchar(50) = NULL,
	@UnitConceptKey char(16) = NULL

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	IF @IsDescriptor = 1
		SET @LowerValue = @Value

	/*----------------------------------------------------------------------------------*\
	  If we are updating measurement data, more information needs to changed than if
	  we are inserting descriptor data. Hence, there are two different update statements.
	\*----------------------------------------------------------------------------------*/
	BEGIN TRANSACTION

		IF @IsDescriptor = 1	
			-- Updating a descriptor.
			UPDATE	Occurrence_Data
			SET	Applies_To = @AppliesTo,
				Parameter_Concept_Key = @ParameterConceptKey,
				Lower_Value = IsNull(@LowerValue, ' '),
				Is_Descriptor = @IsDescriptor,
				Changed_Session_ID = @SessionID
			WHERE	Occurrence_Data_Key = @Key
			AND	(@Timestamp = Timestamp)
	
		ELSE		
			-- Updating a measurement.
			UPDATE	Occurrence_Data
			SET	Applies_To = @AppliesTo,
				Parameter_Concept_Key = @ParameterConceptKey,
				Lower_Value = IsNull(@LowerValue, ' '),
				Is_Descriptor = @IsDescriptor,
				Changed_Session_ID = @SessionID,
				Occurrence_Key = @OccurrenceKey,
				Method_Concept_Key = @MethodConceptKey,
				Duration = @Duration,
				Accuracy = @Accuracy,
				Unit_Concept_Key = @UnitConceptKey,
				Upper_Value = @UpperValue
			WHERE	Occurrence_Data_Key = @Key
			AND	 (@Timestamp = Timestamp)

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceData_Update') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_OccurrenceData_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Occurrences_Select_ForSample]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Occurrences_Select_ForSample]
GO

/*===========================================================================*\
  Description:	Returns occurrence records for a specified sample key.

  Parameters:	@SampleKey

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Occurrences_Select_ForSample]
	@SampleKey char(16)
AS

SET NOCOUNT ON
	
	SELECT DISTINCT	O.Occurrence_Key AS [Item_Key], 
			CTP.Item_Name AS [Item_Name], 
			O.Checked,
			O.Confidential
	FROM		Occurrence O
	LEFT JOIN	Determination D1 ON D1.Occurrence_Key = O.Occurrence_Key AND D1.Preferred = 1
	LEFT JOIN	Specimen_Field_Data SFD ON SFD.Occurrence_Key = O.Occurrence_Key 
	LEFT JOIN	Specimen_Unit SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
	LEFT JOIN	Determination D2 ON D2.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key AND D2.Preferred = 1
	LEFT JOIN	vw_ConceptTermPreferred CTP ON 
				(CTP.Concept_Key = D1.Concept_Key AND D1.Concept_Key IS NOT NULL) OR
				(CTP.Concept_Key = D2.Concept_Key AND D2.Concept_Key IS NOT NULL)
	WHERE		O.Sample_Key = @SampleKey

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Occurrences_Select_ForSample') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Occurrences_Select_ForSample'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSample TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSample TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSample TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSample TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSample TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSample TO [Dev - JNCC SQL]
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

	SELECT		
		O.Occurrence_Key AS [Item_Key], 
		CT.Item_Name + ' - ' +
				dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) +
			' ' + CASE WHEN S.Spatial_Ref IS NULL THEN '' ELSE S.Spatial_Ref END
		AS SearchTerm,
		CT.Item_Name + ' - ' +
				dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) +
			' ' + CASE WHEN S.Spatial_Ref IS NULL THEN '' ELSE S.Spatial_Ref END
		AS DisplayTerm
	FROM		Occurrence O
	INNER JOIN	Determination D ON O.Occurrence_Key = D.Occurrence_Key
	INNER JOIN	vw_ConceptTermPreferred CT ON D.Concept_Key = CT.Concept_Key
	INNER JOIN Sample S ON S.Sample_Key=O.Sample_Key
	WHERE		CT.Item_Name LIKE @SearchText + '%'
	ORDER BY	CT.Item_Name

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
	   WHERE  Id = Object_Id(N'[dbo].[usp_Occurrence_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Occurrence_Select]
GO

/*===========================================================================*\
  Description:	Selects an occurrence record.

  Parameters:	@Key

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Occurrence_Select]
	@Key char(16)
AS
	-- Options to get performance benefits from VW_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	SELECT		O.Occurrence_Key,
			O.Surveyors_Ref,
			O.Record_Type_Concept_Key AS [Record_Type_Key],
			CTRT.Item_Name AS [Record_Type_Name],
			O.Comment, 
			O.Confidential, 
			O.Checked, 
			O.Checked_By,
			O.Checked_Date,
			O.Verified, 
			O.Custodian, 
			O.TimeStamp,
			LD.Domain_Key
	FROM		Occurrence O
	LEFT JOIN 	VW_ConceptTerm CTRT ON CTRT.Concept_Key = O.Record_Type_Concept_Key

	-- Extra bit to try and obtain the Domain key for the Record_Type concept group
	LEFT JOIN	Determination D1 ON D1.Occurrence_Key = O.Occurrence_Key AND D1.Preferred = 1
	LEFT JOIN	Specimen_Field_Data SFD ON SFD.Occurrence_Key = O.Occurrence_Key 
	LEFT JOIN	Specimen_Unit SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
	LEFT JOIN	Determination D2 ON D2.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key AND D2.Preferred = 1
	LEFT JOIN	vw_ConceptTermPreferred CTP ON 
				(CTP.Concept_Key = D1.Concept_Key AND D1.Concept_Key IS NOT NULL) OR
				(CTP.Concept_Key = D2.Concept_Key AND D2.Concept_Key IS NOT NULL)
	LEFT JOIN	Concept_Group CG ON CG.Concept_Group_Key = CTP.Concept_Group_Key
	LEFT JOIN	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key

	WHERE		O.Occurrence_Key = @Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Occurrence_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Occurrence_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Occurrence_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Occurrence_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Occurrence_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Occurrence_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Occurrence_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Occurrence_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OtherPartySameOrganisation_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OtherPartySameOrganisation_Get]
GO

/*===========================================================================*\
  Description:	This proc. checks that the 'Other Party' is not - or is not a 
		member of - the same organisation that the Staff Responsible
		comes from. Takes in the key of the 'Other Party' and the
		'Staff Responsible'. 'Other Party' may be the key of an 
		Individual or an Organisation. Returns 1 or 0 depending on 
		whether the Other Party is or isn't related to the organisation
		of the Staff Responsible. 

		We don't want this proc. to be run for Internal Transfers.

  Parameters:	@StaffResponsibleKey
		@OtherPartyKey 
		@SameOrganisation bit OUTPUT 

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_OtherPartySameOrganisation_Get]
	@StaffResponsibleKey char(16),
	@OtherPartyKey char(16),
	@SameOrganisation bit OUTPUT 
AS
	SET NOCOUNT OFF

	DECLARE @OtherPartyIsOrganisation bit,
		@StaffResponsibleOrganisationKey char(16)

	/*--------------------------------------------------------------------*\
	  Staff Responsible must be an Individual. Get their Organisation.
	\*--------------------------------------------------------------------*/	
	SELECT 		@StaffResponsibleOrganisationKey = O.Name_Key
	FROM		Individual AS I
	LEFT JOIN	Name_Relation AS NR1 ON NR1.Name_Key_1 = I.Name_Key
	LEFT JOIN	Name_Relation AS NR2 ON NR2.Name_Key_2 = I.Name_Key
	INNER JOIN	Organisation AS O ON (O.Name_Key = NR1.Name_Key_2
					  OR O.Name_Key = NR2.Name_Key_1)
	WHERE		I.Name_Key = @StaffResponsibleKey
	
	/*--------------------------------------------------------------------*\
	  If the individual is a member of move than one organisation, then
	  set @SameOrganisation to 0, because there is no way of knowing which
	  organisation they are moving the item to.
	\*--------------------------------------------------------------------*/	
	IF @@RowCount > 1 
	BEGIN
		SET @SameOrganisation = 0
		RETURN 0
	END

	/*---------------------------------------*\
	  See if Other Party is an organisation.
	\*---------------------------------------*/
	SELECT 	@OtherPartyIsOrganisation = Organisation
	FROM	[Name]
	WHERE	Name_Key = @OtherPartyKey

	/*--------------------------------------------------------------------*\
	  If Other Party is an organisation, check it isn't the same 
	  organisation that the the Staff Responsible is from.
	\*--------------------------------------------------------------------*/	
	IF @OtherPartyIsOrganisation = 1 BEGIN
		IF @StaffResponsibleOrganisationKey = @OtherPartyKey 
			SET @SameOrganisation = 1
	END
	ELSE

	BEGIN
		/*-----------------------------------------------------------------------------*\
		  If Other Party is an individual, check they aren't from the same 
		  organisation that the the Staff Responsible is from. If the Other Party
		  individual belongs to none, or more than one organisation it is permitted.
		  It is permitted when they belong to more than one organisation because we 
		  can't tell what organisation they are involving in the movement.
		\*-----------------------------------------------------------------------------*/
		SELECT @SameOrganisation = CASE WHEN Count(O.Name_Key) = 1 THEN 1 
									   ELSE 0
					END
		FROM			Individual AS I
		LEFT JOIN		Name_Relation AS NR1 ON NR1.Name_Key_1 = I.Name_Key
		LEFT JOIN		Name_Relation AS NR2 ON NR2.Name_Key_2 = I.Name_Key
		INNER JOIN		Organisation AS O ON (O.Name_Key = NR1.Name_Key_2
								OR O.Name_Key = NR2.Name_Key_1)
		WHERE			I.Name_Key = @OtherPartyKey
		AND			O.Name_Key = @StaffResponsibleOrganisationKey
	END

	/*--------------------------------------------------------------------*\
	  Ensure that at least something is returned.
	\*--------------------------------------------------------------------*/	
	IF @SameOrganisation IS NULL SET @SameOrganisation = 0
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OtherPartySameOrganisation_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_OtherPartySameOrganisation_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_OtherPartySameOrganisation_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OtherPartySameOrganisation_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OtherPartySameOrganisation_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_OtherPartySameOrganisation_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OtherPartySameOrganisation_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_OtherPartySameOrganisation_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QETemplateField_Select_ForTemplate]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QETemplateField_Select_ForTemplate]
GO


/*===========================================================================*\
  Description:	Selects the fields for a template

  Parameters:	@QETemplateKey - QE_Template_Key
							@TemplateType - see template type field description

  Created:	Jan 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/    
CREATE PROCEDURE [dbo].[usp_QETemplateField_Select_ForTemplate]
  @QETemplateKey as Char(16),
  @TemplateType as Tinyint
 AS

SET NOCOUNT ON

Select  
	F.Item_Name as 'Field', 
	General_Tab,
 	Specimen_Tab,
	TF.Item_Name as 'Alternative_Name', 
	Default_Value, 
	F.Default_Size,
	TF.QE_Template_Field_Key, 
	TF.Timestamp,
	Data_Type, 
	F.QE_Field_Key,
	F.Field_Lookup_Key,
	Default_Display,
	Is_Measurement,
	null as Measurement_Applies_To,
	null as Measurement_Method_Concept_Key,
	null as Measurement_Duration,
	null as Measurement_Accuracy,
	null as Measurement_Parameter_Concept_Key,
	null as Measurement_Unit_Concept_Key,
	null as Measurement_Is_Specimen,
	Field_Name,
	Table_Name
		
FROM  QE_Field as F 
LEFT JOIN QE_Template_Field as TF on 
	((F.QE_Field_Key = TF.QE_Field_Key) and ( @QETemplateKey =  TF.QE_Template_Key)
	and (Is_Measurement=0))
WHERE (Template_Type & @TemplateType)<> 0

UNION -- measurements

Select Measurement_Applies_To, 
	General_Tab,
	Specimen_Tab, 
	Item_Name, 
	Default_Value,
	20, 
	QE_Template_Field_Key, 
	Timestamp,
	0, 
	null,
	'',
	Default_Display,
 	Is_Measurement,
	Measurement_Applies_To,
	Measurement_Method_Concept_Key,
	Measurement_Duration,
	Measurement_Accuracy,
	Measurement_Parameter_Concept_Key,
	Measurement_Unit_Concept_Key,
	Measurement_Is_Specimen,
	null,
	null
	from
	QE_Template_Field 
	where (Is_Measurement=1) and
	(QE_Template_Key= @QETemplateKey)
	and ((Measurement_Is_Specimen = 0)
	or (@TemplateType=1))
	ORDER BY F.Table_Name
GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Select_ForTemplate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplateField_Select_ForTemplate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_RecordTypeConceptGroup_Select_ForDomain]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_RecordTypeConceptGroup_Select_ForDomain]
GO

/*===========================================================================*\
  Description:	Returns the contents of the concept group called Record Types
			in the domain supplied

  Parameters:	@Key	Domain key

  Created:	Mar 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_RecordTypeConceptGroup_Select_ForDomain]
	@Key char(16)
AS

SET NOCOUNT ON

SELECT CT.Concept_Key, CT.Plaintext
FROM Concept_Group CG
INNER JOIN Local_Domain LD ON LD.Local_Domain_Key=CG.Local_Domain_Key
INNER JOIN VW_ConceptTermPreferred CT ON CT.Concept_Group_Key=CG.Concept_Group_Key
WHERE CG.Item_Name = 'Record Types'
AND LD.Domain_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_RecordTypeConceptGroup_Select_ForDomain') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_RecordTypeConceptGroup_Select_ForDomain'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_RecordTypeConceptGroup_Select_ForDomain TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_RecordTypeConceptGroup_Select_ForDomain TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_RecordTypeConceptGroup_Select_ForDomain TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_RecordTypeConceptGroup_Select_ForDomain TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_RecordTypeConceptGroup_Select_ForDomain TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_RecordTypeConceptGroup_Select_ForDomain TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_RecordTypes_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_RecordTypes_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of occurrence record types

  Created:	Mar 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_RecordTypes_Select]
AS

SET NOCOUNT ON

	SELECT RECORD_TYPE_KEY, SHORT_NAME
	FROM RECORD_TYPE

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_RecordTypes_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_RecordTypes_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_RecordTypes_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_RecordTypes_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_RecordTypes_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_RecordTypes_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_RecordTypes_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_RecordTypes_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SampleKeys_Select_ForSpecimenKeys]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SampleKeys_Select_ForSpecimenKeys]
GO

/*===========================================================================*\
  Description:	Returns a list of sample keys that relate to a list of
		specimen unit keys supplied by the #TempFilter table.

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SampleKeys_Select_ForSpecimenKeys]
AS

SET NOCOUNT ON

IF object_id('tempdb..#TempFilter') IS NULL
  RAISERROR('usp_SampleKeys_Select_ForSpecimenKeys expects the #TempFilter table to be populated', 16, 1) 

SELECT DISTINCT S.Sample_Key
FROM Specimen_Field_Data SFD
LEFT JOIN Occurrence O ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
INNER JOIN [Sample] S ON S.Sample_Key=O.Sample_Key
		OR S.Sample_Key=XO.Sample_Key
INNER JOIN #TempFilter TF ON TF.ItemKey= SFD.Collection_Unit_Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SampleKeys_Select_ForSpecimenKeys') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SampleKeys_Select_ForSpecimenKeys'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SampleKeys_Select_ForSpecimenKeys TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SampleKeys_Select_ForSpecimenKeys TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SampleKeys_Select_ForSpecimenKeys TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SampleKeys_Select_ForSpecimenKeys TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SampleKeys_Select_ForSpecimenKeys TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SampleKeys_Select_ForSpecimenKeys TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SampleKey_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SampleKey_Get]
GO
/*===========================================================================*\
  Description:	
  Parameters:	

  Created:	July 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SampleKey_Get]
	@Key char(16) OUTPUT,
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@SpatialRef varchar(20),
	@SpatialRefSystem varchar(4),
	@Lat float,
	@Long float,
	@SpatialRefQualifier varchar(20),
	@SampleTypeKey char(16),
	@LocationKey char(16),
	@SurveyEventKey char(16),
	@LocationName varchar(100),
	@SurveyKey char(16)
AS
	/*----------------------------------------------------------------------------*\
	  Match exact list of Field Collectors if #TempFieldCollectors has been 
	  created by Collections Browser.
	\*----------------------------------------------------------------------------*/
	IF object_id('tempdb..#TempFieldCollectors') IS NOT NULL
	BEGIN
		/*-----------------------------------------------------------------*\
		  Create a table variable to store the Survey_Event match keys.
		\*-----------------------------------------------------------------*/
		DECLARE @tableSurveyEventMatches TABLE (
			Survey_Event_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS
		)
	
		DECLARE @CurrentFieldCollector char(16)
	
		/*------------------------------------------------------------------*\
		  Create the cursor.
		\*------------------------------------------------------------------*/
		DECLARE curFieldCollectors CURSOR LOCAL FAST_FORWARD FOR
			SELECT 	Name_Key
			FROM	#TempFieldCollectors
	
		OPEN	curFieldCollectors

		/*-------------------------------------------------------*\
		  Give @CurrentFieldCollector its first value.
		\*-------------------------------------------------------*/	
		FETCH NEXT
		FROM	curFieldCollectors
		INTO	@CurrentFieldCollector

		/*-------------------------------------------------------*\
		  Start looping through the field collectors.
		\*-------------------------------------------------------*/		
		WHILE @@Fetch_Status = 0
		BEGIN
			/*------------------------------------------------------------------------*\
			  If the there are no records in @tableSurveyEventMatches, insert all of
			  the Survey_Event records for the first Field Collector in the table.
			\*------------------------------------------------------------------------*/
			IF NOT EXISTS (SELECT * FROM @tableSurveyEventMatches)
			BEGIN
				INSERT INTO @tableSurveyEventMatches (
					Survey_Event_Key
				) 
				SELECT 	Survey_Event_Key
				FROM	Survey_Event_Recorder
				WHERE	Name_Key = @CurrentFieldCollector
			END
			ELSE
			/*------------------------------------------------------------------------*\
			  As there are records in @tableSurveyEventMatches, we can now start
			  removing records that aren't matched.
			\*------------------------------------------------------------------------*/ 
			BEGIN
				-- Delete non matches
				DELETE		@tableSurveyEventMatches
				FROM		@tableSurveyEventMatches AS SEM
				LEFT JOIN	Survey_Event_Recorder AS SER ON SER.Survey_Event_Key = SEM.Survey_Event_Key
									AND SER.Name_Key = @CurrentFieldCollector
				WHERE		SER.Survey_Event_Key IS NULL
			END

			/*---------------------------------------------------------------------*\
			  Get next field collector and put value into @CurrentFieldCollector.
			\*---------------------------------------------------------------------*/		
			FETCH NEXT
			FROM	curFieldCollectors
			INTO	@CurrentFieldCollector
		END
	
		CLOSE curFieldCollectors
		DEALLOCATE curFieldCollectors

		/*---------------------------------------------------------------------*\
		  Now match on Spatial Ref, Sample Type, Date and Sample Recorders.
		\*---------------------------------------------------------------------*/	
		SELECT 		@Key = Sample_Key
		FROM 		[Sample] AS S
		INNER JOIN 	Survey_Event AS SE ON SE.Survey_Event_Key = S.Survey_Event_Key
		INNER JOIN 	@tableSurveyEventMatches AS SEM ON SEM.Survey_Event_Key = S.Survey_Event_Key
		WHERE 		(((S.Vague_Date_Start = @VagueDateStart) 
					AND	(S.Vague_Date_End = @VagueDateEnd) 
					AND	(S.Vague_Date_Type = @VagueDateType)) 
				OR ((S.Vague_Date_Type = 'U') AND (@VagueDateType='U')))
		AND		S.Spatial_Ref = @SpatialRef
		AND 		(IsNull(S.Spatial_Ref_System,'') = IsNull(@SpatialRefSystem, ''))
		AND		S.Spatial_Ref_Qualifier = @SpatialRefQualifier
		AND		(S.Sample_Type_Key = @SampleTypeKey)
		AND		(IsNull(S.Location_key, '') = IsNull(@LocationKey, ''))
		AND		(IsNull(S.Location_Name, '') = IsNull(@LocationName, ''))
		AND		(SE.Survey_Key = @SurveyKey)
	END
	ELSE
		/*---------------------------------------------------------------------*\
		  This matches on Spatial Ref, Sample Type and Date. I left this here  
		  in case it is required somewhere else and #TempFieldCollectors hasn't 
		  been created by the application.
		\*---------------------------------------------------------------------*/	
		SELECT 		@Key = Sample_Key
		FROM 		[Sample] AS S
		INNER JOIN 	Survey_Event AS SE ON SE.Survey_Event_Key = S.Survey_Event_Key
		WHERE 		(((S.Vague_Date_Start = @VagueDateStart) 
					AND	(S.Vague_Date_End = @VagueDateEnd) 
					AND	(S.Vague_Date_Type = @VagueDateType)) 
				OR ((S.Vague_Date_Type = 'U') AND (@VagueDateType='U')))
		AND		S.Spatial_Ref = @SpatialRef
		AND 		(IsNull(S.Spatial_Ref_System,'') = IsNull(@SpatialRefSystem, ''))
		AND		S.Spatial_Ref_Qualifier = @SpatialRefQualifier
		AND		(S.Sample_Type_Key = @SampleTypeKey)
		AND		(IsNull(S.Location_key, '') = IsNull(@LocationKey, ''))
		AND		(IsNull(S.Location_Name, '') = IsNull(@LocationName, ''))
		AND		(SE.Survey_Key = @SurveyKey)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SampleKey_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SampleKey_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Samples_Select_ForSpecimenKeysMap]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Samples_Select_ForSpecimenKeysMap]
GO

/*===========================================================================*\
  Description:	Returns a list of samples that relate to a list of
		specimen unit keys supplied by the #TempFilter table, only including
		the mappable ones

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Samples_Select_ForSpecimenKeysMap]
AS

SET NOCOUNT ON

IF object_id('tempdb..#TempFilter') IS NULL
  RAISERROR('usp_Samples_Select_ForSpecimenKeysMap expects the #TempFilter table to be populated', 16, 1) 

SELECT DISTINCT S.Sample_Key, S.Spatial_Ref, S.Spatial_Ref_System, S.Lat, S.Long, S.Vague_Date_Start
FROM Specimen_Field_Data SFD
LEFT JOIN Occurrence O ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
INNER JOIN [Sample] S ON S.Sample_Key=O.Sample_Key
		OR S.Sample_Key=XO.Sample_Key
INNER JOIN #TempFilter TF ON TF.ItemKey= SFD.Collection_Unit_Key
WHERE S.Lat IS NOT NULL AND S.Long IS NOT NULL

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Samples_Select_ForSpecimenKeysMap') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Samples_Select_ForSpecimenKeysMap'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Samples_Select_ForSpecimenKeysMap TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Samples_Select_ForSpecimenKeysMap TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Samples_Select_ForSpecimenKeysMap TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Samples_Select_ForSpecimenKeysMap TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Samples_Select_ForSpecimenKeysMap TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Samples_Select_ForSpecimenKeysMap TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SeasonNames_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SeasonNames_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of season names according to preferred language.

  Parameters:	@Language	The ISO abbreviation of language in use on 
				client machine.

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SeasonNames_Select]
	@Language varchar(4)
AS
	SELECT 	C1.Concept_Key, T.Plaintext
	FROM 	Concept C1
	JOIN 	Meaning M ON M.Meaning_Key = C1.Concept_Key
	JOIN 	Concept C2 ON C2.Meaning_Key = M.Meaning_Key
	JOIN 	Term T ON T.Term_Key = C2.Term_Key
	JOIN 	Language L ON L.Language_Key = T.Language_Key
	WHERE 	C2.Name_Type_Concept_Key = 'SYSTEM000000000L'	-- Common names
	AND	C1.Concept_Group_Key = 'SYSTEM000000000R'	-- Season names
	AND	L.Language_Key = @Language
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SeasonNames_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SeasonNames_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SeasonNames_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SeasonNames_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SeasonNames_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SeasonNames_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SeasonNames_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SeasonNames_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpatialRef_Select_ForLocation]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpatialRef_Select_ForLocation]
GO

/*===========================================================================*\
  Description:	Returns the spatial ref and spatial ref qualifier for a 
		location key.

  Parameters:	@Key	Location key

  Created:	January 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpatialRef_Select_ForLocation]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT
		Spatial_Ref AS SpatialRef,
		Spatial_Ref_Qualifier AS Qualifier,
		Spatial_Ref_System AS [System],
		Lat,
		Long
	FROM	Location
	WHERE	Location_Key = @Key


SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpatialRef_Select_ForLocation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpatialRef_Select_ForLocation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpatialRef_Select_ForLocation TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpatialRef_Select_ForLocation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpatialRef_Select_ForLocation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpatialRef_Select_ForLocation TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpatialRef_Select_ForLocation TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpatialRef_Select_ForLocation TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimensCollected_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimensCollected_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of all preferred determinations and the
		registration number for the specimens linked to the currently
		selected collection. Returns the list of all determinations for 
		specimen top level node.

  Parameters:	@Key			Key of the event recorder person
		@CollectionUnitKey	Specimen or Collection Unit key
		@KeyIsSpecimen		Bit saying whether the key is a specimen

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimensCollected_Select]
	@Key char(16),
	@CollectionUnitKey char(16),
	@UserDomainMask int,
	@SessionID char(16),
	@ShowCommonNames bit,
	@KeyIsSpecimen bit
AS

SET NOCOUNT ON

IF @KeyIsSpecimen = 1
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key,
			CASE WHEN SU.Life_Sciences = 0
			THEN CTP.PlainText COLLATE database_default
			ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								Preferred_Name,
								0,
								Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + CASE WHEN CUN.Number IS NULL THEN '' ELSE ' - ' + CUN.Number END AS Item_Name
	FROM		Individual AS I
	INNER JOIN	Survey_Event_Recorder AS SER ON SER.Name_Key = I.Name_Key
	INNER JOIN	Sample_Recorder AS SR ON SR.SE_Recorder_Key = SER.SE_Recorder_Key
	INNER JOIN	[Sample] AS S ON S.Sample_Key = SR.Sample_Key
	LEFT JOIN	Occurrence AS O ON O.Sample_Key = S.Sample_Key
	LEFT JOIN	Taxon_Occurrence AS XO ON XO.Sample_Key = S.Sample_Key
	INNER JOIN	Specimen_Field_Data AS SFD ON (SFD.Occurrence_Key = O.Occurrence_Key
							OR SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key)
						AND SFD.Gathering_Event = 1
	INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
					AND SU.Collection_Unit_Key = @CollectionUnitKey
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key 
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID)
						OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
					
	LEFT JOIN	(Determination AS D
				INNER JOIN	VW_ConceptTermPreferred AS CTP ON CTP.Concept_Key = D.Concept_Key)
			ON D.Determination_Key = SU.Preferred_Determination_Key
	LEFT JOIN 	(Taxon_Determination AS TD 
				INNER JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key)
			ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key  
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
					 AND CUN.Preferred = 1 AND CUN.Type_Concept_Key = 'SYSTEM0000000001' --REGISTRATION NUMBER
	
	WHERE		I.Name_Key = @Key
	ORDER BY	Item_Name
ELSE
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key,
			CASE WHEN SU.Life_Sciences = 0
			THEN CTP.PlainText COLLATE database_default
			ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								Preferred_Name,
								0,
								Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + CASE WHEN CUN.Number IS NULL THEN '' ELSE ' - ' + CUN.Number END AS Item_Name
	FROM		Individual AS I
	INNER JOIN	Survey_Event_Recorder AS SER ON SER.Name_Key = I.Name_Key
	INNER JOIN	Sample_Recorder AS SR ON SR.SE_Recorder_Key = SER.SE_Recorder_Key
	INNER JOIN	[Sample] AS S ON S.Sample_Key = SR.Sample_Key
	LEFT JOIN	Occurrence AS O ON O.Sample_Key = S.Sample_Key
	LEFT JOIN	Taxon_Occurrence AS XO ON XO.Sample_Key = S.Sample_Key
	INNER JOIN	Specimen_Field_Data AS SFD ON (SFD.Occurrence_Key = O.Occurrence_Key
							OR SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key)
						AND SFD.Gathering_Event = 1
	INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
					AND SU.Parent_Collection_Collection_Unit_Key = @CollectionUnitKey
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key 
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID)
						OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	LEFT JOIN	(Determination AS D
				INNER JOIN	VW_ConceptTermPreferred AS CTP ON CTP.Concept_Key = D.Concept_Key)
			ON D.Determination_Key = SU.Preferred_Determination_Key
	LEFT JOIN 	(Taxon_Determination AS TD 
				INNER JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key)
			ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key  
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
					 AND CUN.Preferred = 1 AND CUN.Type_Concept_Key = 'SYSTEM0000000001' --REGISTRATION NUMBER
	
	WHERE		I.Name_Key = @Key
	ORDER BY	Item_Name
SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensCollected_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimensCollected_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimensDetermined_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimensDetermined_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of all preferred determinations and the
		registration number for the specimens linked to the currently
		selected collection. Returns the list of all determinations for
		specimen top level node.

  Parameters:	@Key			Key of the determiner.
		@CollectionUnitKey	Key of the collection unit
		@KeyIsSpecimen		Bit saying whether the key is a specimen

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimensDetermined_Select]
	@Key char(16),
	@CollectionUnitKey char(16),
	@UserDomainMask int,
	@SessionID char(16),
	@ShowCommonNames bit,
	@KeyIsSpecimen bit
AS

SET NOCOUNT ON

IF @KeyIsSpecimen = 1
BEGIN
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key, 
			CASE SU.Life_Sciences 
				WHEN 0 THEN 
					CTPref.PlainText COLLATE database_default
				ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								ITN.Preferred_Name,
								0,
								ITN.Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + 
			CASE 
				WHEN CUN.Number IS NULL THEN '' 
				ELSE ' - ' + CUN.Number 
			END AS Item_Name
	FROM 		Specimen_Unit AS SU
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key	
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
	LEFT JOIN	Taxon_Determination AS TD ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key
	LEFT JOIN	Determination AS D ON D.Determination_Key = SU.Preferred_Determination_Key
	INNER JOIN	Individual AS I ON (I.Name_Key = TD.Determiner
						OR I.Name_Key = D.Determiner_Name_Key)
					AND I.Name_Key = @Key
	LEFT JOIN 	VW_ConceptTermPreferred CTPref ON CTPref.Concept_Key=D.Concept_Key
	LEFT JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
			 			AND CUN.Preferred = 1 
						AND CUN.Type_Concept_Key = 'SYSTEM0000000001' --REGISTRATION NUMBER
	WHERE 		SU.Collection_unit_key = @CollectionUnitKey
	ORDER BY 	Item_Name
END
ELSE
BEGIN
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key, 
			CASE SU.Life_Sciences 
				WHEN 0 THEN 
					CTPref.PlainText COLLATE database_default
				ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								ITN.Preferred_Name,
								0,
								ITN.Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + 
			CASE 
				WHEN CUN.Number IS NULL THEN '' 
				ELSE ' - ' + CUN.Number 
			END AS Item_Name
	FROM 		Specimen_Unit AS SU
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key	
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
	LEFT JOIN	Taxon_Determination AS TD ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key
	LEFT JOIN	Determination AS D ON D.Determination_Key = SU.Preferred_Determination_Key
	INNER JOIN	Individual AS I ON (I.Name_Key = TD.Determiner
						OR I.Name_Key = D.Determiner_Name_Key)
					AND I.Name_Key = @Key
	LEFT JOIN 	VW_ConceptTermPreferred CTPref ON CTPref.Concept_Key=D.Concept_Key
	LEFT JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
			 			AND CUN.Preferred = 1 
						AND CUN.Type_Concept_Key = 'SYSTEM0000000001' --REGISTRATION NUMBER
	WHERE 		SU.Parent_Collection_Collection_unit_key = @CollectionUnitKey
	ORDER BY 	Item_Name
END


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensDetermined_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimensDetermined_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForCollection]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForCollection]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForCollection] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ParentKey CHAR(16),
@ShowCommonNames BIT,
@SortOrderIndex TINYINT

AS
--  DESCRIPTION
--  Returns Specimens data to the CollectionsBrowser for a given Collection
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@ParentKey 			When specified, only the records associated with the parent key are returned
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-14
--
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
	[Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE database_default NULL,
	[Hint] [nvarchar] (150) COLLATE database_default NULL 
)


INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences) 
SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key
	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
WHERE SU.Parent_Collection_Collection_Unit_Key = @ParentKey


UPDATE @SpecimensSearch
SET	Det_Item_Key =
		    CASE WHEN SU.Life_Sciences = 0 THEN 
		      CPref.Concept_Key 
	    	ELSE
					TD.Taxon_List_Item_Key					
	    	END,
		Item_Name = 
		    CASE WHEN SU.Life_Sciences = 0 THEN 
		      TPref.Item_Name 
	    	ELSE
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames)
				END,
		Number = CUN.Number,
		Det_Item_Name =
			CASE WHEN SU.Life_Sciences = 0 THEN TDet.Plaintext 
		    ELSE ITN.Actual_Name
	    	END
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001'
INNER JOIN Specimen_Unit SUnit ON SUnit.Collection_Unit_Key=SU.Item_Key
LEFT JOIN Determination D ON D.Determination_Key=SUnit.Preferred_Determination_Key
LEFT JOIN Concept C ON C.Concept_Key=D.Concept_Key
LEFT JOIN Term TDet ON TDet.Term_Key=C.Term_Key
LEFT JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
LEFT JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key
LEFT JOIN Taxon_Determination TD ON SU.Item_Key = TD.Specimen_Collection_Unit_Key
LEFT JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key



-- Select table and sort appropriately
IF @SortOrderIndex = 0
	SELECT * from @SpecimensSearch
	ORDER BY Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT * from @SpecimensSearch
	ORDER BY Number, Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForCollection') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForCollection'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForLinkedOther]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForLinkedOther]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForLinkedOther] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ParentKey CHAR(16),
@ShowCommonNames BIT,
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Related Specimens for a specified Specimen
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@ParentKey 			Only the records associated with the parent key are returned
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     Ben Collier, Dorset Software
--  CREATED:    2003-08-28
--
SET NOCOUNT ON
-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF


IF @SortOrderIndex = 0
	SELECT DISTINCT
		CUR.Collection_Unit_Relation_Key AS Item_Key,
		SU.Collection_Unit_Key AS Hyperlink_Item_Key,
		SU.Collection_Unit_Key AS Drag_Drop_Item_Key,
	    CASE WHEN SU.Life_Sciences=0 THEN 
			CT.Concept_Key
	    ELSE
			ITN.Taxon_List_Item_Key
		END AS Det_Item_Key,
		CASE 
		WHEN SU.Life_Sciences=0 THEN 
				CT.Item_Name 
		ELSE
				dbo.ufn_GetFormattedTaxonNameByParams(ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name, 
				ITN.Common_Name_Italic, ITN.Authority, @ShowCommonNames) 
		END AS Item_Name,
		
		SU.Life_Sciences,
		CUN.Number
		
	FROM 
		COLLECTION_UNIT_RELATION CUR
		INNER JOIN
			THESAURUS_RELATION_TYPE TRT
		ON CUR.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
		
		INNER JOIN
			SEMANTIC_RELATION SR
		ON TRT.Semantic_Relation_Key = SR.Semantic_Relation_Key
			AND (CUR.From_Collection_Unit_Key = @ParentKey)
				OR ((CUR.To_Collection_Unit_Key = @ParentKey) AND (SR.Unidirectional = 0))
		
		INNER JOIN
			SPECIMEN_UNIT SU
		ON SU.Collection_Unit_Key = 
			CASE WHEN CUR.To_Collection_Unit_Key = @ParentKey 
				THEN CUR.From_Collection_Unit_Key
			ELSE CUR.To_Collection_Unit_Key END

		INNER JOIN
			COLLECTION_UNIT CU
		ON SU.Collection_Unit_Key = CU.Collection_Unit_Key
			AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		
		LEFT JOIN 
			Collection_Unit_Number CUN 
		ON SU.Collection_Unit_Key = CUN.Collection_Unit_Key
			 AND CUN.Preferred = 1 AND CUN.Type_Concept_Key = 'SYSTEM0000000001'
		LEFT JOIN 
			Determination D 
		ON SU.Preferred_Determination_Key = D.Determination_Key
		LEFT JOIN 
			VW_ConceptTermPreferred CT 
		ON D.Concept_Key = CT.Concept_Key 

		LEFT JOIN 
			Taxon_Determination TD 
		ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
		LEFT JOIN 
			Index_Taxon_Name ITN 
		ON TD.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
	ORDER BY Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT DISTINCT
		CUR.Collection_Unit_Relation_Key AS Item_Key,
		SU.Collection_Unit_Key AS Hyperlink_Item_Key,
		SU.Collection_Unit_Key AS Drag_Drop_Item_Key,
	    CASE WHEN SU.Life_Sciences=0 THEN 
			CT.Concept_Key
	    ELSE
			ITN.Taxon_List_Item_Key
		END AS Det_Item_Key,
		CASE 
		WHEN SU.Life_Sciences=0 THEN 
				CT.Item_Name
		ELSE
				dbo.ufn_GetFormattedTaxonNameByParams(ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name, 
				ITN.Common_Name_Italic, ITN.Authority, @ShowCommonNames) 
		END AS Item_Name,
		
		SU.Life_Sciences,
		CUN.Number
		
	FROM 
		COLLECTION_UNIT_RELATION CUR
		INNER JOIN
			THESAURUS_RELATION_TYPE TRT
		ON CUR.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
		
		INNER JOIN
			SEMANTIC_RELATION SR
		ON TRT.Semantic_Relation_Key = SR.Semantic_Relation_Key
			AND (CUR.From_Collection_Unit_Key = @ParentKey)
				OR ((CUR.To_Collection_Unit_Key = @ParentKey) AND (SR.Unidirectional = 0))
		
		INNER JOIN
			SPECIMEN_UNIT SU
		ON SU.Collection_Unit_Key = 
			CASE WHEN CUR.To_Collection_Unit_Key = @ParentKey 
				THEN CUR.From_Collection_Unit_Key
			ELSE CUR.To_Collection_Unit_Key END

		INNER JOIN
			COLLECTION_UNIT CU
		ON SU.Collection_Unit_Key = CU.Collection_Unit_Key
			AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		
		LEFT JOIN 
			Collection_Unit_Number CUN 
		ON SU.Collection_Unit_Key = CUN.Collection_Unit_Key
			 AND CUN.Preferred = 1 AND CUN.Type_Concept_Key = 'SYSTEM0000000001'
		LEFT JOIN 
			Determination D 
		ON SU.Preferred_Determination_Key = D.Determination_Key
		LEFT JOIN 
			VW_ConceptTermPreferred CT 
		ON D.Concept_Key = CT.Concept_Key 

		LEFT JOIN 
			Taxon_Determination TD 
		ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
		LEFT JOIN 
			Index_Taxon_Name ITN 
		ON TD.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
	ORDER BY Number, Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForLinkedOther') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForLinkedOther'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForLinkedOther TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForLinkedOther TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForLinkedOther TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForLinkedOther TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForLinkedOther TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForLinkedOther TO [Dev - JNCC SQL]
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
	[Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Key] [char] (16) COLLATE database_default NULL,
	[DisplayTerm] [nvarchar] (150) COLLATE database_default NULL,
	[SearchTerm] [nvarchar] (150) COLLATE database_default NULL,
	[Life_Sciences] [bit] NULL
)

--Find all specimens with a determination match
INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, SearchTerm, DisplayTerm, Det_Item_Key) 
SELECT DISTINCT 
	SU.Collection_Unit_Key					COLLATE database_default, 
	SU.Life_Sciences,
	CASE Su.Life_Sciences
		WHEN 0 THEN TSearch.Plaintext		COLLATE database_default 
		ELSE ITN.Actual_Name				COLLATE database_default 
	END AS SearchTerm,
	CASE Su.Life_Sciences 
		WHEN 0 THEN TSearch.Item_Name		COLLATE database_default
		ELSE CASE ITN.Actual_Name_Italic	
			WHEN 1 THEN '<i>' + ITN.Actual_Name + '</i>' COLLATE database_default
			ELSE ITN.Actual_Name			COLLATE database_default
		END
	END AS DisplayTerm,
	CASE Su.Life_Sciences 
		WHEN 0 THEN C.Concept_Key COLLATE database_default
		ELSE ITN.Taxon_List_Item_Key COLLATE database_default
	END AS Det_Item_Key
	
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
LEFT JOIN DETERMINATION D ON SU.Collection_Unit_Key = D.Specimen_Collection_Unit_Key
LEFT JOIN Concept C ON D.Concept_Key = C.Concept_Key
LEFT JOIN Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
LEFT JOIN Term TSearch ON TSearch.Term_Key=CSearch.Term_Key
LEFT JOIN TAXON_DETERMINATION TD ON SU.Collection_Unit_Key = TD.Specimen_Collection_Unit_Key
LEFT JOIN Index_Taxon_Synonym ITS ON ITS.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
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
INNER JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN Concept C ON C.Concept_Key=CUN.Type_Concept_Key
	AND C.Meaning_Key='SYSTEM0000000001'

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

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByAnyDetermination]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByAnyDetermination]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByAnyDetermination] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SearchText VARCHAR(150),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Specimens based on the any determination
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SearchText			Text to be searched on
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     Ben Collier, Dorset Software
--  CREATED:    2003-10-07
--
SET NOCOUNT ON


--Use temp table to build output, so silly data errors such as duplicate accessions
-- don't duplicate in the list
DECLARE @SpecimensSearch TABLE
(
	[Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE database_default NULL,
	[Hint] [nvarchar] (150) COLLATE database_default NULL 
)

--Find all specimens with a determination match
INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences) 
SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
LEFT JOIN DETERMINATION D ON SU.Collection_Unit_Key = D.Specimen_Collection_Unit_Key
LEFT JOIN Concept C ON D.Concept_Key = C.Concept_Key
LEFT JOIN Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
LEFT JOIN Term TSearch ON TSearch.Term_Key=CSearch.Term_Key
LEFT JOIN TAXON_DETERMINATION TD ON SU.Collection_Unit_Key = TD.Specimen_Collection_Unit_Key
LEFT JOIN Index_Taxon_Synonym ITS ON ITS.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
LEFT JOIN INDEX_TAXON_NAME ITN	ON ITS.Synonym_List_Item_Key = ITN.Taxon_List_Item_Key
WHERE 
	(TSearch.Plaintext LIKE @SearchText + '%' AND SU.Life_Sciences=0) 
	OR 
	(ITN.Actual_Name LIKE @SearchText + '%' AND SU.Life_Sciences=1)


UPDATE @SpecimensSearch
SET	Det_Item_Key=
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      CPref.Concept_Key 
	    	ELSE
					TD.Taxon_List_Item_Key					
	    	END,
		Item_Name = 
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      TPref.Item_Name 
	    	ELSE
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames)
				END,
		Number=CUN.Number,
		Det_Item_Name=
				CASE 
					WHEN SU.Life_Sciences=0 THEN TDet.Plaintext 
		    	ELSE ITN.Actual_Name
	    	END,
		Hint=
				CASE 
					WHEN SU.Life_Sciences=0 THEN TDet.Plaintext 
		    	ELSE ITN.Actual_Name
	    	END
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN Specimen_Unit SUnit ON SUnit.Collection_Unit_Key=SU.Item_Key
LEFT JOIN Determination D ON D.Determination_Key=SUnit.Preferred_Determination_Key
LEFT JOIN Concept C ON C.Concept_Key=D.Concept_Key
LEFT JOIN Term TDet ON TDet.Term_Key=C.Term_Key
LEFT JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
LEFT JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key
LEFT JOIN Taxon_Determination TD ON SU.Item_Key = TD.Specimen_Collection_Unit_Key
LEFT JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key

-- Select table and sort appropriately
IF @SortOrderIndex = 0
	SELECT * from @SpecimensSearch
	ORDER BY Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT * from @SpecimensSearch
	ORDER BY Number, Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByAnyDetermination') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByAnyDetermination'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByDescription]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByDescription]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByDescription] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SearchText VARCHAR(100),
@SortOrderIndex TINYINT

AS
--  DESCRIPTION
--  Returns Specimens data based on a search using the Description parameter
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-10-07
--
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
	[Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE database_default NULL,
	[Hint] [text] COLLATE database_default NULL 
)

INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
SELECT 
	SU.Collection_Unit_Key, SU.Life_Sciences, M.Text
FROM Specimen_Unit SU
INNER JOIN Metadata M ON M.Record_Key=SU.Collection_Unit_Key
	AND M.Metadata_Type_Key='SYSTEM0000000006'
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
WHERE 
	M.Text LIKE @SearchText + '%'


UPDATE @SpecimensSearch
SET	Det_Item_Key=
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      CPref.Concept_Key 
	    	ELSE
					TD.Taxon_List_Item_Key					
	    	END,
		Item_Name = 
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      TPref.Item_Name 
	    	ELSE
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames)
				END,
		Number=CUN.Number,
		Det_Item_Name=
				CASE 
					WHEN SU.Life_Sciences=0 THEN TDet.Plaintext 
		    	ELSE ITN.Actual_Name
	    	END
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN Specimen_Unit SUnit ON SUnit.Collection_Unit_Key=SU.Item_Key
LEFT JOIN Determination D ON D.Determination_Key=SUnit.Preferred_Determination_Key
LEFT JOIN Concept C ON C.Concept_Key=D.Concept_Key
LEFT JOIN Term TDet ON TDet.Term_Key=C.Term_Key
LEFT JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
LEFT JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key
LEFT JOIN Taxon_Determination TD ON SU.Item_Key = TD.Specimen_Collection_Unit_Key
LEFT JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByDescription') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByDescription'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByDeterminationInGroup]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByDeterminationInGroup]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByDeterminationInGroup] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SearchText VARCHAR(100),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Specimens data based on the search parameter for Gathering Location
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SearchText			Text to search On
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-10-08
--
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
	[Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE database_default NULL,
	[Hint] [nvarchar] (150) COLLATE database_default NULL 
)

--Create a temp table to hold the lineages of all the terms that match the search
DECLARE @SearchLineage TABLE (
	Lineage varchar(900) COLLATE SQL_Latin1_General_CP1_CI_AS, 
	Concept_Group_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS, 
	PlainText varchar(150) COLLATE SQL_Latin1_General_CP1_CI_AI)

INSERT INTO @SearchLineage
SELECT DISTINCT CL.Lineage, CSynSearch.Concept_Group_Key, T.Plaintext
FROM Concept CSearch
	INNER JOIN Term T ON T.Term_Key = CSearch.Term_Key
	INNER JOIN Concept_Group CG ON CG.Concept_Group_Key=CSearch.Concept_Group_Key
	INNER JOIN Local_Domain LD ON LD.Local_Domain_Key=CG.Local_Domain_Key
	INNER JOIN Domain D ON D.Domain_Key=LD.Domain_Key
		AND D.Has_Occurrences = 1
	INNER JOIN Concept CSynSearch ON CSynSearch.Meaning_Key = CSearch.Meaning_Key
	INNER JOIN Concept_Lineage CL ON CL.Concept_Key = CSynSearch.Concept_Key
WHERE T.Plaintext LIKE @SearchText + '%'

INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
SELECT DISTINCT 
	SU.Collection_Unit_Key, 0 As Life_Sciences, SL.Plaintext
FROM @SearchLineage SL
	INNER JOIN Concept_Lineage CLChild ON CLChild.Lineage LIKE SL.Lineage + '\%'
	INNER JOIN Concept CChild ON CChild.Concept_Key = CLChild.Concept_Key
	    AND (CChild.Concept_Group_Key = SL.Concept_Group_Key)
	INNER JOIN Concept CChildSyn ON CChildSyn.Meaning_Key = CChild.Meaning_Key
	INNER JOIN Determination D ON D.Concept_Key = CChildSyn.Concept_Key
	INNER JOIN Specimen_Unit SU ON SU.Collection_Unit_Key = D.Specimen_Collection_Unit_Key
	INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key
		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	
	UNION 
	
SELECT DISTINCT 
		SU.Collection_Unit_Key AS Item_Key, 1 As Life_Sciences, ITNSearch.Actual_Name
FROM Index_Taxon_Name ITNSearch
	INNER JOIN Index_Taxon_Synonym ITSSearch ON ITSSearch.Taxon_List_Item_Key = ITNSearch.Taxon_List_Item_Key
	INNER JOIN Index_Taxon_Group ITG ON ITG.Taxon_List_Item_Key = ITSSearch.Synonym_List_Item_Key
	INNER JOIN Index_Taxon_Synonym ITSSyn ON ITSSyn.Taxon_List_Item_Key = ITG.Contained_List_Item_Key
	INNER JOIN Index_Taxon_Name ITNSyn ON ITNSyn.Taxon_List_Item_Key = ITSSyn.Synonym_List_Item_Key
	INNER JOIN Taxon_Determination TD ON TD.Taxon_List_Item_Key = ITNSyn.Taxon_List_Item_Key
	INNER JOIN Specimen_Unit SU ON SU.Collection_Unit_Key = TD.Specimen_Collection_Unit_Key
	INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key
		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
WHERE ITNSearch.Actual_Name LIKE @SearchText+'%'


UPDATE @SpecimensSearch
SET	Det_Item_Key=
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      CPref.Concept_Key 
	    	ELSE
					TD.Taxon_List_Item_Key					
	    	END,
		Item_Name = 
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      TPref.Item_Name 
	    	ELSE
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames)
				END,
		Number=CUN.Number,
		Det_Item_Name=
				CASE 
					WHEN SU.Life_Sciences=0 THEN TDet.Plaintext 
		    	ELSE ITN.Actual_Name
	    	END
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN Specimen_Unit SUnit ON SUnit.Collection_Unit_Key=SU.Item_Key
LEFT JOIN Determination D ON D.Determination_Key=SUnit.Preferred_Determination_Key
LEFT JOIN Concept C ON C.Concept_Key=D.Concept_Key
LEFT JOIN Term TDet ON TDet.Term_Key=C.Term_Key
LEFT JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
LEFT JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key
LEFT JOIN Taxon_Determination TD ON SU.Item_Key = TD.Specimen_Collection_Unit_Key
LEFT JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByDeterminationInGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByGatheringDate]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGatheringDate]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGatheringDate] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SearchText VARCHAR(50),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Specimens data based on the search parameter for Gathering Date
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SearchText			Text to search On
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-10-09
--
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
	[Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE database_default NULL,
	[Hint] [nvarchar] (150) COLLATE database_default NULL 
)

INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
SELECT DISTINCT 
	SU.Collection_Unit_Key, 
	SU.Life_Sciences, 
	dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type)
FROM Specimen_Unit SU
	INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	INNER JOIN Specimen_Field_Data SFD ON SFD.Collection_Unit_Key = SU.Collection_Unit_Key
		AND SFD.Gathering_Event=1
	LEFT JOIN Occurrence O ON O.Occurrence_Key = SFD.Occurrence_Key
	LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	INNER JOIN [Sample] S ON S.Sample_Key=O.Sample_Key OR S.Sample_Key = XO.Sample_Key
WHERE 
	dbo.ufn_CBWrapperForDoVagueDatesOverlap(@SearchText, S.Vague_Date_Start, S.Vague_Date_End) = 1


UPDATE @SpecimensSearch
SET	Det_Item_Key=
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      CPref.Concept_Key 
	    	ELSE
					TD.Taxon_List_Item_Key					
	    	END,
		Item_Name = 
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      TPref.Item_Name 
	    	ELSE
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames)
				END,
		Number=CUN.Number,
		Det_Item_Name=
				CASE 
					WHEN SU.Life_Sciences=0 THEN TDet.Plaintext 
		    	ELSE ITN.Actual_Name
	    	END
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN Specimen_Unit SUnit ON SUnit.Collection_Unit_Key=SU.Item_Key
LEFT JOIN Determination D ON D.Determination_Key=SUnit.Preferred_Determination_Key
LEFT JOIN Concept C ON C.Concept_Key=D.Concept_Key
LEFT JOIN Term TDet ON TDet.Term_Key=C.Term_Key
LEFT JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
LEFT JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key
LEFT JOIN Taxon_Determination TD ON SU.Item_Key = TD.Specimen_Collection_Unit_Key
LEFT JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGatheringDate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByGatheringDate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByGatheringLocation]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGatheringLocation]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGatheringLocation] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SearchText VARCHAR(100),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Specimens data based on the search parameter for Gathering Location
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SearchText			Text to search On
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-10-08
--
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
	[Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE database_default NULL,
	[Hint] [nvarchar] (150) COLLATE database_default NULL 
)

INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
SELECT 
	DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, LN.Item_Name
FROM Specimen_Unit SU
	INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	INNER JOIN Specimen_Field_Data SFD ON SFD.Collection_Unit_Key = SU.Collection_Unit_Key
		AND SFD.Gathering_Event=1
	LEFT JOIN Occurrence O ON O.Occurrence_Key = SFD.Occurrence_Key
	LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	INNER JOIN [Sample] S ON S.Sample_Key=O.Sample_Key OR S.Sample_Key = XO.Sample_Key
	INNER JOIN [Survey_Event] SE ON SE.Survey_Event_Key=S.Survey_Event_Key
	INNER JOIN Location_Name LN ON LN.Location_Key=S.Location_Key OR LN.Location_KEY=SE.Location_Key
WHERE 
	LN.Item_Name LIKE @SearchText + '%'


UPDATE @SpecimensSearch
SET	Det_Item_Key=
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      CPref.Concept_Key 
	    	ELSE
					TD.Taxon_List_Item_Key					
	    	END,
		Item_Name = 
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      TPref.Item_Name 
	    	ELSE
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames)
				END,
		Number=CUN.Number,
		Det_Item_Name=
				CASE 
					WHEN SU.Life_Sciences=0 THEN TDet.Plaintext 
		    	ELSE ITN.Actual_Name
	    	END
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN Specimen_Unit SUnit ON SUnit.Collection_Unit_Key=SU.Item_Key
LEFT JOIN Determination D ON D.Determination_Key=SUnit.Preferred_Determination_Key
LEFT JOIN Concept C ON C.Concept_Key=D.Concept_Key
LEFT JOIN Term TDet ON TDet.Term_Key=C.Term_Key
LEFT JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
LEFT JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key
LEFT JOIN Taxon_Determination TD ON SU.Item_Key = TD.Specimen_Collection_Unit_Key
LEFT JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGatheringLocation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByGatheringLocation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByGeographicInformation]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGeographicInformation]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGeographicInformation] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SearchText VARCHAR(100),
@SortOrderIndex TINYINT

AS
--  DESCRIPTION
--  Returns Specimens data based on the search parameter for Geographic Information
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-10-07
--
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
	[Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE database_default NULL,
	[Hint] [text] COLLATE database_default NULL 
)

INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
SELECT 
	SU.Collection_Unit_Key, SU.Life_Sciences, M.Text
FROM Specimen_Unit SU
INNER JOIN Metadata M ON M.Record_Key=SU.Collection_Unit_Key
	AND M.Metadata_Type_Key='SYSTEM0000000005'
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
WHERE 
	M.Text LIKE @SearchText + '%'


UPDATE @SpecimensSearch
SET	Det_Item_Key=
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      CPref.Concept_Key 
	    	ELSE
					TD.Taxon_List_Item_Key					
	    	END,
		Item_Name = 
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      TPref.Item_Name 
	    	ELSE
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames)
				END,
		Number=CUN.Number,
		Det_Item_Name=
				CASE 
					WHEN SU.Life_Sciences=0 THEN TDet.Plaintext 
		    	ELSE ITN.Actual_Name
	    	END
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN Specimen_Unit SUnit ON SUnit.Collection_Unit_Key=SU.Item_Key
LEFT JOIN Determination D ON D.Determination_Key=SUnit.Preferred_Determination_Key
LEFT JOIN Concept C ON C.Concept_Key=D.Concept_Key
LEFT JOIN Term TDet ON TDet.Term_Key=C.Term_Key
LEFT JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
LEFT JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key
LEFT JOIN Taxon_Determination TD ON SU.Item_Key = TD.Specimen_Collection_Unit_Key
LEFT JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGeographicInformation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByGeographicInformation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByPreferredAccNumber]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredAccNumber]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredAccNumber] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SearchText VARCHAR(30),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Specimens based on the Accession Number
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SearchText			Text to be searched on
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     Ben Collier, Dorset Software
--  CREATED:    2003-10-07
--
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
	[Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE database_default NULL,
	[Hint] [nvarchar] (150) COLLATE database_default NULL 
)

INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, M.Number
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
INNER JOIN Movement_Collection_Unit MCU ON MCU.Collection_Unit_Key = SU.Collection_Unit_Key
INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key=MCU.Movement_Direction_Key
	AND MD.Outbound=0
INNER JOIN Movement_Of_Ownership MOE ON MOE.Movement_Direction_Key=MD.Movement_Direction_Key
LEFT JOIN Movement_Of_Ownership_Exclusion MOEE 
		ON MOEE.Movement_Of_Ownership_Key=MOE.Movement_Of_Ownership_Key
		AND SU.Collection_Unit_Key=MOEE.Collection_Unit_Key
INNER JOIN Movement M ON M.Movement_Key=MD.Movement_Key
	AND M.Movement_Type IN (0,1)
WHERE M.Number LIKE @SearchText + '%'
AND MOEE.Movement_Of_Ownership_Exclusion_Key IS NULL


UPDATE @SpecimensSearch
SET	Det_Item_Key=
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      CPref.Concept_Key 
	    	ELSE
					TD.Taxon_List_Item_Key					
	    	END,
		Item_Name = 
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      TPref.Item_Name 
	    	ELSE
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames)
				END,
		Number=CUN.Number,
		Det_Item_Name=
				CASE 
					WHEN SU.Life_Sciences=0 THEN TDet.Plaintext 
		    	ELSE ITN.Actual_Name
	    	END
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN Specimen_Unit SUnit ON SUnit.Collection_Unit_Key=SU.Item_Key
LEFT JOIN Determination D ON D.Determination_Key=SUnit.Preferred_Determination_Key
LEFT JOIN Concept C ON C.Concept_Key=D.Concept_Key
LEFT JOIN Term TDet ON TDet.Term_Key=C.Term_Key
LEFT JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
LEFT JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key
LEFT JOIN Taxon_Determination TD ON SU.Item_Key = TD.Specimen_Collection_Unit_Key
LEFT JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByPreferredAccNumber'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByPreferredDetermination]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredDetermination]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredDetermination] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SearchText VARCHAR(150),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Specimens based on the the preferred determination
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SearchText			Text to be searched on
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     Ben Collier, Dorset Software
--  CREATED:    2003-10-07
--
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
	[Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Item_Name] [nvarchar] (185) COLLATE database_default NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE database_default NULL,
	[Hint] [nvarchar] (185) COLLATE database_default NULL 
)

--Find all specimens with a determination match
INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences) 
SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
LEFT JOIN DETERMINATION D ON D.Determination_Key=SU.Preferred_Determination_Key
LEFT JOIN Concept C ON D.Concept_Key = C.Concept_Key
LEFT JOIN Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
LEFT JOIN Term TSearch ON TSearch.Term_Key=CSearch.Term_Key
LEFT JOIN TAXON_DETERMINATION TD ON TD.Taxon_Determination_Key=SU.Preferred_Taxon_Determination_Key
LEFT JOIN Index_Taxon_Synonym ITS ON ITS.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
LEFT JOIN INDEX_TAXON_NAME ITN	ON ITS.Synonym_List_Item_Key = ITN.Taxon_List_Item_Key
WHERE 
	(TSearch.Plaintext LIKE @SearchText + '%' AND SU.Life_Sciences=0) 
	OR 
	(ITN.Actual_Name LIKE @SearchText + '%' AND SU.Life_Sciences=1)



UPDATE @SpecimensSearch
SET	Det_Item_Key=
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      CPref.Concept_Key 
	    	ELSE
					TD.Taxon_List_Item_Key					
	    	END,
		Item_Name = 
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      TPref.Item_Name 
	    	ELSE
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames)
				END,
		Number=CUN.Number,
		Det_Item_Name=
				CASE 
					WHEN SU.Life_Sciences=0 THEN TDet.Plaintext 
		    	ELSE ITN.Actual_Name
	    	END,
		Hint=
				CASE 
					WHEN SU.Life_Sciences=0 THEN TDet.Plaintext 
		    	ELSE ITN.Actual_Name
	    	END
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN Specimen_Unit SUnit ON SUnit.Collection_Unit_Key=SU.Item_Key
LEFT JOIN Determination D ON D.Determination_Key=SUnit.Preferred_Determination_Key
LEFT JOIN Concept C ON C.Concept_Key=D.Concept_Key
LEFT JOIN Term TDet ON TDet.Term_Key=C.Term_Key
LEFT JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
LEFT JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key
LEFT JOIN Taxon_Determination TD ON SU.Item_Key = TD.Specimen_Collection_Unit_Key
LEFT JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key

-- Select table and sort appropriately
IF @SortOrderIndex = 0
	SELECT * from @SpecimensSearch
	ORDER BY Det_Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT * from @SpecimensSearch
	ORDER BY Number, Det_Item_Name


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredDetermination') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByPreferredDetermination'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByPreferredRegNumber]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredRegNumber]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredRegNumber] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SearchText VARCHAR(30),
@SortOrderIndex TINYINT

AS
--  DESCRIPTION
--  Returns Specimens data based on the search parameter for Preferred Reg Number
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SearchText			Text to search On
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-10-07
--
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
	[Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE database_default NULL,
	[Hint] [nvarchar] (150) COLLATE database_default NULL 
)

INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, CUN.Number
FROM Specimen_Unit SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
INNER JOIN Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
	AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
	AND CUN.Preferred = 1
	AND CUN.Number LIKE @SearchText + '%'

UPDATE @SpecimensSearch
SET	Det_Item_Key=
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      CPref.Concept_Key 
	    	ELSE
					TD.Taxon_List_Item_Key					
	    	END,
		Item_Name = 
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      TPref.Item_Name 
	    	ELSE
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames)
				END,
		Number=CUN.Number,
		Det_Item_Name=
				CASE 
					WHEN SU.Life_Sciences=0 THEN TDet.Plaintext 
		    	ELSE ITN.Actual_Name
	    	END
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN Specimen_Unit SUnit ON SUnit.Collection_Unit_Key=SU.Item_Key
LEFT JOIN Determination D ON D.Determination_Key=SUnit.Preferred_Determination_Key
LEFT JOIN Concept C ON C.Concept_Key=D.Concept_Key
LEFT JOIN Term TDet ON TDet.Term_Key=C.Term_Key
LEFT JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
LEFT JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key
LEFT JOIN Taxon_Determination TD ON SU.Item_Key = TD.Specimen_Collection_Unit_Key
LEFT JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByPreferredRegNumber'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByType]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByType]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByType] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SearchText VARCHAR(150),
@SortOrderIndex TINYINT

AS
--  DESCRIPTION
--  Returns Specimens data based on the search parameter for Specimen Type
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-10-07
--
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
	[Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE database_default NULL,
	[Hint] [nvarchar] (150) COLLATE database_default NULL 
)

INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, T.Plaintext
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
INNER JOIN Concept C ON C.Concept_Key = SU.Specimen_Type_Concept_Key
INNER JOIN Concept CSyn ON CSyn.Meaning_Key=C.Meaning_Key
INNER JOIN Term T ON T.Term_Key=CSyn.Term_Key
WHERE T.Plaintext LIKE @Searchtext + '%'


UPDATE @SpecimensSearch
SET	Det_Item_Key=
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      CPref.Concept_Key 
	    	ELSE
					TD.Taxon_List_Item_Key					
	    	END,
		Item_Name = 
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      TPref.Item_Name 
	    	ELSE
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames)
				END,
		Number=CUN.Number,
		Det_Item_Name=
				CASE 
					WHEN SU.Life_Sciences=0 THEN TDet.Plaintext 
		    	ELSE ITN.Actual_Name
	    	END
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN Specimen_Unit SUnit ON SUnit.Collection_Unit_Key=SU.Item_Key
LEFT JOIN Determination D ON D.Determination_Key=SUnit.Preferred_Determination_Key
LEFT JOIN Concept C ON C.Concept_Key=D.Concept_Key
LEFT JOIN Term TDet ON TDet.Term_Key=C.Term_Key
LEFT JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
LEFT JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key
LEFT JOIN Taxon_Determination TD ON SU.Item_Key = TD.Specimen_Collection_Unit_Key
LEFT JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForTopLevel]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForTopLevel] 
@UserDomainMask BIGINT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SortOrderIndex TINYINT,
@Key CHAR(16) = NULL
AS

--  DESCRIPTION
--  Returns top level Specimens data to the CollectionsBrowser
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@Key 				Optional Key. When specified, only the single top level record is returned with that key
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-14
--
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
DECLARE @Search TABLE
(
	[Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Item_Name] [nvarchar] (150) COLLATE database_default NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE database_default NULL,
	[Hint] [nvarchar] (150) COLLATE database_default NULL 
)

IF @Key IS NULL
	IF object_id('tempdb..#TempFilter') is not null
		-- Display data for a list of keys in the #TempFilter table
		INSERT INTO 
			@Search (Item_Key, Life_Sciences) 
		SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
		FROM SPECIMEN_UNIT SU
		INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		INNER JOIN #TempFilter ON #TempFilter.ItemKey=CU.Collection_Unit_Key
	ELSE
		INSERT INTO 
			@Search (Item_Key, Life_Sciences) 
		SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
		FROM SPECIMEN_UNIT SU
		INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
ELSE
	-- Display data for a single key
	INSERT INTO 
		@Search (Item_Key, Life_Sciences) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
	FROM SPECIMEN_UNIT SU
	INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE SU.Collection_Unit_Key=@Key


UPDATE @Search
SET	Det_Item_Key=
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      CPref.Concept_Key 
	    	ELSE
					TD.Taxon_List_Item_Key					
	    	END,
		Item_Name = 
		    CASE WHEN SU.Life_Sciences=0 THEN 
		      TPref.Item_Name 
	    	ELSE
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames)
				END,
		Number=CUN.Number,
		Det_Item_Name=
				CASE 
					WHEN SU.Life_Sciences=0 THEN TDet.Plaintext 
		    	ELSE ITN.Actual_Name
	    	END
FROM @Search SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001'
INNER JOIN Specimen_Unit SUnit ON SUnit.Collection_Unit_Key=SU.Item_Key
LEFT JOIN Determination D ON D.Determination_Key=SUnit.Preferred_Determination_Key
LEFT JOIN Concept C ON C.Concept_Key=D.Concept_Key
LEFT JOIN Term TDet ON TDet.Term_Key=C.Term_Key
LEFT JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
LEFT JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key
LEFT JOIN Taxon_Determination TD ON SU.Item_Key = TD.Specimen_Collection_Unit_Key
LEFT JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key

-- Select table and sort appropriately
IF @SortOrderIndex = 0
	SELECT * from @Search
	ORDER BY Det_Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT * from @Search
	ORDER BY Number, Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForTopLevel]
GO

CREATE PROCEDURE [dbo].[usp_Stores_Select_ForTopLevel] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@Key CHAR(16) = NULL

AS

--  DESCRIPTION
--  Returns top level Stores data to the CollectionsBrowser
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@Key 				Optional Key. When specified, only the single top level record is returned with that key
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:		    2003-08-15
--
SET NOCOUNT ON

-- Create  a table to hold the items we are looking for
DECLARE @Search TABLE (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)

IF @Key IS NOT NULL
	INSERT INTO @Search VALUES (@Key)
ELSE IF object_id('tempdb..#TempFilter') is not null
	INSERT INTO @Search SELECT DISTINCT ItemKey FROM #TempFilter
ELSE
	INSERT INTO @Search SELECT Collection_Unit_Key FROM Store

IF @SortOrderIndex = 0
BEGIN
		SELECT S.Collection_Unit_Key AS Item_Key, S.Item_Name, Number
		FROM 
		STORE S
		    INNER JOIN
	    	    COLLECTION_UNIT CU 
		    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	        	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
					OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			LEFT JOIN 
				COLLECTION_UNIT_NUMBER CUN 
			ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
				AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
				AND CUN.Preferred = 1
		INNER JOIN @Search SR ON SR.ItemKey=CU.Collection_Unit_Key
		ORDER BY S.Item_Name, Number
END
ELSE IF @SortOrderIndex = 1
BEGIN
		SELECT S.Collection_Unit_Key AS Item_Key, S.Item_Name, Number
		FROM 
		STORE S
		    INNER JOIN
	    	    COLLECTION_UNIT CU 
		    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	        	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
					OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			LEFT JOIN 
				COLLECTION_UNIT_NUMBER CUN 
			ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
				AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
				AND CUN.Preferred = 1
		INNER JOIN @Search SR ON SR.ItemKey=CU.Collection_Unit_Key
		ORDER BY Number, S.Item_Name
END
ELSE IF @SortOrderIndex = 2
BEGIN
		SELECT S.Collection_Unit_Key AS Item_Key, S.Item_Name, Number, Current_Location_Code AS Hint
		FROM 
		STORE S
		    INNER JOIN
	    	    COLLECTION_UNIT CU 
		    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	        	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
					OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			LEFT JOIN 
				COLLECTION_UNIT_NUMBER CUN 
			ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
				AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
				AND CUN.Preferred = 1
		INNER JOIN @Search SR ON SR.ItemKey=CU.Collection_Unit_Key
		ORDER BY Current_Location_Code, S.Item_Name, Number
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Store_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Store_Get]
GO

/*===========================================================================*\
  Description:	Returns a store's caption

  Parameters:	@Key 
				@Caption OUTPUT

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Store_Get] 
@Key CHAR(16),
@Caption VARCHAR(150) OUTPUT

AS

SET NOCOUNT ON

SELECT @Caption = Item_Name
FROM Store
WHERE Collection_Unit_Key = @Key


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Store_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Store_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Store_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Store_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Store_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Store_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Store_Get TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SurveyEventKey_ForSample_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_SurveyEventKey_ForSample_Get]
GO

/*===========================================================================*\
  Description: 	Retrieve the Survey Event key for a Sample.

  Parameters:	@Key 		Sample key
		@SurveyEventKey Survey Event Key (output)

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_SurveyEventKey_ForSample_Get] 
	@Key char(16),
	@SurveyEventKey char(16) OUTPUT
AS

SET NOCOUNT ON

	SELECT 	@SurveyEventKey = Survey_Event_Key
	FROM	[Sample]
	WHERE	Sample_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventKey_ForSample_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventKey_ForSample_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventKey_ForSample_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_ForSample_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_ForSample_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_ForSample_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_ForSample_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventKey_ForSample_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SurveyEventKey_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SurveyEventKey_Get]
GO
/*===========================================================================*\
  Description: 	Returns a survey event key based on properties of the survey 
	       	event

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyEventKey_Get]
	@Key char(16) OUTPUT,
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@SpatialRef varchar(20),
	@SpatialRefSystem varchar(4),
	@Lat float,
	@Long float,
	@SpatialRefQualifier varchar(20),
	@LocationKey char(16),
	@SurveyKey char(16),
	@LocationName varchar(100)
	
AS
	/*----------------------------------------------------------------------------*\
	  Match exact list of Field Collectors if #TempFieldCollectors has been 
	  created by Collections Browser.
	\*----------------------------------------------------------------------------*/
	IF object_id('tempdb..#TempFieldCollectors') IS NOT NULL
	BEGIN
		/*-----------------------------------------------------------------*\
		  Create a table variable to store the Survey_Event match keys.
		\*-----------------------------------------------------------------*/
		DECLARE @tableSurveyEventMatches TABLE (
			Survey_Event_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS
		)
	
		DECLARE @CurrentFieldCollector char(16)
	
		/*------------------------------------------------------------------*\
		  Create the cursor.
		\*------------------------------------------------------------------*/
		DECLARE curFieldCollectors CURSOR LOCAL FAST_FORWARD FOR
			SELECT 	Name_Key
			FROM	#TempFieldCollectors
	
		OPEN	curFieldCollectors

		/*-------------------------------------------------------*\
		  Give @CurrentFieldCollector its first value.
		\*-------------------------------------------------------*/	
		FETCH NEXT
		FROM	curFieldCollectors
		INTO	@CurrentFieldCollector

		/*-------------------------------------------------------*\
		  Start looping through the field collectors.
		\*-------------------------------------------------------*/		
		WHILE @@Fetch_Status = 0
		BEGIN
			/*------------------------------------------------------------------------*\
			  If the there are no records in @tableSurveyEventMatches, insert all of
			  the Survey_Event records for the first Field Collector in the table.
			\*------------------------------------------------------------------------*/
			IF NOT EXISTS (SELECT * FROM @tableSurveyEventMatches)
			BEGIN
				INSERT INTO @tableSurveyEventMatches (
					Survey_Event_Key
				) 
				SELECT 	Survey_Event_Key
				FROM	Survey_Event_Recorder
				WHERE	Name_Key = @CurrentFieldCollector
			END
			ELSE
			/*------------------------------------------------------------------------*\
			  As there are records in @tableSurveyEventMatches, we can now start
			  removing records that aren't matched.
			\*------------------------------------------------------------------------*/ 
			BEGIN
				-- Delete non matches
				DELETE		@tableSurveyEventMatches
				FROM		@tableSurveyEventMatches AS SEM
				LEFT JOIN	Survey_Event_Recorder AS SER ON SER.Survey_Event_Key = SEM.Survey_Event_Key
									AND SER.Name_Key = @CurrentFieldCollector
				WHERE		SER.Survey_Event_Key IS NULL
			END

			/*---------------------------------------------------------------------*\
			  Get next field collector and put value into @CurrentFieldCollector.
			\*---------------------------------------------------------------------*/		
			FETCH NEXT
			FROM	curFieldCollectors
			INTO	@CurrentFieldCollector
		END
	
		CLOSE curFieldCollectors
		DEALLOCATE curFieldCollectors

		/*---------------------------------------------------------------------*\
		  Now match on Survey, Location, Date and Sample Recorders.
		\*---------------------------------------------------------------------*/		
		SELECT 		@Key = SE.Survey_Event_Key 
		FROM 		Survey_Event AS SE
		INNER JOIN 	@tableSurveyEventMatches AS SEM ON SEM.Survey_Event_Key = SE.Survey_Event_Key
		WHERE		(((Vague_Date_Start = @VagueDateStart) 
					AND	(Vague_Date_End = @VagueDateEnd) 
					AND	(Vague_Date_Type = @VagueDateType)) 
				OR	((Vague_Date_Type = 'U') and (@VagueDateType='U')))
		AND		(IsNull(Spatial_Ref,'') = IsNull(@SpatialRef, ''))
		AND 		(IsNull(Spatial_Ref_System,'') = IsNull(@SpatialRefSystem, ''))
		AND		(IsNull(Lat,'') = IsNull(@Lat, ''))
		AND		(IsNull(Long,'') = IsNull(@Long, ''))
		AND		(IsNull(Spatial_Ref_Qualifier,'') = IsNull(@SpatialRefQualifier, ''))
		AND		(Location_key = @LocationKey)
		AND		(Survey_Key = @SurveyKey)
		AND		(IsNull(Location_Name, '') = IsNull(@LocationName, ''))
	END
	ELSE
		/*---------------------------------------------------------------------*\
		  This matches on Survey, Location and Date. I left this here in case
		  it is required somewhere else and #TempFieldCollectors hasn't been
		  created by the application.
		\*---------------------------------------------------------------------*/	
		SELECT 		@Key = SE.Survey_Event_Key 
		FROM 		Survey_Event AS SE
		WHERE		(((Vague_Date_Start = @VagueDateStart) 
					AND	(Vague_Date_End = @VagueDateEnd) 
					AND	(Vague_Date_Type = @VagueDateType)) 
				OR	((Vague_Date_Type = 'U') and (@VagueDateType='U')))
		AND		(IsNull(Spatial_Ref,'') = IsNull(@SpatialRef, ''))
		AND 		(IsNull(Spatial_Ref_System,'') = IsNull(@SpatialRefSystem, ''))
		AND		(IsNull(Lat,'') = IsNull(@Lat, ''))
		AND		(IsNull(Long,'') = IsNull(@Long, ''))
		AND		(IsNull(Spatial_Ref_Qualifier,'') = IsNull(@SpatialRefQualifier, ''))
		AND		(Location_key = @LocationKey)
		AND		(Survey_Key = @SurveyKey)
		AND		(IsNull(Location_Name, '') = IsNull(@LocationName, ''))
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventKey_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventKey_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Synonym_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Synonym_Delete]
GO

/*===========================================================================*\
  Description:	'Delete' a Synonym. This doesn't actually delete the concept
		that is the selected synonym, but gives the concept a new
		meaning key, so it is no longer a synonym.

  Parameters:	@Key
		@RecordsAffected  OUTPUT

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Synonym_Delete]
	@Key char(16),
	@Timestamp timestamp,
	@RecordsAffected int OUTPUT
AS
SET NOCOUNT OFF
	
	BEGIN TRANSACTION
		DECLARE @MeaningKey char(16),
			@Error int

		/*-------------------------------------------------------------*\
		  Create a new Meaning Key
		\*-------------------------------------------------------------*/
		EXEC spNextKey 'Meaning', @MeaningKey OUTPUT
		IF @@ERROR <> 0 GOTO RollbackAndExit

		INSERT INTO Meaning (
			Meaning_Key
		) VALUES (
			@MeaningKey
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Update the concept record
		\*-------------------------------------------------------------*/
		UPDATE	Concept
		SET 	Meaning_Key = @MeaningKey,	
			List_Preferred = 1
		WHERE	Concept_Key = @Key
		AND	[Timestamp] = @Timestamp

		/*-------------------------------------------------------------*\
		  Get the number of records affected and see if any errors.
		\*-------------------------------------------------------------*/	
		SELECT	@RecordsAffected = @@RowCount,
			@Error = @@Error
		IF @Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Synonym_Delete failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Synonym_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Synonym_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Synonym_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Synonym_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Synonym_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Synonym_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Synonym_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SystemSuppliedData_BCP_Export]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SystemSuppliedData_BCP_Export]
GO

/*===========================================================================*\
  Description:	Exports system supplied data as text files to the given
		output location.

  Parameters:	@OutputPath	Location for output files.

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SystemSuppliedData_BCP_Export]
	@OutputPath varchar(1000) = NULL
AS
	DECLARE @TableName varchar(50),
		@Cmd varchar(500),
		@PrimaryKey varchar(50)

	/*-------------------------------------------------------------*\
		Ensure developer's domains do not transfer through
	\*-------------------------------------------------------------*/
  UPDATE Collection_Unit SET domain_Mask=0 WHERE collection_Unit_Key like 'SYSTEM00%'

	/*-------------------------------------------------------------*\
	  Set default output path, if none provided.
	\*-------------------------------------------------------------*/
	IF @OutputPath IS NULL
		SET @OutputPath = 'C:\Temp\'
	
	/*-------------------------------------------------------------*\
	  Declare a cursor to loop through all the tables with a
	  System Supplied Data field.
	\*-------------------------------------------------------------*/
	DECLARE curTableNames CURSOR LOCAL FAST_FORWARD
	FOR
		SELECT		TableName = Convert(SysName, O.Name)
		FROM		SysObjects O
		INNER JOIN 	SysColumns C ON C.Id = O.Id
		WHERE		O.Type = 'U'
		AND		C.Number = 0
		AND		(C.Name = 'System_Supplied_Data' OR O.NAME IN ('Meaning', 'Language', 'Store', 'Collection_Unit', 'Database_Relationship'))
		ORDER BY 	O.Name

	/*-------------------------------------------------------------*\
	  Run BCP command on each table returned.
	\*-------------------------------------------------------------*/
	OPEN curTableNames
	FETCH NEXT FROM curTableNames INTO @TableName

  WHILE @@Fetch_Status = 0
	BEGIN
		--Obtain table's primary key 
		SELECT @PrimaryKey = KCU.Column_Name
		FROM Information_Schema.Table_Constraints AS TC
		INNER JOIN Information_Schema.Key_Column_Usage AS KCU
		  ON (TC.Table_Catalog = kcu.Table_Catalog and
		         TC.Table_Schema  = kcu.Table_Schema and
		         TC.Table_Name    = kcu.Table_Name and
		         TC.Constraint_Name = kcu.Constraint_Name)
		WHERE TC.Table_Catalog   = 'CollectionsDev'
		AND   TC.Table_Schema    = 'dbo'
		AND   TC.Table_Name      = @TableName
		AND   TC.Constraint_Type = 'PRIMARY KEY'

		IF @TableName IN ('Meaning', 'Store', 'Collection_Unit', 'Database_Relationship')
			SET @Cmd = 'bcp "SELECT * FROM ' + DB_Name() + '.dbo.' + @TableName + ' WHERE ' +
					'LEFT(' + @PrimaryKey + ', 8)=''SYSTEM00''" ' +
			 	 'queryout "' + @OutputPath + @TableName + '.txt" -m50 -T -c -t*@*@ -r!@!@ -S' + @@ServerName
		ELSE IF @TableName = 'Language'
			SET @Cmd = 'bcp "SELECT * FROM ' + DB_Name() + '.dbo.' + @TableName + 
				 '" queryout "' + @OutputPath + @TableName + '.txt" -m50 -T -c -t*@*@ -r!@!@ -S' + @@ServerName
		ELSE IF @TableName = 'Taxon_Dictionary_Name_Type_Mapping'
			SET @Cmd = 'bcp "SELECT * FROM ' + DB_Name() + '.dbo.' + @TableName + ' WHERE ' +
					'System_Supplied_Data = 1" ' +
				 'queryout "' + @OutputPath + @TableName + '.txt" -m50 -T -c -t*@*@ -r!@!@ -S' + @@ServerName
		ELSE
			SET @Cmd = 'bcp "SELECT * FROM ' + DB_Name() + '.dbo.' + @TableName + ' WHERE System_Supplied_Data = 1 AND ' +
					'LEFT(' + @PrimaryKey + ', 8)=''SYSTEM00''" ' +
			 	 'queryout "' + @OutputPath + @TableName + '.txt" -m50 -T -c -t*@*@ -r!@!@ -S' + @@ServerName

		EXEC Master..xp_CmdShell @Cmd, no_output

		FETCH NEXT FROM curTableNames INTO @TableName
	END

	/*-------------------------------------------------------------*\
	  Cleanup.
	\*-------------------------------------------------------------*/
	CLOSE curTableNames
	DEALLOCATE curTableNames
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
GRANT EXECUTE ON [dbo].[usp_SystemSuppliedData_BCP_Export] TO [Public]
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Tasks_Select_ForSearchByIncompleteAndUrgent]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Tasks_Select_ForSearchByIncompleteAndUrgent]
GO

CREATE PROCEDURE [dbo].[usp_Tasks_Select_ForSearchByIncompleteAndUrgent] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Tasks data based on the search parameter for Status
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-15
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT CT.Conservation_Task_Key AS Item_Key, CT.Display_Caption, T.Item_Name AS Hint
	FROM 
	CONSERVATION_TASK CT
		INNER JOIN
			CONSERVATION_CHECK CC
		ON CT.Conservation_Check_Key = CC.Conservation_Check_Key 
			AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
				OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
			AND CT.Priority = 3
			AND CT.Status IN (0, 1) -- Pending or Open
		INNER JOIN 
			CONCEPT C
		ON CT.Type_Concept_Key = C.Concept_Key
		INNER JOIN 
			TERM T
		ON C.Term_Key = T.Term_Key
	ORDER BY CT.Set_Vague_Date_Start DESC, CT.Set_Vague_Date_End DESC, CT.Set_Vague_Date_Type, 
			dbo.ufn_GetConservationStatus(Status)

ELSE IF @SortOrderIndex = 1
	SELECT CT.Conservation_Task_Key AS Item_Key, CT.Display_Caption, T.Item_Name AS Hint
	FROM 
	CONSERVATION_TASK CT
		INNER JOIN
			CONSERVATION_CHECK CC
		ON CT.Conservation_Check_Key = CC.Conservation_Check_Key 
			AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
				OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
			AND CT.Priority = 3
			AND CT.Status IN (0, 1) -- Pending or Open
		INNER JOIN 
			CONCEPT C
		ON CT.Type_Concept_Key = C.Concept_Key
		INNER JOIN 
			TERM T
		ON C.Term_Key = T.Term_Key
	ORDER BY dbo.ufn_GetConservationStatus(Status), CT.Set_Vague_Date_Start DESC, CT.Set_Vague_Date_End DESC, CT.Set_Vague_Date_Type
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Tasks_Select_ForSearchByIncompleteAndUrgent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Tasks_Select_ForSearchByIncompleteAndUrgent'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByIncompleteAndUrgent TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByIncompleteAndUrgent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByIncompleteAndUrgent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByIncompleteAndUrgent TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByIncompleteAndUrgent TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByIncompleteAndUrgent TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Tasks_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Tasks_Select_ForTopLevel]
GO

CREATE PROCEDURE [dbo].[usp_Tasks_Select_ForTopLevel] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@Key CHAR(16) = NULL

AS
--  DESCRIPTION
--  Returns Tasks to the top level of the CollectionsBrowser
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@Key 				Optional Key. When specified, only the single top level record is returned with that key
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-19
--
SET NOCOUNT ON

-- Create  a table to hold the items we are looking for
DECLARE @Search TABLE (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)

IF @Key IS NOT NULL
		INSERT INTO @Search VALUES (@Key)
ELSE IF object_id('tempdb..#TempFilter') is not null
	INSERT INTO @Search SELECT DISTINCT ItemKey FROM #TempFilter
ELSE
	INSERT INTO @Search SELECT Conservation_Task_Key FROM Conservation_Task

IF @SortOrderIndex = 0	
BEGIN
		SELECT CT.Conservation_Task_Key AS Item_Key, CT.Display_Caption
		FROM 
		CONSERVATION_TASK CT
			INNER JOIN
				CONSERVATION_CHECK CC
			ON CT.Conservation_Check_Key = CC.Conservation_Check_Key 
				AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
					OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
		INNER JOIN @Search S ON S.ItemKey=CT.Conservation_Task_Key
		ORDER BY CT.Set_Vague_Date_Start DESC, CT.Set_Vague_Date_End DESC, CT.Set_Vague_Date_Type, 
			dbo.ufn_GetConservationStatus(Status)
END
ELSE IF @SortOrderIndex = 1
BEGIN
		SELECT CT.Conservation_Task_Key AS Item_Key, CT.Display_Caption
		FROM 
		CONSERVATION_TASK CT
			INNER JOIN
				CONSERVATION_CHECK CC
			ON CT.Conservation_Check_Key = CC.Conservation_Check_Key 
				AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
					OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
		INNER JOIN @Search S ON S.ItemKey=CT.Conservation_Task_Key
		ORDER BY dbo.ufn_GetConservationStatus(Status),
			CT.Set_Vague_Date_Start DESC, CT.Set_Vague_Date_End DESC, CT.Set_Vague_Date_Type
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Tasks_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Tasks_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Tasks_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Tasks_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Task_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Task_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Conservation_Task table

  Parameters:	@Key
		@ConservationCheckKey 
		@ConservationJobKey 
		@SetVagueDateStart
		@SetVagueDateEnd
		@SetVagueDateType
		@Status
		@TypeConceptKey
		@Priority
		@Duration
		@DurationUnitConceptKey
		@IdentifierNameKey
		@TaskAction 
		@Comment
		@SessionID
	

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Task_Insert]
	@Key char(16) OUTPUT,
	@ConservationCheckKey char(16), 
	@ConservationJobKey char(16),
	@SetVagueDateStart int, 
	@SetVagueDateEnd int, 
	@SetVagueDateType varchar(2),
	@Status tinyint, 
	@TypeConceptKey char(16), 
	@Priority tinyint, 
	@Duration float, 
	@DurationUnitConceptKey char(16),
	@IdentifierNameKey char(16), 
	@TaskAction text, 
	@Comment text, 
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Conservation_Task', @Key OUTPUT

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Conservation_Task.
		\*-------------------------------------------------------------*/
		INSERT INTO Conservation_Task (
			Conservation_Task_Key, Conservation_Check_Key, Conservation_Job_Key,
			Set_Vague_Date_Start, Set_Vague_Date_End, Set_Vague_Date_Type,
			Status, Type_Concept_Key, Priority, Duration, Duration_Unit_Concept_Key,
			Identifier_Name_Key, Task_Action, Comment, Entered_Session_ID
			
		) VALUES (
			@Key, @ConservationCheckKey, @ConservationJobKey,
			@SetVagueDateStart, @SetVagueDateEnd, @SetVagueDateType,
			@Status, @TypeConceptKey, @Priority, @Duration, @DurationUnitConceptKey,
			@IdentifierNameKey, @TaskAction, @Comment, @SessionID
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*---------------------------------------------------------------------------------*\
		  If the Conservation Check has just one task linked to it, we will want to duplicate 
		  records from the Collection_Unit_Check table into the Collection_Unit_Table.
		  Here we are going to find out how many Tasks are linked to the Conservation_Check.
		  If it is just one, then we have stored this task key in @TaskKey and can run
		  usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck which will duplicate the 
		  records for us.
		\*---------------------------------------------------------------------------------*/

		DECLARE @TaskKey char(16)

		SELECT 	@TaskKey = Conservation_Task_Key
		FROM	Conservation_Task
		WHERE	Conservation_Check_Key = @ConservationCheckKey

		IF @@RowCount = 1
		BEGIN
			EXEC usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck @TaskKey, @ConservationCheckKey, @SessionID
		END


	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Task_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Task_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Task_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Task_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Task_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Task_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Task_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Task_Update_ForConditionCheck]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Task_Update_ForConditionCheck]
GO

/*===========================================================================*\
  Description:	Updates the record in the Conservation_Task table with the 
		key of the parent Conservation_Check_Key.

  Parameters:	@ParentKey 	The key of the parent ConservationCheck node.
		@ChildKey	The key of the added (child) Task node. 

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Task_Update_ForConditionCheck] 
	@ParentKey CHAR(16),
	@ChildKey CHAR(16),
	@SessionID CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE	@TaskKey char(16)

	BEGIN TRANSACTION

		UPDATE	Conservation_Task
		SET	Conservation_Check_Key = @ParentKey
		WHERE	Conservation_Task_Key = @ChildKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		/*---------------------------------------------------------------------------------*\
		  If the Conservation Check has just one task linked to it, we will want to duplicate 
		  records from the Collection_Unit_Check table into the Collection_Unit_Table.
		  Here we are going to find out how many Tasks are linked to the Conservation_Check.
		  If it is just one, then we have stored this task key in @TaskKey and can run
		  usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck which will duplicate the 
		  records for us.
		\*---------------------------------------------------------------------------------*/

		SELECT 	@TaskKey = Conservation_Task_Key
		FROM	Conservation_Task
		WHERE	Conservation_Check_Key = @ParentKey

		IF @@RowCount = 1
		BEGIN
			EXEC usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck @TaskKey, @ParentKey, @SessionID
		END

		IF @@Error <> 0 GOTO RollbackAndExit
	
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Task_Update_ForConditionCheck') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Task_Update_ForConditionCheck'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Task_Update_ForConditionCheck TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Task_Update_ForConditionCheck TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Task_Update_ForConditionCheck TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Task_Update_ForConditionCheck TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Task_Update_ForConditionCheck TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonDetermination_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonDetermination_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record in the Taxon Determination table.
		Ensures the Domain mask of the specimen is also updated.

  Parameters:	@DeterminationKey
		@IsForSpecimen

  Created:	November 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonDetermination_Delete]
	@DeterminationKey char(16),
	@IsForSpecimen bit,
	@Timestamp timestamp
AS
	DECLARE @SpecimenKey char(16),
		@ConceptKey char(16),
		@ConceptMask bigint,
		@OccurrenceKey char(16),
		@WasPreferredForOccurrence bit,
		@WasPreferredForSpecimen bit

	SET @WasPreferredForSpecimen = 0
	SET @WasPreferredForOccurrence = 0

	BEGIN TRANSACTION
		/*-------------------------------------------------------------*\
		  Set some values to search for new preferred, if deleting 
		  current preferred.
		\*-------------------------------------------------------------*/
		-- Check for Specimen.
		IF EXISTS(SELECT * FROM Specimen_Unit WHERE Preferred_Taxon_Determination_Key = @DeterminationKey) BEGIN
			SELECT	@WasPreferredForSpecimen = 1,
				@SpecimenKey = Collection_Unit_Key
			FROM	Specimen_Unit
			WHERE	Preferred_Taxon_Determination_Key = @DeterminationKey

			-- Need to clear the field because of referential integrity.
			UPDATE 	Specimen_Unit
			SET	Preferred_Taxon_Determination_Key = NULL
			WHERE	Collection_Unit_Key = @SpecimenKey
			IF @@Error <> 0 GOTO RollbackAndExit
		END

		-- Check for Occurrence.
		SELECT		@ConceptKey = TDM.Concept_Key,
				@OccurrenceKey = TD.Taxon_Occurrence_Key,
				@WasPreferredForOccurrence = TD.Preferred,
				@SpecimenKey = TD.Specimen_Collection_Unit_Key
		FROM		Taxon_Determination TD
		INNER JOIN 	Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
		WHERE		TD.Taxon_Determination_Key = @DeterminationKey

		/*-------------------------------------------------------------*\
		  Delete either from Determination table, or just the link.
		\*-------------------------------------------------------------*/
		IF @OccurrenceKey IS NOT NULL AND @SpecimenKey IS NOT NULL 
			-- Determination linked to both a specimen AND an occurrence
			IF @IsForSpecimen = 1
				-- Deleting for specimen, clear the link and keep Occurrence key
				UPDATE	Taxon_Determination_Key
				SET	Specimen_Collection_Unit_Key = NULL
				WHERE	Taxon_Determination_Key = @DeterminationKey
				AND	 (@Timestamp = Timestamp)
			ELSE
				-- Deleting for occurrence, clear the link and keep Spceimen key
				UPDATE	Taxon_Determination_Key
				SET	Occurrence_Key = NULL
				WHERE	Taxon_Determination_Key = @DeterminationKey
				AND	 (@Timestamp = Timestamp)
		ELSE
			-- Only either specimen or occurrence key, safe to delete.
			DELETE	Taxon_Determination
			WHERE	Taxon_Determination_Key = @DeterminationKey
			AND	 (@Timestamp = Timestamp)

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  See if preferred flags need to be updated, and where.
		\*-------------------------------------------------------------*/
		-- Retrieve the mask of the concept from deleted determination.
		EXECUTE	usp_Get_Concept_Domain_Mask @ConceptKey, @ConceptMask OUTPUT

		-- Need to look for a new preferred determination. Default to first one found.
		IF @WasPreferredForOccurrence = 1 OR @WasPreferredForSpecimen = 1 BEGIN
			DECLARE	@NewConceptKey char(16),
				@NewConceptMask bigint,
				@NewPreferredDeterminationKey char(16)

			-- Get new preferred first, if there are determinations left to choose from.
			IF EXISTS(SELECT * FROM Taxon_Determination WHERE Taxon_Occurrence_Key = @OccurrenceKey)
			BEGIN
				-- For occurrences, it all happens in Taxon_Determination table.
				IF @WasPreferredForOccurrence = 1 BEGIN
					SELECT	TOP 1 @NewPreferredDeterminationKey = Taxon_Determination_Key
					FROM	Taxon_Determination
					WHERE	Taxon_Occurrence_Key = @OccurrenceKey

					UPDATE 	Taxon_Determination
					SET	Preferred = 1
					WHERE	Taxon_Determination_Key = @NewPreferredDeterminationKey
					IF @@Error <> 0 GOTO RollbackAndExit
				END

				-- For specimen, it happens in Specimen_Unit table.
				IF @WasPreferredForSpecimen = 1 BEGIN
					SELECT	TOP 1 @NewPreferredDeterminationKey = Taxon_Determination_Key
					FROM	Taxon_Determination
					WHERE	Specimen_Collection_Unit_Key = @SpecimenKey

					UPDATE 	Specimen_Unit
					SET	Preferred_Taxon_Determination_Key = @NewPreferredDeterminationKey
					WHERE	Collection_Unit_Key = @SpecimenKey
					IF @@Error <> 0 GOTO RollbackAndExit
				END

				-- Get concept key of new preferred determination
				SELECT		@NewConceptKey = TDM.Concept_Key
				FROM		Taxon_Determination TD
				INNER JOIN	Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
				WHERE		Taxon_Determination_Key = @NewPreferredDeterminationKey
			END

			-- Retrieve the mask of the concept from deleted determination.
			EXECUTE	usp_Get_Concept_Domain_Mask @NewConceptKey, @NewConceptMask OUTPUT

			-- Different mask, so switch old one OFF and new one ON.
			IF @SpecimenKey IS NOT NULL AND @ConceptMask <> @NewConceptMask
			BEGIN
				EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenKey, @ConceptMask, 0
				IF @@Error <> 0 GOTO RollbackAndExit

				EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenKey, @NewConceptMask, 1
				IF @@Error <> 0 GOTO RollbackAndExit
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDetermination_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonDetermination_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonListItem_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonListItem_Get]
GO

/*===========================================================================*\
  Description:	Return the plaintext caption of a taxon from a taxon list item 
  Parameters:	@Key  
			@Caption OUTPUT

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonListItem_Get] 
@Key CHAR(16),
@Caption VARCHAR(150) OUTPUT

AS


SET NOCOUNT ON

SELECT @Caption = Actual_Name
FROM Index_Taxon_Name ITN
WHERE ITN.Taxon_List_Item_Key=@Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonListItem_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonListItem_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonListItem_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonListItem_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonListItem_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonListItem_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon list items corresponding to the concepts in a
				concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 3 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonListItem_ImportConceptGroup]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key		CHAR(16),
				@concept_key			CHAR(16),
				@taxon_list_item_key	CHAR(16),
				@term_key				CHAR(16),
				@term_version_key		CHAR(16),
				@list_preferred			BIT,
				@meaning_key			CHAR(16),
				@taxon_key				CHAR(16),
				@taxon_version_key		CHAR(16),
				@preferred_name			CHAR(16),
				@sort_code				INT,
				@taxon_rank_key			CHAR(16),
				@ins_user_key			CHAR(16),
				@ins_date				SMALLDATETIME,
				@upd_user_key			CHAR(16),
				@upd_date				SMALLDATETIME,
				@system					BIT,
				@from_list_version_key	CHAR(16),
				@to_list_version_key	CHAR(16),
				@is_new					BIT

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
													'Exporting concepts'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		concepts	CURSOR FAST_FORWARD LOCAL FOR
	SELECT		c.Concept_Key,
				c.Term_Key,
				c.Term_Version_Key,
				c.List_Preferred,
				c.Meaning_Key,
				c.Sort_Code,
				crm.Taxon_Rank_Key,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				c.System_Supplied_Data
	FROM		Concept									AS	c
	INNER JOIN	Taxon_Dictionary_Concept_Rank_Mapping	AS	crm
	ON			crm.Concept_Rank_Key					=	c.Concept_Rank_Key
	INNER JOIN	Session									AS	es
	ON			es.Session_ID							=	c.Entered_Session_ID
	LEFT JOIN	Session									AS	cs
	ON			cs.Session_ID							=	c.Changed_Session_ID
	WHERE		c.Concept_Group_Key						=	@concept_group_key
	ORDER BY	c.List_Preferred DESC	/* i.e. preferred names first */

	OPEN		concepts

	WHILE 1 = 1
	BEGIN
		FETCH		concepts
		INTO		@concept_key,
					@term_key,
					@term_version_key,
					@list_preferred,
					@meaning_key,
					@sort_code,
					@taxon_rank_key,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SET ROWCOUNT 1

		SELECT      @from_list_version_key							=	gvm.Taxon_List_Version_Key
		FROM		Concept_History									AS	h
		INNER JOIN	Concept_Group_Version							AS	gv
		ON			gv.Concept_Group_Version_Key					=	h.Concept_Group_Version_From
		INNER JOIN	Taxon_Dictionary_Concept_Group_Version_Mapping	AS	gvm
		ON			gvm.Concept_Group_Version_Key					=	gv.Concept_Group_Version_Key
		WHERE		h.Concept_Key									=	@concept_key
		ORDER BY	gv.Sequence

		SELECT		@to_list_version_key							=	gvm.Taxon_List_Version_Key
		FROM		Concept_History									AS	h
		INNER JOIN	Concept_Group_Version							AS	gv
		ON			gv.Concept_Group_Version_Key					=	h.Concept_Group_Version_To
		INNER JOIN	Taxon_Dictionary_Concept_Group_Version_Mapping	AS	gvm
		ON			gvm.Concept_Group_Version_Key					=	gv.Concept_Group_Version_Key
		WHERE		h.Concept_Key									=	@concept_key
		ORDER BY	gv.Sequence DESC

		IF @@ROWCOUNT = 0
		BEGIN
			SET			@to_list_version_key	=	NULL
		END

		/* we do the term and term version mappings inside the 'SET ROWCOUNT 1'
		 * because a single term may map onto multiple taxa; we need to choose
		 * exactly one, but any one will do
		 */
		SELECT		@taxon_key						=	Taxon_Key
		FROM		Taxon_Dictionary_Term_Mapping
		WHERE		Term_Key						=	@term_key

		IF @@ROWCOUNT = 0 GOTO skip_item

		IF @term_version_key IS NULL
		BEGIN
			SET			@taxon_version_key	=	NULL
		END
		ELSE
		BEGIN
			SELECT		@taxon_version_key						=	m.Taxon_Version_Key
			FROM		Taxon_Dictionary_Term_Version_Mapping	AS	m
			INNER JOIN	TAXON_VERSION							AS	tv
			ON			tv.TAXON_VERSION_KEY					=	m.Taxon_Version_Key
			WHERE		m.Term_Version_Key						=	@term_version_key
			AND			tv.TAXON_KEY							=	@taxon_key

			IF @@ROWCOUNT = 0 GOTO skip_item				
		END

		SET ROWCOUNT 0

		/* check for existing mapping */
		SELECT		@taxon_list_item_key				=	Taxon_List_Item_Key
		FROM		Taxon_Dictionary_Concept_Mapping
		WHERE		Concept_Key							=	@concept_key

		SET			@is_new		=	CASE WHEN @@ROWCOUNT = 0 THEN 1 ELSE 0 END

		IF @is_new = 1
		BEGIN
			EXECUTE		spNextKey	'TAXON_LIST_ITEM',
									@taxon_list_item_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* work out preferred name */
		IF @list_preferred = 1
		BEGIN
			SET			@preferred_name		=	@taxon_list_item_key
		END
		ELSE
		BEGIN
			SET			@preferred_name						=	NULL

			SET ROWCOUNT 1

			SELECT		@preferred_name						=	m.Taxon_List_Item_Key
			FROM		Concept								AS	c
			INNER JOIN 	Taxon_Dictionary_Concept_Mapping	AS	m
			ON			m.Concept_Key						=	c.Concept_Key
			WHERE		c.Concept_Group_Key					=	@concept_group_key
			AND			c.Meaning_Key						=	@meaning_key
			AND			c.List_Preferred					=	1

			SET ROWCOUNT 0
		END

		IF @preferred_name IS NOT NULL AND @taxon_version_key IS NULL
		BEGIN
			/* create a minimal taxon version */
			EXECUTE		spNextKey	'TAXON_VERSION',
									@taxon_version_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_VERSION (
						TAXON_VERSION_KEY,
						TAXON_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_version_key,
						@taxon_key,
						@ins_user_key,
						@ins_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		IF @is_new = 0
		BEGIN
			IF @preferred_name IS NULL
			BEGIN
				/* concept is not selectable; remove any list items */
				DELETE		TAXON_LIST_ITEM
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
			ELSE
			BEGIN
				/* update taxon list item */
				UPDATE		TAXON_LIST_ITEM
				SET			TAXON_VERSION_KEY		=	@taxon_version_key,
							TAXON_LIST_VERSION_KEY	=	@from_list_version_key,
							TAXON_LIST_VERSION_TO	=	@to_list_version_key,
							PREFERRED_NAME			=	@preferred_name,
							SORT_CODE				=	@sort_code,
							TAXON_RANK_KEY			=	@taxon_rank_key,
							ENTERED_BY				=	@ins_user_key,
							ENTRY_DATE				=	@ins_date,
							CHANGED_BY				=	@upd_user_key,
							CHANGED_DATE			=	@upd_date,
							SYSTEM_SUPPLIED_DATA	=	@system
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* create taxon list item */
			INSERT		TAXON_LIST_ITEM (
						TAXON_LIST_ITEM_KEY,
						TAXON_VERSION_KEY,
						TAXON_LIST_VERSION_KEY,
						TAXON_LIST_VERSION_TO,
						PREFERRED_NAME,
						SORT_CODE,
						TAXON_RANK_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_list_item_key,
						@taxon_version_key,
						@from_list_version_key,
						@to_list_version_key,
						@preferred_name,
						@sort_code,
						@taxon_rank_key,
						@ins_user_key,
						@ins_date,
						@upd_user_key,
						@upd_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Mapping (
						Taxon_List_Item_Key,
						Concept_key)
			VALUES		(@taxon_list_item_key,
						@concept_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		
skip_item:
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		concepts
	DEALLOCATE	concepts
	RETURN

fail_from_cursor:
	CLOSE		concepts
	DEALLOCATE	concepts

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonListItem_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
GRANT EXECUTE ON [dbo].[usp_TaxonListItem_ImportConceptGroup] TO [Public]
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonList_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonList_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import a concept group as a taxon list.

  Parameters:   @job_id					Job identifier
				@taxon_list_key			Taxon list key
				@concept_group_key		Concept group key

  Created:		Dec 2003

  Last revision information:
	$Revision: 3 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonList_ImportConceptGroup]
	@job_id				INT,
	@taxon_list_key		CHAR(16),
	@concept_group_key	CHAR(16),
	@SessionID	CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE		@existing_list_key		CHAR(16)

	SELECT		@existing_list_key						=	Taxon_List_Key
	FROM		Taxon_Dictionary_Concept_Group_Mapping
	WHERE		Concept_Group_Key						=	@concept_group_key

	IF @@ROWCOUNT = 0
	BEGIN
		/* record mapping */
		INSERT		Taxon_Dictionary_Concept_Group_Mapping (
					Taxon_List_Key,
					Concept_Group_Key)
		VALUES		(@taxon_list_key,
					@concept_group_key)

		IF @@ERROR <> 0 RETURN
	END
	ELSE IF @existing_list_key <> @taxon_list_key
	BEGIN
		RAISERROR (
			'Concept group has previously been imported into a different taxon list',
			16,
			1)
		RETURN
	END

	/* Calculate size of job */
	DECLARE		@record_count			INT

	SELECT		@record_count			=	COUNT(*)
	FROM		Concept_Group_Version
	WHERE		Concept_Group_Key		=	@concept_group_key

	SELECT		@record_count							=	@record_count
															+ COUNT(DISTINCT c.Name_Type_Concept_Key)
															+ COUNT(DISTINCT c.Term_Key)
															+ COUNT(DISTINCT c.Term_Version_Key)
															+ COUNT(DISTINCT j.Source_Join_Key)
															+ COUNT(DISTINCT c.Concept_Rank_Key)
															+ COUNT(DISTINCT c.Concept_Key)
															+ COUNT(DISTINCT d.Designation_Type_Concept_Key)
															+ COUNT(DISTINCT d.Concept_Designation_Key)
															+ COUNT(DISTINCT f.Thesaurus_Fact_Key
																	+ vm.Term_Version_Key)
	FROM		Concept									AS	c
	LEFT JOIN	Source_Join								AS	j
	ON			j.Record_Key							=	c.Term_Key
	AND			j.Table_Name							=	'Term'
	LEFT JOIN	Concept_Designation						AS	d
	ON			d.Concept_Key							=	c.Concept_Key
	LEFT JOIN	Thesaurus_Fact							AS	f
	ON			f.Meaning_Key							=	c.Meaning_Key
	LEFT JOIN	Taxon_Dictionary_Term_Version_Mapping	AS	vm
	ON			vm.Term_Version_Key						=	c.Term_Version_Key
	WHERE		c.Concept_Group_Key						=	@concept_group_key

	EXECUTE		usp_Import_Export_Job_Configure		@job_id,
													@concept_group_key,
													@record_count
	IF @@ERROR <> 0 RETURN
	
	/* import versions */
	EXECUTE		usp_TaxonListVersion_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import name types */
	EXECUTE		usp_TaxonNameType_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxa */
	EXECUTE		usp_Taxon_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxon versions */
	EXECUTE		usp_TaxonVersion_ImportConceptGroup		@job_id, @SessionID
	IF @@ERROR <> 0 RETURN

	/* import taxon/source relationships */
	EXECUTE		usp_TaxonSources_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxon ranks */
	EXECUTE		usp_TaxonRank_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxon list items */
	EXECUTE		usp_TaxonListItem_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import hierarchical relationships */
	EXECUTE		usp_TaxonListItem_ImportRelationships	@job_id
	IF @@ERROR <> 0 RETURN

	/* import designation types */
	EXECUTE		usp_TaxonDesignationType_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import designations */
	EXECUTE		usp_TaxonDesignation_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import facts */
	EXECUTE		usp_TaxonFact_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN 

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Updating Taxon Names Index...'
	/* Discard Index_Taxon_Name records for the concept group */
	DELETE ITN
	FROM Index_Taxon_Name ITN
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM.Concept_Key
			AND C1.Concept_Group_Key=@Concept_Group_Key
	
	/* Rebuild Index_Taxon_Name for the concept group */
	INSERT INTO Index_Taxon_Name (Taxon_List_Item_Key, Taxon_List_Version_Key,
	 Actual_Name, Actual_Name_Italic, Common_Name, Common_Name_Italic, 
	  Preferred_Name, Preferred_Name_Italic, Abbreviation, Authority, System_Supplied_Data )
	SELECT TLI.Taxon_List_Item_Key, TLI.Taxon_List_Version_Key, 
	  T.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T.Language = 'La' THEN 1 ELSE 0 END, 
	  T2.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T2.Language = 'La' THEN 1 ELSE 0 END, 
	  T3.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T3.Language = 'La' THEN 1 ELSE 0 END, 
	  T.Abbreviation, T.Authority, 1 
	FROM ((((((((Taxon_List_Item AS TLI 
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key=TLI.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM.Concept_Key
			AND C1.Concept_Group_Key=@Concept_Group_Key
	LEFT JOIN Taxon_version AS TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T ON T.Taxon_Key = TV.Taxon_Key) 
	LEFT JOIN Taxon_Common_Name AS TCN ON TCN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key) 
	LEFT JOIN Taxon_Version AS TV2 ON TV2.Taxon_Version_Key = TCN.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T2 ON T2.Taxon_Key = TV2.Taxon_Key) 
	LEFT JOIN Taxon_List_Item AS TLI3 ON TLI3.Taxon_List_Item_Key = TLI.Preferred_Name) 
	LEFT JOIN Taxon_Rank AS TR3 ON TR3.Taxon_Rank_Key = TLI3.Taxon_Rank_Key) 
	LEFT JOIN Taxon_Version AS TV3 ON TV3.Taxon_Version_Key = TLI3.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T3 ON T3.Taxon_Key = TV3.Taxon_Key 
	WHERE TLI.Taxon_List_Version_To IS NULL
	
	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting Taxon Common Names...'	

	/* Update any existing taxon common name records */
	UPDATE Taxon_Common_Name
	SET Taxon_Version_Key=TLI.Taxon_Version_Key
	FROM Taxon_Dictionary_Concept_Mapping TDM1 
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM1.Concept_Key
			AND C1.Concept_Group_Key=@Concept_Group_Key
	LEFT JOIN (
			Concept C2 
			INNER JOIN Term T ON T.Term_Key=C2.Term_Key
			INNER JOIN Language L ON L.Language_Key=T.Language_Key AND L.Priority=1
		) ON C2.Meaning_Key=C1.Meaning_Key
			AND C2.Preferred=1
			AND C2.Name_Type_Concept_Key='SYSTEM000000000L'
	LEFT JOIN Concept C3 ON C3.Meaning_Key=C1.Meaning_Key
		AND C3.List_Preferred=1
		AND C3.Concept_Group_Key=C1.Concept_Group_Key
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM2 ON TDM2.Concept_Key=ISNULL(C2.Concept_Key, C3.Concept_Key)
	INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=TDM2.Taxon_List_Item_Key
	LEFT JOIN Taxon_Common_Name TCN ON TCN.Taxon_List_Item_Key=TDM1.Taxon_List_Item_Key
	WHERE TCN.Taxon_Version_Key<>TLI.Taxon_Version_Key
		
	/* Insert any new required taxon common name records */
	INSERT INTO Taxon_Common_Name
	SELECT DISTINCT TDM1.Taxon_list_item_key, TLI.Taxon_Version_Key
	FROM Taxon_Dictionary_Concept_Mapping TDM1 
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM1.Concept_Key
			AND C1.Concept_Group_Key=@Concept_Group_Key
	LEFT JOIN (
			Concept C2 
			INNER JOIN Term T ON T.Term_Key=C2.Term_Key
			INNER JOIN Language L ON L.Language_Key=T.Language_Key AND L.Priority=1
		) ON C2.Meaning_Key=C1.Meaning_Key
			AND C2.Preferred=1
			AND C2.Name_Type_Concept_Key='SYSTEM000000000L'
	LEFT JOIN Concept C3 ON C3.Meaning_Key=C1.Meaning_Key
		AND C3.List_Preferred=1
		AND C3.Concept_Group_Key=C1.Concept_Group_Key
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM2 ON TDM2.Concept_Key=ISNULL(C2.Concept_Key, C3.Concept_Key)
	INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=TDM2.Taxon_List_Item_Key
	LEFT JOIN Taxon_Common_Name TCN ON TCN.Taxon_List_Item_Key=TDM1.Taxon_List_Item_Key
	WHERE TCN.Taxon_List_Item_Key IS NULL


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
/* TODO: correct permissions */
GRANT EXECUTE ON [dbo].[usp_TaxonList_ImportConceptGroup] TO [Public]
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonOccurrences_Select_ForSearch]')
	   AND 	  Type = 'P')
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

	SELECT		XO.Taxon_Occurrence_Key AS [Item_Key], 
		ITN.Actual_Name + ' - ' +
			dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) +
			' ' + CASE WHEN S.Spatial_Ref IS NULL THEN '' ELSE S.Spatial_Ref END
		AS SearchTerm,
		CASE ITN.Actual_Name_Italic
			WHEN 1 THEN '<i>' + ITN.Actual_Name + '</i>'
			ELSE ITN.Actual_Name END + ' - ' +
			dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) +
			' ' + CASE WHEN S.Spatial_Ref IS NULL THEN '' ELSE S.Spatial_Ref END
		AS DisplayTerm
	FROM		Taxon_Occurrence XO
	INNER JOIN	Taxon_Determination TD ON XO.Taxon_Occurrence_Key = TD.Taxon_Occurrence_Key
			AND TD.Preferred=1
	INNER JOIN	Taxon_Dictionary_Concept_Mapping TDCM ON TDCM.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
	INNER JOIN	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
	INNER JOIN Sample S ON S.Sample_Key=XO.Sample_Key
	WHERE		ITN.Actual_Name LIKE @SearchText + '%'
	ORDER BY	ITN.Actual_Name

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
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonVersion_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonVersion_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon versions corresponding to items in a concept
				group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 3 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonVersion_ImportConceptGroup]
	@job_id					INT,
	@SessionID			CHAR(16)
AS
	SET NOCOUNT ON

 DECLARE     @concept_group_key			CHAR(16),
				@term_version_key			CHAR(16),
				@taxon_key					CHAR(16),
				@attribute					VARCHAR(10),
				@entered_by					CHAR(16),
				@entry_date					SMALLDATETIME,
				@changed_by					CHAR(16),
				@changed_date				SMALLDATETIME,
				@system						BIT,
				@taxon_version_key			CHAR(16),
				@source_key					CHAR(16),
				@source_join_key			CHAR(16),
				@prior_term_version_key		CHAR(16),
				@concept_key	CHAR(16),
				@term_key		CHAR(16)

SET @Concept_Group_Key='DSS0039400000003'
SET @SessionID='DSS0039400000001'
 

	DECLARE		versions	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tv.Term_Version_Key,
				tm.Taxon_Key,
				tv.Version_Label,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				ISNULL(tv.System_Supplied_Data, c.System_Supplied_Data),
				c.Concept_Key,
				tm.Term_key
	FROM		Concept							AS	c
	LEFT JOIN	Term_Version					AS	tv
	ON			tv.Term_Version_Key				=	c.Term_Version_Key
	INNER JOIN	Taxon_Dictionary_Term_Mapping	AS	tm
	ON			tm.Term_Key						=	c.Term_Key
	INNER JOIN	Session							AS	es
	ON			es.Session_ID					=	 ISNULL(tv.Entered_Session_ID, @SessionID)
	LEFT JOIN	Session							AS	cs
	ON			cs.Session_ID					=	tv.Changed_Session_ID
	WHERE		c.Concept_Group_Key				=	@concept_group_key
	ORDER BY	tv.Term_Version_Key

	SET			@prior_term_version_key			=	''

	OPEN		versions

	WHILE 1 = 1
	BEGIN
		FETCH		versions
		INTO		@term_version_key,
					@taxon_key,
					@attribute,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date,
					@system,
					@concept_key,
					@term_key

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT      @taxon_version_key						=	NULL,
					@source_join_key						=	NULL,
					@source_key								=	NULL

		SELECT		@taxon_version_key						=	m.Taxon_Version_Key,
					@source_key								=	j.Source_Key
		FROM		Taxon_Dictionary_Term_Version_Mapping	AS	m
		LEFT JOIN	Source_Join								AS	j
		ON			j.Source_Join_Key						=	m.Source_Join_Key
		WHERE		m.Term_Version_Key						=	@term_version_key

		IF @source_key IS NULL
		BEGIN
			/* there is no existing mapping for the source join; pick an
			 * arbitrary join record (if there are any) and make this the
			 * mapped join.
			 */
			SELECT		@source_join_key	=	Source_Join_Key,
						@source_key			=	Source_Key
			FROM		Source_Join
			WHERE		Record_Key			=	@term_version_key
			AND			Table_Name			=	'Term_Version'
			ORDER BY	Source_Join_Key
		END

		IF @taxon_version_key IS NOT NULL
		BEGIN
			/* update taxon version */
			UPDATE		TAXON_VERSION
			SET			TAXON_KEY				=	@taxon_key,
						ATTRIBUTE				=	@attribute,
						SOURCE_KEY				=	@source_key,
						ENTERED_BY				=	@entered_by,
						ENTRY_DATE				=	@entry_date,
						CHANGED_BY				=	@changed_by,
						CHANGED_DATE			=	@changed_date,
						SYSTEM_SUPPLIED_DATA   	=	@system
			WHERE		TAXON_VERSION_KEY		=	@taxon_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @source_join_key IS NOT NULL
			BEGIN
				UPDATE		Taxon_Dictionary_Term_Version_Mapping
				SET			Source_Join_Key							=	@source_join_key
				WHERE		Taxon_Version_Key						=	@taxon_version_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* create taxon version */
			EXECUTE		spNextKey		'TAXON_VERSION',
										@taxon_version_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_VERSION (
						TAXON_VERSION_KEY,
						TAXON_KEY,
						ATTRIBUTE,
						UK_NATIVE,
						SOURCE_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_version_key,
						@taxon_key,
						@attribute,
						0,
						@source_key,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* If no term version, then create one for synchronisation reasons */
			IF @Term_Version_Key IS NULL
			BEGIN
				EXECUTE		spNextKey		'TERM_VERSION',
										@term_version_key	OUTPUT

				INSERT INTO Term_Version (
						Term_Version_Key,
						Term_Key,
						Entered_Session_ID,
						System_Supplied_Data)
				VALUES (
						@Term_Version_Key,
						@Term_Key,
						@SessionId,
						@system)
				
				UPDATE Concept 
				SET Term_Version_Key=@Term_Version_Key
				WHERE Concept_Key=@Concept_Key

			END

			/* record mapping */
			INSERT		Taxon_Dictionary_Term_Version_Mapping (
						Taxon_Version_Key,
						Term_Version_Key,
						Source_Join_Key)
			VALUES		(@taxon_version_key,
						@term_version_key,
						@source_join_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		IF @term_version_key <> @prior_term_version_key
		BEGIN
			/* Use of @prior_term_version_key is a hack for the case where
			 * a single Term corresponds to multiple Taxon records; we don't
			 * increment the progress count until all the taxa have been
			 * considered.
			 */
			SET			@prior_term_version_key		=	@term_version_key

		END

		COMMIT TRANSACTION
	END

	CLOSE		versions
	DEALLOCATE	versions
	RETURN

fail_from_cursor:
	CLOSE		versions
	DEALLOCATE	versions

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonVersion_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
GRANT EXECUTE ON [dbo].[usp_TaxonVersion_ImportConceptGroup] TO [Public]
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
		WHERE		Item_Name LIKE @SearchText + '%'
		AND 		Language_Key = @SearchKey
		ORDER BY 	Plaintext
	ELSE
		SELECT 
				T.Term_Key AS Item_Key,
				Item_Name AS DisplayTerm,
				Item_Name AS SearchTerm,
				Language_Key
		FROM		Term AS T
		WHERE		Item_Name LIKE @SearchText + '%'
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
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusFact_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ThesaurusFact_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import thesaurus facts corresponding to the facts associated
  				with a taxon list.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 3 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFact_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE		@taxon_list_key			CHAR(16),
				@taxon_fact_key			CHAR(16),
				@type					VARCHAR(1),
				@meaning_key			CHAR(16),
				@ins_user_key			CHAR(16),
				@ins_date				SMALLDATETIME,
				@ins_session_id			CHAR(16),
				@upd_user_key			CHAR(16),
				@upd_date				SMALLDATETIME,
				@upd_session_id			CHAR(16),
				@system					BIT,
				@source_key				CHAR(16),
				@thesaurus_fact_key		CHAR(16),
				@fact_type_concept_key	CHAR(16),
				@source_join_key		CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing facts'
	IF @@ERROR <> 0 RETURN

	DECLARE		@versions	TABLE (
				Taxon_Version_Key	CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS)

	INSERT		@versions
	SELECT		tli.TAXON_VERSION_KEY
	FROM		TAXON_LIST_VERSION						AS	tlv
	INNER JOIN	TAXON_LIST_ITEM							AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY				=	tlv.TAXON_LIST_VERSION_KEY
	WHERE		tlv.TAXON_LIST_KEY						=	@taxon_list_key

	DECLARE		facts	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tf.TAXON_FACT_KEY,
				tf.TYPE,
				CASE tf.TYPE
					WHEN 'T' THEN 'SYSTEM00000002NO' /* HTML */
					WHEN 'S' THEN 'SYSTEM00000002NO' /* HTML */
					WHEN 'A' THEN 'SYSTEM00000002L9' /* AVI */
					WHEN 'W' THEN 'SYSTEM00000002L8' /* WAV */
					WHEN 'B' THEN 'SYSTEM00000000W0' /* Bitmap */
					WHEN 'J' THEN 'SYSTEM00000000VY' /* JPEG */
				END,
				c.Meaning_Key,
				tf.ENTERED_BY,
				tf.ENTRY_DATE,
				tf.CHANGED_BY,
				tf.CHANGED_DATE,
				tf.SYSTEM_SUPPLIED_DATA,
				tf.SOURCE_KEY
	FROM		@versions								AS	tli
	INNER JOIN	TAXON_FACT								AS	tf
	ON			tf.TAXON_VERSION_KEY					=	tli.TAXON_VERSION_KEY
	INNER JOIN	Taxon_Dictionary_Term_Version_Mapping	AS	m
	ON			m.Taxon_Version_Key						=	tf.TAXON_VERSION_KEY
	INNER JOIN	Concept									AS	c
	ON			c.Term_Version_Key						=	m.Term_Version_Key

	OPEN		facts

	WHILE 1 = 1
	BEGIN
		FETCH		facts
		INTO        @taxon_fact_key,
					@type,
					@fact_type_concept_key,
					@meaning_key,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system,
					@source_key

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		/* obtain session identifiers */
		EXECUTE		usp_Session_ForDate		@ins_user_key,
											@ins_date,
											@ins_session_id		OUTPUT
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @upd_user_key IS NULL
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

		SELECT		@thesaurus_fact_key						=	Thesaurus_Fact_Key,
					@source_join_key						=	Source_Join_Key
		FROM		Taxon_Dictionary_Thesaurus_Fact_Mapping
		WHERE		Taxon_Fact_Key							=	@taxon_fact_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update existing thesaurus fact */
			UPDATE		Thesaurus_Fact
			SET			Item_Name				=	tf.TITLE,
						Data
							=	CASE
									WHEN @type = 'T' OR @type = 'S' THEN tf.DATA
									WHEN CHARINDEX('href="', tf.DATA) > 0 THEN
										SUBSTRING(
											tf.DATA,
											CHARINDEX('href="', tf.DATA) + 6,
											CHARINDEX('"', tf.DATA, CHARINDEX('href="', tf.DATA) + 6)
											- (CHARINDEX('href="', tf.DATA) + 6))
									WHEN CHARINDEX('href=''', tf.DATA) > 0 THEN
										SUBSTRING(
											tf.DATA,
											CHARINDEX('href=''', tf.DATA) + 6,
											CHARINDEX('''', tf.DATA, CHARINDEX('href=''', tf.DATA) + 6)
										- (CHARINDEX('href=''', tf.DATA) + 6))
									ELSE SUBSTRING(
											tf.DATA,
											CHARINDEX('href=', tf.DATA) + 5,
											PATINDEX(
												'%[ >]%',
												SUBSTRING(
													tf.DATA,
													CHARINDEX('href=', tf.DATA) + 5,
													DATALENGTH(tf.DATA))) - 1)
								END,
						Meaning_Key				=	@meaning_key,
						Concept_Key				=	NULL,
						Term_Version_Key    	=	NULL,
						Inherited				=	0,
						Fact_Vague_Date_Start	=	tf.FACT_VAGUE_DATE_START,
						Fact_Vague_Date_End		=	tf.FACT_VAGUE_DATE_END,
						Fact_Vague_Date_Type	=	ISNULL(
														tf.FACT_VAGUE_DATE_TYPE,
														'U'),
						Fact_Type_Concept_Key	=	@fact_type_concept_key,
						Entered_Session_ID		=	@ins_session_id,
						Changed_Session_ID		=	@upd_session_id,
						System_Supplied_Data	=	@system
			FROM		TAXON_FACT			   	AS	tf,
						Thesaurus_Fact
			WHERE		tf.TAXON_FACT_KEY		=	@taxon_fact_key
			AND			Thesaurus_Fact_Key		=	@thesaurus_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create thesaurus fact */
			EXECUTE		spNextKey	'Thesaurus_Fact',
									@thesaurus_fact_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			SET			@source_join_key			=	NULL

			INSERT		Thesaurus_Fact (
						Thesaurus_Fact_Key,
						Item_Name,
						Data,
						Meaning_Key,
						Language_Key,
						Fact_Vague_Date_Start,
						Fact_Vague_Date_End,
						Fact_Vague_Date_Type,
						Fact_Type_Concept_Key,
						Related_Term_Versions,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			SELECT		@thesaurus_fact_key,
						tf.TITLE,
						CASE
							WHEN @type = 'T' OR @type = 'S' THEN tf.DATA
							WHEN CHARINDEX('href="', tf.DATA) > 0 THEN
								SUBSTRING(
									tf.DATA,
									CHARINDEX('href="', tf.DATA) + 6,
									CHARINDEX('"', tf.DATA, CHARINDEX('href="', tf.DATA) + 6)
									- (CHARINDEX('href="', tf.DATA) + 6))
							WHEN CHARINDEX('href=''', tf.DATA) > 0 THEN
								SUBSTRING(
									tf.DATA,
									CHARINDEX('href=''', tf.DATA) + 6,
									CHARINDEX('''', tf.DATA, CHARINDEX('href=''', tf.DATA) + 6)
								- (CHARINDEX('href=''', tf.DATA) + 6))
							ELSE SUBSTRING(
									tf.DATA,
									CHARINDEX('href=', tf.DATA) + 5,
									PATINDEX(
										'%[ >]%',
										SUBSTRING(
											tf.DATA,
											CHARINDEX('href=', tf.DATA) + 5,
											DATALENGTH(tf.DATA))) - 1)
						END,
						@meaning_key,
						'en',
						tf.FACT_VAGUE_DATE_START,
						tf.FACT_VAGUE_DATE_END,
						ISNULL(tf.FACT_VAGUE_DATE_TYPE, 'U'),
						@fact_type_concept_key,
						0,
						@ins_session_id,
						@upd_session_id,
						@system
			FROM		TAXON_FACT					AS	tf
			WHERE		tf.TAXON_FACT_KEY			=	@taxon_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Thesaurus_Fact_Mapping (
						Taxon_Fact_Key,
						Thesaurus_Fact_Key)
			VALUES		(@taxon_fact_key,
						@thesaurus_fact_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* make any changes required in Source_Join */
		IF @source_key IS NULL
		BEGIN
			UPDATE		Taxon_Dictionary_Thesaurus_Fact_Mapping
			SET			Source_Join_Key							=	NULL
			WHERE		Taxon_Fact_Key							=	@taxon_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_SourceJoin_RecordImported	@source_join_key	OUTPUT,
													'Thesaurus_Fact',
													@thesaurus_fact_key,
													@source_key,
													@ins_session_id,
													@system
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @source_key IS NOT NULL
		BEGIN
			UPDATE		Taxon_Dictionary_Thesaurus_Fact_Mapping
			SET			Source_Join_Key							=	@source_join_key
			WHERE		Taxon_Fact_Key							=	@taxon_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* update progress counter */
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor
		
		COMMIT TRANSACTION
	END

	CLOSE		facts
	DEALLOCATE	facts
	RETURN

fail_from_cursor:
	CLOSE		facts
	DEALLOCATE	facts

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ThesaurusFact_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
GRANT EXECUTE ON [dbo].[usp_ThesaurusFact_ImportTaxonList] TO [Public]
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusRelationForwardReverse_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusRelationForwardReverse_Select]
GO

/*===========================================================================*\
  Description:	Returns the forward and reverse terms for each record
		in the Thesaurus_Relation_Type table.

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusRelationForwardReverse_Select]
AS

SET NOCOUNT ON
	SELECT 	 Thesaurus_Relation_Type_Key, Forward_Term AS Item_Name, 1 AS IsForward
	FROM 	 Thesaurus_Relation_Type
	UNION
	SELECT 	 Thesaurus_Relation_Type_Key, Reverse_Term AS Item_Name, 0 AS IsForward
	FROM 	 Thesaurus_Relation_Type

	ORDER BY Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusRelationForwardReverse_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusRelationForwardReverse_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ThesaurusRelationForwardReverse_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationForwardReverse_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationForwardReverse_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationForwardReverse_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationForwardReverse_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusRelationForwardReverse_Select TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Valuations_Select_ForSearchByValue]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Valuations_Select_ForSearchByValue]
GO

CREATE PROCEDURE [dbo].[usp_Valuations_Select_ForSearchByValue] 
@UserID CHAR(16),
@SearchText VARCHAR(30),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Valuations based on the search parameter for the amount
--	@SortOrderIndex		Index determining Sort Order
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@UserID				Name_Key of current user
--	@SearchText			Text to be used for search
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-17
--
SET NOCOUNT ON
DECLARE @GT BIT, @LT BIT, @GTET BIT, @LTET BIT, @ET BIT, @OperatorFound BIT
DECLARE @Index INT
DECLARE @Amount MONEY

SET @GT = 0
SET @LT = 0
SET @GTET = 0
SET @LTET = 0
SET @ET = 0
SET @OperatorFound = 0

SET @Index = CHARINDEX('>=', @SearchText)
IF @Index > 0 
BEGIN
	SET @GTET = 1
	SET @OperatorFound = 1
	--Remove operator from searchtext
	SET @SearchText = REPLACE(@SearchText, '>=', '')
	GOTO OperatorFound
END

SET @Index = CHARINDEX('<=', @SearchText)
IF @Index > 0 
BEGIN
	SET @LTET = 1
	SET @OperatorFound = 1
	SET @SearchText = REPLACE(@SearchText, '<=', '')
	GOTO OperatorFound
END

SET @Index = CHARINDEX('>', @SearchText)
IF @Index > 0 
BEGIN
	SET @GT = 1
	SET @OperatorFound = 1
	SET @SearchText = REPLACE(@SearchText, '>', '')
	GOTO OperatorFound
END

SET @Index = CHARINDEX('<', @SearchText)
IF @Index > 0 
BEGIN
	SET @LT = 1
	SET @OperatorFound = 1
	SET @SearchText = REPLACE(@SearchText, '<', '')
	GOTO OperatorFound
END

SET @Index = CHARINDEX('=', @SearchText)
IF @Index > 0 
BEGIN
	SET @ET = 1
	SET @OperatorFound = 1
	SET @SearchText = REPLACE(@SearchText, '=', '')
	GOTO OperatorFound
END
OperatorFound:
IF ISNUMERIC(RTRIM(LTRIM(@SearchText)))=1
	SET @Amount = CONVERT(MONEY, RTRIM(LTRIM(@SearchText)))
ELSE
	SET @Amount=NULL



IF @SortOrderIndex = 0
BEGIN
	IF (@OperatorFound = 0) OR ((@OperatorFound = 1) AND (@ET = 1))
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
			CONVERT(VARCHAR(10), V.Value_Amount, 0) + ' ' + CT.Item_Name AS Hint
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Currency_Concept_Key
		WHERE U.ALLOW_FINANCE = 1
			AND V.Value_Amount = @Amount
		ORDER BY V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type, CT.Item_Name
	ELSE IF @GTET = 1
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
			CONVERT(VARCHAR(10), V.Value_Amount, 0) + ' ' + CT.Item_Name AS Hint
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Currency_Concept_Key
		WHERE U.ALLOW_FINANCE = 1
			AND V.Value_Amount >= @Amount
		ORDER BY V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type, CT.Item_Name
	ELSE IF @LTET = 1
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
			CONVERT(VARCHAR(10), V.Value_Amount, 0) + ' ' + CT.Item_Name AS Hint
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Currency_Concept_Key
		WHERE U.ALLOW_FINANCE = 1
			AND V.Value_Amount <= @Amount
		ORDER BY V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type, CT.Item_Name
	ELSE IF @GT = 1
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
			CONVERT(VARCHAR(10), V.Value_Amount, 0) + ' ' + CT.Item_Name AS Hint
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Currency_Concept_Key
		WHERE U.ALLOW_FINANCE = 1
			AND V.Value_Amount > @Amount
		ORDER BY V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type, CT.Item_Name
	ELSE IF @LT = 1
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
			CONVERT(VARCHAR(10), V.Value_Amount, 0) + ' ' + CT.Item_Name AS Hint
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Currency_Concept_Key
		WHERE U.ALLOW_FINANCE = 1
			AND V.Value_Amount < @Amount
		ORDER BY V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type, CT.Item_Name
END
ELSE IF @SortOrderIndex = 1
BEGIN
	IF (@OperatorFound = 0) OR ((@OperatorFound = 1) AND (@ET = 1))
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
			CONVERT(VARCHAR(10), V.Value_Amount, 0) + ' ' + CT.Item_Name AS Hint
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Currency_Concept_Key
		WHERE U.ALLOW_FINANCE = 1
			AND V.Value_Amount = @Amount
		ORDER BY CT.Item_Name, V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type
	ELSE IF @GTET = 1
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
			CONVERT(VARCHAR(10), V.Value_Amount, 0) + ' ' + CT.Item_Name AS Hint
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Currency_Concept_Key
		WHERE U.ALLOW_FINANCE = 1
			AND V.Value_Amount >= @Amount
		ORDER BY CT.Item_Name, V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type
	ELSE IF @LTET = 1
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
			CONVERT(VARCHAR(10), V.Value_Amount, 0) + ' ' + CT.Item_Name AS Hint
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Currency_Concept_Key
		WHERE U.ALLOW_FINANCE = 1
			AND V.Value_Amount <= @Amount
		ORDER BY CT.Item_Name, V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type
	ELSE IF @GT = 1
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
			CONVERT(VARCHAR(10), V.Value_Amount, 0) + ' ' + CT.Item_Name AS Hint
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Currency_Concept_Key
		WHERE U.ALLOW_FINANCE = 1
			AND V.Value_Amount > @Amount
		ORDER BY CT.Item_Name, V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type
	ELSE IF @LT = 1
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
			CONVERT(VARCHAR(10), V.Value_Amount, 0) + ' ' + CT.Item_Name AS Hint
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Currency_Concept_Key
		WHERE U.ALLOW_FINANCE = 1
			AND V.Value_Amount < @Amount
		ORDER BY CT.Item_Name, V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type
END


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Valuations_Select_ForSearchByValue') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Valuations_Select_ForSearchByValue'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByValue TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByValue TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByValue TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByValue TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByValue TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByValue TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Valuations_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Valuations_Select_ForTopLevel]
GO

CREATE PROCEDURE [dbo].[usp_Valuations_Select_ForTopLevel] 
@UserID CHAR(16),
@SortOrderIndex TINYINT,
@Key CHAR(16) = NULL

AS

--  DESCRIPTION
--  Returns top level Valuations data to the CollectionsBrowser
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@UserID				Name_Key of current user
--	@Key 				Optional Key. When specified, only the single top level record is returned with that key
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-08-25
--
SET NOCOUNT ON

-- Create  a table to hold the items we are looking for
DECLARE @Search TABLE (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)

IF @Key IS NOT NULL
	INSERT INTO @Search VALUES (@Key)
ELSE IF object_id('tempdb..#TempFilter') is not null
	INSERT INTO @Search SELECT DISTINCT ItemKey FROM #TempFilter
ELSE
	INSERT INTO @Search SELECT Valuation_Key FROM Valuation

IF @SortOrderIndex = 0
BEGIN
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Type_Concept_Key	
		INNER JOIN @Search S ON S.ItemKey = V.Valuation_Key
		WHERE U.ALLOW_FINANCE = 1
		ORDER BY V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type, CT.Item_Name
END
ELSE IF @SortOrderIndex = 1
BEGIN
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Type_Concept_Key	
		INNER JOIN @Search S ON S.ItemKey = V.Valuation_Key
		WHERE U.ALLOW_FINANCE = 1
		ORDER BY CT.Item_Name, V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Valuations_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Valuations_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Valuations_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Valuations_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Valuation_Duplicate]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Valuation_Duplicate]
GO

/*===========================================================================*\
  Description:	Duplicates a Valuation record except for the Value itself.

  Parameters:	@OriginalKey

  Created:	January 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Valuation_Duplicate]
	@NewKey char(16) OUTPUT,
	@Key char(16),
	@SessionID char(16),
	@RecordsAffected int OUTPUT
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	
	DECLARE @Error int

	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Valuation', @NewKey OUTPUT

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Valuation.
		\*-------------------------------------------------------------*/
		INSERT INTO Valuation (
			Valuation_Key, 
			Vague_Date_Start, 
			Vague_Date_End, 
			Vague_Date_Type, 
			Ref_Number, 
			Type_Concept_Key, 
			Valued_By_Name_Key, 
			Value_Amount, 
			Currency_Concept_Key,
			Valid_From_Vague_Date_Start, 
			Valid_From_Vague_Date_End,
			Valid_From_Vague_Date_Type, 
			Valid_To_Vague_Date_Start,
			Valid_To_Vague_Date_End, 
			Valid_To_Vague_Date_Type,
			[Description], 
			Display_Caption,
			Search_Caption,
			Entered_Session_ID
		) 
		SELECT 	
		 	@NewKey, 
			Vague_Date_Start, 
			Vague_Date_End, 
			Vague_Date_Type, 
			Ref_Number, 
			Type_Concept_Key, 
			Valued_By_Name_Key, 
			0, 
			Currency_Concept_Key,
			Valid_From_Vague_Date_Start, 
			Valid_From_Vague_Date_End,
			Valid_From_Vague_Date_Type, 
			Valid_To_Vague_Date_Start,
			Valid_To_Vague_Date_End, 
			Valid_To_Vague_Date_Type,
			[Description], 
			Display_Caption,
			Search_Caption,
			@SessionID
		FROM	Valuation
		WHERE	Valuation_Key = @Key

		SELECT	@RecordsAffected = @@RowCount,
			@Error = @@Error

		IF @Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Duplicate any links to sources.
		\*-------------------------------------------------------------*/
		DECLARE	@OriginalSourceJoinKey char(16),
			@NewSourceJoinKey char(16)

		DECLARE curSources CURSOR LOCAL FAST_FORWARD FOR
			SELECT	Source_Join_Key
			FROM	Source_Join
			WHERE	Record_Key = @Key
			AND	Table_Name = 'Valuation'

		OPEN curSources

		FETCH NEXT
		FROM 	curSources
		INTO	@OriginalSourceJoinKey

		WHILE @@Fetch_Status = 0
		BEGIN
			EXECUTE spNextKey 'Source_Join', @NewSourceJoinKey OUTPUT

			INSERT INTO Source_Join (
				Source_Join_Key,
				Table_Name,
				Record_Key,
				Source_Key,
				Original,
				Entered_Session_ID,
				System_Supplied_Data
			) SELECT
				@NewSourceJoinKey,
				Table_Name,
				@NewKey,
				Source_Key,
				Original,
				@SessionID,
				0
			FROM 	Source_Join
			WHERE	Source_Join_Key = @OriginalSourceJoinKey
		
			IF @Error <> 0 GOTO RollbackAndExit
		
			FETCH NEXT
			FROM 	curSources
			INTO	@OriginalSourceJoinKey
		END

		CLOSE curSources
		DEALLOCATE curSources

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Valuation_Duplicate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Valuation_Duplicate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Valuation_Duplicate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Valuation_Duplicate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Valuation_Duplicate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Valuation_Duplicate TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Valuation_Duplicate TO [Dev - JNCC SQL]
END

GO
			

