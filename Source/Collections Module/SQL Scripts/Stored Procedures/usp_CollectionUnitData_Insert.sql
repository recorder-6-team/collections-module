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
		@Value
		@UpperValue
		@IsDescriptor
		@SessionID

  Created:	September 2003

  Last revision information:
    $Revision: 10 $
    $Date: 9/08/04 10:46 $
    $Author: Anthonysimpson $

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
	@UpperValue varchar(50) = NULL,
	@MethodConceptKey char(16) = NULL,
	@Duration varchar(50) = NULL,
	@Accuracy varchar(50) = NULL,
	@UnitConceptKey char(16) = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

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
				IsNull(@Value, ' '), @IsDescriptor, @SessionID
			)
		ELSE
			INSERT INTO Collection_Unit_Data (
				Collection_Unit_Data_Key, Collection_Unit_Key, Applies_To,
				Method_Concept_Key, Duration, Accuracy, Parameter_Concept_Key,
				Unit_Concept_Key, Lower_Value, Upper_Value, Is_Descriptor,
				Entered_Session_ID
			) VALUES (
				@Key, @CollectionUnitKey, @AppliesTo, @MethodConceptKey, @Duration,
				@Accuracy, @ParameterConceptKey, @UnitConceptKey, IsNull(@Value, ' '),
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