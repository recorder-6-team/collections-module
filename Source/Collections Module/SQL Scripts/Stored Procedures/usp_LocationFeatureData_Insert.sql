/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFeatureData_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_LocationFeatureData_Insert]
GO

/*===========================================================================*\
  Description: Inserts a record into Location_Feature_Data.

  Parameters:  Fields of Location_Feature_Data

  Created:     August 2004

  Last revision information:
    $Revision: 1 $
    $Date: 31/08/04 17:42 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_LocationFeatureData_Insert]
	-- Required by both Measurements and Descriptors updates.
	@Key char(16) OUTPUT,
	@LocationFeatureKey char(16) = NULL,
       	@AppliesTo varchar(50),
       	@ParameterConceptKey char(16),
	@Value varchar(50),	-- Used for Descriptors and as Lower_Value for Measurements
	@IsDescriptor bit,
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

	BEGIN TRANSACTION

		EXECUTE spNextKey 'Location_Feature_Data', @Key OUTPUT

		/*----------------------------------------------------------------------------------*\
		  If we are inserting measurement data, more information needs to inserted than if 
		  we are inserting descriptor data. Hence, there are two different insert statements.
		\*----------------------------------------------------------------------------------*/
		IF @IsDescriptor = 1
			INSERT INTO Location_Feature_Data (
				Location_Feature_Data_Key, Location_Feature_Key, Applies_To, Parameter_Concept_Key, 
				Lower_Value, Is_Descriptor, Entered_Session_ID
			) VALUES (
				@Key, @LocationFeatureKey, @AppliesTo, @ParameterConceptKey, IsNull(@Value, ' '), 
				1, @SessionID
			)
		ELSE
			INSERT INTO Location_Feature_Data (
				Location_Feature_Data_Key, Location_Feature_Key, Applies_To, Method_Concept_Key, Duration,
				Accuracy, Parameter_Concept_Key, Unit_Concept_Key, Lower_Value, Upper_Value,
				Is_Descriptor, Entered_Session_ID
			) VALUES (
				@Key, @LocationFeatureKey, @AppliesTo, @MethodConceptKey, @Duration,
				@Accuracy, @ParameterConceptKey, @UnitConceptKey, IsNull(@Value, ' '), @UpperValue,
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFeatureData_Insert') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_LocationFeatureData_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Insert TO [Dev - JNCC SQL]
END
GO