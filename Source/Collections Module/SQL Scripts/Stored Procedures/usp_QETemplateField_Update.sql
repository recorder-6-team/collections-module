If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QETemplateField_Update]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QETemplateField_Update]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 9 $
    $Date: 3/02/09 10:22 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QETemplateField_Update]
 	@Key as char(16),
	@Timestamp as timestamp,
	@GeneralTab as bit,
	@SpecimenTab as bit,
	@ItemName as varchar(100),
	@DefaultValue as varchar(200),
	@DefaultDisplay as varchar(200),
	@SessionID as varchar(16),
	@Sequence int,
	@NumberTypeConceptKey as varchar(16),
	@NumberPreferred as bit,
	@Hidden as bit,
	@Locked as bit,
	@MetadataTypeKey as varchar(16)
AS

Set Nocount off

update QE_Template_Field
	set 
	General_Tab = @GeneralTab,
	Specimen_Tab = @SpecimenTab,
	Item_Name = @ItemName,
	Default_Value = @DefaultValue,
	Default_Display = @DefaultDisplay,
	Changed_Session_ID= @SessionID,
	Sequence = @Sequence,
	Number_Type_Concept_Key = @NumberTypeConceptKey,
	Number_Preferred = @NumberPreferred,
	Hidden = @Hidden,
	Locked = @Locked,
	Metadata_Type_Key = @MetadataTypeKey
	where QE_Template_Field_Key= @Key 
	AND		[Timestamp] = @Timestamp

IF @@Rowcount = 0 AND EXISTS(SELECT 1 FROM QE_Template_Field WHERE QE_Template_Field_Key = @Key)
	RAISERROR('Record updated by another user', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplateField_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Update TO [Dev - JNCC SQL]
END

GO