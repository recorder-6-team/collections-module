If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QETemplate_Update]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QETemplate_Update]
GO
    
/*===========================================================================*\
  Description:	Updates a QETemplate

  Parameters:	@QE_Template_Key  Key of template
		@Timestamp        Previously retrieved timestamp
		@Item_Name        Name of the template
		@Template_Type    
		@Subject_Area_Key 

  Created:	August 2003

  Last revision information:
    $Revision: 6 $
    $Date: 3/02/09 11:33 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QETemplate_Update]
  @Key  Char(16),
  @Timestamp      timestamp,
  @ItemName      varchar(100),
  @TemplateType  tinyint,
  @SessionID      Char(16),
  @SubjectAreaKey char(16)
 AS

SET NOCOUNT OFF

update QE_Template 
	set Item_Name = @ItemName,
	Template_Type = @TemplateType,
	Subject_Area_Key = @SubjectAreaKey,
	Changed_Session_ID = @SessionID
	where QE_Template_Key = @Key
	AND ([Timestamp] = @Timestamp)

IF @@Rowcount = 0 AND EXISTS(SELECT 1 FROM QE_Template WHERE QE_Template_Key = @Key)
	RAISERROR('Record updated by another user', 16, 1)

GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplate_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplate_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplate_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplate_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplate_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplate_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplate_Update TO [Dev - JNCC SQL]
END

GO


 