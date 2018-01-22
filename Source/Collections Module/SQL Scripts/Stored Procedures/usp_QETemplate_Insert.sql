If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QETemplate_Insert]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QETemplate_Insert]
GO
    
/*===========================================================================*\
  Description:	Inserts a new record into QETemplate

  Parameters:	@Item_Name        Name of the template
		@Template_Type    
		@Subject_Area_Key 

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 20/02/04 15:21 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QETemplate_Insert]
  @ItemName      varchar(100),
  @TemplateType  tinyint,
  @SessionID char(16),
  @SubjectAreaKey char(16),
  @Key char(16)=null output
 AS


SET NOCOUNT OFF

exec spNextKey 'QE_Template', @Key output

insert into QE_Template 
	(QE_Template_key, Item_Name, Template_Type, Subject_Area_Key, System_Supplied_Data, Entered_Session_ID)
	Values(@Key,@ItemName,@TemplateType,@SubjectAreaKey, 0, @SessionID)

GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplate_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplate_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplate_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplate_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplate_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplate_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplate_Insert TO [Dev - JNCC SQL]
END

GO

 