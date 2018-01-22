/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonOccurrence_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonOccurrence_Insert]
GO

/*===========================================================================*\
  Description:	
  Parameters:	

  Created:	July 2003

  Last revision information:
    $Revision: 1 $
    $Date: 13/12/07 15:03 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonOccurrence_Insert]
	@Key 			CHAR(16) OUTPUT,
	@SampleKey 		CHAR(16),
	@Confidential 	BIT 		= NULL,
	@Comment 		TEXT 		= NULL,
	@EnteredBy 		CHAR(16),
	@RecordTypeKey 	CHAR(16) 	= NULL,
	@Checked 		BIT			= 0,
	@CheckedBy		CHAR(16)	= NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	
	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Taxon_Occurrence', @Key OUTPUT
	
	INSERT INTO Taxon_Occurrence
		(Taxon_Occurrence_Key, Comment, Confidential, Sample_Key,  
		Record_Type_Key, Entered_By, Checked, Checked_By)
	VALUES
		(@Key, @Comment, IsNull(@Confidential, 0), @SampleKey, 
		@RecordTypeKey, @EnteredBy, @Checked, @CheckedBy)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrence_Insert') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_TaxonOccurrence_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_TaxonOccurrence_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_TaxonOccurrence_Insert TO [Dev - JNCC SQL]
END

GO