/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_EnquiryResponse_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_EnquiryResponse_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Enquiry table

  Parameters:	@Key
		@Response
		@SessionID
		@Timestamp

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 8/12/03 10:48 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_EnquiryResponse_Update]
	@Key char(16) OUTPUT,
	@Response text,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Update in Enquiry.
		\*-------------------------------------------------------------*/
		UPDATE 	Enquiry
		SET	Response = @Response, 
			Changed_Session_ID = @SessionID
		WHERE	Enquiry_Key = @Key
		--AND	TSEqual (@Timestamp, Timestamp)

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_EnquiryResponse_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_EnquiryResponse_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_EnquiryResponse_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_EnquiryResponse_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_EnquiryResponse_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_EnquiryResponse_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_EnquiryResponse_Update TO [Dev - JNCC SQL]
END
GO