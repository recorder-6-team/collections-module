If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_EnquiryConceptExists_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_EnquiryConceptExists_Get]
GO

CREATE PROCEDURE [dbo].[usp_EnquiryConceptExists_Get] 
@EnquiryKey CHAR(16),
@ConceptKey CHAR(16),
@Exists BIT OUTPUT

AS

--  DESCRIPTION
--  Determines if a concept is already linked to an enquiry
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@EnquiryKey			Enquiry Key
--	@ConceptKey			Concept Key
--	@Exists				Output Result
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			27/02/2004
--
SET NOCOUNT ON

IF EXISTS(SELECT * FROM Enquiry_Concept WHERE (Enquiry_Key = @EnquiryKey) AND (Concept_Key = @ConceptKey))
	SET @Exists = 1
ELSE
	SET @Exists = 0

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_EnquiryConceptExists_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_EnquiryConceptExists_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_EnquiryConceptExists_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_EnquiryConceptExists_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_EnquiryConceptExists_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_EnquiryConceptExists_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_EnquiryConceptExists_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_EnquiryConceptExists_Get TO [Dev - JNCC SQL]
END

GO
