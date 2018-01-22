If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Terms_Select_ForEnquiry]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Terms_Select_ForEnquiry]
GO

/*===========================================================================*\
  Description:	Returns Terms data to the CollectionsBrowser for a given Enquiry.

  Parameters:	
	@ParentKey 	Only the records associated with the parent key are returned

  Created:	August 2003

  Last revision information:
    $Revision: 4 $
    $Date: 4/08/11 10:42 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Terms_Select_ForEnquiry] 
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		C.Concept_Key AS Item_Key, C.Published_Term AS Item_Name, EC.Enquiry_Concept_Key AS Join_Key

	FROM 		Enquiry_Concept EC
	INNER JOIN 	Concept C ON EC.Concept_Key = C.Concept_Key AND EC.Enquiry_Key = @ParentKey
	ORDER BY 	C.Published_Term
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Terms_Select_ForEnquiry') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Terms_Select_ForEnquiry'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Terms_Select_ForEnquiry TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForEnquiry TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForEnquiry TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForEnquiry TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForEnquiry TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Terms_Select_ForEnquiry TO [Dev - JNCC SQL]
END
GO