IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_EnquiryStats_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_EnquiryStats_Select]
GO

/*===========================================================================*\
  Description:	Returns the number of enquiries against the departments they were made by.

  Parameters:	
	@VagueDateStart 	Start date to filter records by
	@VagueDateEnd		End date to filter records by

  Created:	January 2003

  Last revision information:
    $Revision: 3 $
    $Date: 9/11/07 16:35 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_EnquiryStats_Select] 
	@VagueDateStart INT,
	@VagueDateEnd INT
AS
SET NOCOUNT ON

	IF @VagueDateStart = -1 SET @VagueDateStart = NULL
	IF @VagueDateEnd = -1 SET @VagueDateEnd = NULL

	/*---------------------------------------------*\
	  Get all enquiry types to build a total column
	\*---------------------------------------------*/
	DECLARE	@Sums varchar(2000),
		@Names varchar(1000)

	-- Default is to have at least a Total for all types.
	SET	@Sums = 'Cast(Count(E.Enquiry_Key) AS varchar(100))'
	SET	@Names = 'Total'

	DECLARE	@ETypeKey char(16), 
		@EType varchar(100)

	DECLARE curEnquiryType CURSOR FOR
		SELECT Concept_Key, PlainText FROM vw_ConceptTerm WHERE Concept_Group_Key = 'SYSTEM000000000A'
	OPEN curEnquiryType
	FETCH NEXT FROM curEnquiryType INTO @ETypeKey, @EType
	WHILE (@@Fetch_Status = 0) BEGIN
		-- Build up the SQL for the calculated value
		SET @Sums = @Sums + '+'',''+' +
			'Cast(Sum(CASE WHEN Enquiry_Type_Concept_Key = ''' + @ETypeKey + ''' THEN 1 ELSE 0 END) AS varchar(100))'
		-- Build up the column name out of all enquiry types.
		SET @Names = @Names + ',' + @EType

		FETCH NEXT FROM curEnquiryType INTO @ETypeKey, @EType
	END
	CLOSE curEnquiryType
	DEALLOCATE curEnquiryType

	/*---------------------------------------------*\
	  And run the query. Has to be a dynamic query.
	\*---------------------------------------------*/
	EXECUTE(
	-- Use NULL to have types as first record in results
	'SELECT 	NULL AS Department, ''' + @names + ''' AS Types ' +
	'UNION ' +
	'SELECT 	OD.Item_Name, ' + @Sums + 
	'FROM		Organisation_Department OD ' +
	'LEFT JOIN 	(Individual I JOIN Enquiry E ON I.Name_Key = E.Answered_By_Name_Key) ' +
	'		ON OD.Organisation_Department_Key = I.Organisation_Department_Key ' +
	'LEFT JOIN	vw_ConceptTerm C ON C.Concept_Key = E.Enquiry_Type_Concept_Key ' +
	'GROUP BY OD.Item_Name ' +
	'ORDER BY "Department"'
	)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_EnquiryStats_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_EnquiryStats_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_EnquiryStats_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_EnquiryStats_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_EnquiryStats_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_EnquiryStats_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_EnquiryStats_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_EnquiryStats_Select TO [Dev - JNCC SQL]
END
GO