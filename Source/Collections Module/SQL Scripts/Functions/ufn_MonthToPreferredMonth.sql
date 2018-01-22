/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_MonthToPreferredMonth]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_MonthToPreferredMonth
GO

/*===========================================================================*\
  Description:	Converts a Month into a Season

  Parameters:	@Month		- Month number

  Created:	18 September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 31/07/08 11:11 $
    $Author: Johndurman $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_MonthToPreferredMonth(
  @Month INT
)
RETURNS varchar(150)

AS
BEGIN
	DECLARE @Output VARCHAR(150)
	DECLARE @Concept_Key CHAR(16)

	SET @Concept_Key = 
		CASE @Month
			WHEN 1 THEN 'SYSTEM000000008I'
			WHEN 2 THEN 'SYSTEM000000008J'
			WHEN 3 THEN 'SYSTEM000000008K'
			WHEN 4 THEN 'SYSTEM000000008L'
			WHEN 5 THEN 'SYSTEM000000008M'
			WHEN 6 THEN 'SYSTEM000000008N'
			WHEN 7 THEN 'SYSTEM000000008O'
			WHEN 8 THEN 'SYSTEM000000008P'
			WHEN 9 THEN 'SYSTEM000000008Q'
			WHEN 10 THEN 'SYSTEM000000008R'
			WHEN 11 THEN 'SYSTEM000000008S'
			WHEN 12 THEN 'SYSTEM000000008T'
		END


	SELECT @Output = PlainText 
	FROM dbo.VW_ConceptTermPreferred
	WHERE Concept_Key = @Concept_Key

  RETURN @Output
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_MonthToPreferredMonth]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_MonthToPreferredMonth'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_MonthToPreferredMonth TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_MonthToPreferredMonth TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_MonthToPreferredMonth TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_MonthToPreferredMonth TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_MonthToPreferredMonth TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_MonthToPreferredMonth TO [Dev - JNCC SQL]
	END
GO
