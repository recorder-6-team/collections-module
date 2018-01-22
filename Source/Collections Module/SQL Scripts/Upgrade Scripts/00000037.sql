/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetTranslation_String_From_Value]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetTranslation_String_From_Value
GO

/*===========================================================================*\
  Description:	Converts the input index into an internationalised string (used application-wide).

  Parameters:	@Index		-Input index value
				@Context	-Context which the value is to be interpreted

  Created:	12 Jan 2004

  Last revision information:
    $Revision: 1 $
    $Date: 7/08/08 15:32 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetTranslation_String_From_Value]
(
@Index INT,
@Context VARCHAR(30)
)
RETURNS varchar(50)

AS
BEGIN
DECLARE @ReturnValue VARCHAR(50)
SET @Context = UPPER(@Context)

SET @ReturnValue = 
CASE @Context
WHEN 'GENERAL' THEN --General
	CASE @Index
		WHEN 0 THEN 'Unknown'
	END
WHEN 'YESNO' THEN --General
	CASE @Index
		WHEN 0 THEN 'No'
		WHEN 1 THEN 'Yes'
	END
WHEN 'CONFIDENCE' THEN --Used for specimen determinations
	CASE @Index
		WHEN 0 THEN 'Uncertain Confidence'
		WHEN 1 THEN 'No Confidence'
		WHEN 2 THEN 'Low Confidence'
		WHEN 3 THEN 'Medium Confidence'
		WHEN 4 THEN 'High Confidence'
	END
WHEN 'LABELINSCRIPTION' THEN --Used for specimen labels and inscriptions
	CASE @Index
		WHEN 0 THEN 'Label'
		WHEN 1 THEN 'Inscription'
	END
WHEN 'STATUS' THEN
	CASE @Index
		WHEN 0 THEN 'Pending'
		WHEN 1 THEN 'Open'
		WHEN 2 THEN 'Closed'
		WHEN 3 THEN 'Postponed'
		WHEN 4 THEN 'Abandoned'
	END
WHEN 'PRIORITY' THEN
	CASE @Index
		WHEN 0 THEN 'Low'
		WHEN 1 THEN 'Medium'
		WHEN 2 THEN 'High'
		WHEN 3 THEN 'Urgent'
	END
WHEN 'IN' THEN
	'in'
WHEN 'MATERIALMOVEMENT' THEN
	CASE @Index
		WHEN 0 THEN 'Material has been '
	END
END

RETURN @ReturnValue

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetTranslation_String_From_Value]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetTranslation_String_From_Value'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetTranslation_String_From_Value TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetTranslation_String_From_Value TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetTranslation_String_From_Value TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetTranslation_String_From_Value TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetTranslation_String_From_Value TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetTranslation_String_From_Value TO [Dev - JNCC SQL]
	END
GO

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
    $Revision: 1 $
    $Date: 7/08/08 15:32 $
    $Author: Johnvanbreda $

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

