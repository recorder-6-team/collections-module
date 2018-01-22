/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[ufn_Base36Sum]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_Base36Sum
GO

/*===========================================================================*\
  Description:	Base 36 representation of the sum of two integers, one of
				which is given in its base 36 representation.

				'#ERROR!' if @operand_1 is not a valid representation of a
				non-negative integer in base 36, or if the sum would be
				negative.

  Parameters:   @operand_1				First operand (base 36 representation)
				@operand_2				Second operand

  Created:		Dec 2003

  Last revision information:
	$Revision: 1 $
	$Date: 27/08/04 9:54 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_Base36Sum(
	@operand_1		VARCHAR(16),
	@operand_2		INT)
RETURNS
	VARCHAR(16)
AS
BEGIN
	DECLARE		@sum				VARCHAR(16),
				@char_index			INT,
				@operand_1_char		CHAR,
				@temp				INT

	SELECT		@sum		=	'',
				@char_index	=	LEN(@operand_1),
				@operand_1	=	UPPER(@operand_1),
				@temp		=	@operand_2

	WHILE		@char_index > 0
	BEGIN
		SET			@operand_1_char	=	SUBSTRING(@operand_1, @char_index, 1)

		IF @operand_1_char >= '0' AND @operand_1_char <= '9'
		BEGIN
			SET			@temp	=	@temp + ASCII(@operand_1_char) - ASCII('0')
		END
		ELSE IF @operand_1_char >= 'A' AND @operand_1_char <= 'Z'
		BEGIN
			SET			@temp	=	@temp + ASCII(@operand_1_char) - ASCII('A') + 10
		END
		ELSE
		BEGIN
			RETURN		'#ERROR!'
		END

		SET			@sum	=	CASE WHEN ((36 + (@temp % 36)) % 36) < 10
									THEN CHAR(ASCII('0') + ((36 + (@temp % 36)) % 36))
									ELSE CHAR(ASCII('A') + ((36 + (@temp % 36)) % 36) - 10)
								END
								+ @sum

		SET			@temp		=	@temp / 36
									- CASE WHEN (@temp % 36) < 0 THEN 1 ELSE 0 END
		SET			@char_index	=	@char_index - 1
	END

	IF @temp < 0 OR (@temp > 0 AND LEN(@sum) = 16)
	BEGIN
		RETURN		'#ERROR!'
	END

	WHILE @temp > 0
	BEGIN
		SET			@sum	=	CASE WHEN (@temp % 36) < 10
									THEN CHAR(ASCII('0') + (@temp % 36))
									ELSE CHAR(ASCII('A') + (@temp % 36) - 10)
								END
								+ @sum

		SET			@temp	=	@temp / 36
	END

	SET			@sum	=	REPLACE(LTRIM(REPLACE(@sum, '0', ' ')), ' ', '0')
	RETURN		CASE WHEN @sum = '' THEN '0' ELSE @sum END
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_Base36Sum]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_Base36Sum'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_Base36Sum TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_Base36Sum TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_Base36Sum TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_Base36Sum TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_Base36Sum TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_Base36Sum TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_CBWrapperForDoVagueDatesOverlap]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_CBWrapperForDoVagueDatesOverlap
GO

/*===========================================================================*\
  Description:	Provides a wrapper to the function dbo.DoVagueDatesOverlap
				to replace zeros contained in @VagueDate_CB_Start, @VagueDate_CB_End
				with NULLs if the VagueDate_CB_Type is Y-, -Y, C- or -C

  Parameters:	@VagueDate_CB_Start 	Start of VagueDate from CollectionsBrowser
				@VagueDate_CB_End		End of VagueDate from CollectionsBrowser
				@VagueDate_CB_Type		Type of VagueDate from CollectionsBrowser
				@VagueDate_DB_Start		Start of VagueDate from Database
				@VagueDate_DB_End		End of VagueDate from Database

  Created:	30 October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 9:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_CBWrapperForDoVagueDatesOverlap]
(
@VagueDateString VARCHAR(30),
@VagueDate_DB_Start INT,
@VagueDate_DB_End INT
)

RETURNS BIT

AS
BEGIN


DECLARE @VagueDate_CB_Start INT
DECLARE @VagueDate_CB_End INT
DECLARE @VagueDate_CB_Type VARCHAR(2)
DECLARE @liDateSeperatorIndex INT
DECLARE @liDateTypeSeperatorIndex INT

SET @liDateSeperatorIndex = CHARINDEX(':', @VagueDateString)
SET @liDateTypeSeperatorIndex = CHARINDEX(';', @VagueDateString)
SET @VagueDate_CB_Start = CAST(SUBSTRING(@VagueDateString, 1, @liDateSeperatorIndex -1) AS INT)
SET @VagueDate_CB_End = CAST(SUBSTRING(@VagueDateString, @liDateSeperatorIndex +1, @liDateTypeSeperatorIndex - @liDateSeperatorIndex -1) AS INT)
SET @VagueDate_CB_Type = SUBSTRING(@VagueDateString, @liDateTypeSeperatorIndex +1, LEN(@VagueDateString) -@liDateTypeSeperatorIndex)

IF (UPPER(@VagueDate_CB_Type) = 'Y-') OR (UPPER(@VagueDate_CB_Type) = 'C-')
	SET @VagueDate_CB_End = NULL
ELSE IF (UPPER(@VagueDate_CB_Type) = '-Y') OR (UPPER(@VagueDate_CB_Type) = '-C')
	SET @VagueDate_CB_Start = NULL

RETURN dbo.ufn_DoVagueDatesOverlap(@VagueDate_CB_Start, @VagueDate_CB_End, @VagueDate_DB_Start, @VagueDate_DB_End)
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_CBWrapperForDoVagueDatesOverlap]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_CBWrapperForDoVagueDatesOverlap'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_CBWrapperForDoVagueDatesOverlap TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_CBWrapperForDoVagueDatesOverlap TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_CBWrapperForDoVagueDatesOverlap TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_CBWrapperForDoVagueDatesOverlap TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_CBWrapperForDoVagueDatesOverlap TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_CBWrapperForDoVagueDatesOverlap TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[ufn_ConceptRelationAffectsLineage]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_ConceptRelationAffectsLineage
GO

/*===========================================================================*\
  Description:	Does the specified relation affect concept lineage?

  Parameters:   @from_concept_key		Source concept key
				@to_concept_key			Destination concept key
				@relation_type_key		Relation type key

  Created:		Jan 2004

  Last revision information:
	$Revision: 1 $
	$Date: 27/08/04 9:54 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_ConceptRelationAffectsLineage(
	@from_concept_key		CHAR(16),
	@to_concept_key			CHAR(16),
	@relation_type_key		CHAR(16))
RETURNS
	BIT
AS
BEGIN
	DECLARE		@from_group						CHAR(16),
				@from_preferred					BIT,
				@to_group						CHAR(16),
				@to_preferred					BIT,
				@hierarchy_relation_type_key	CHAR(16)

	SELECT		@from_group						=	c.Concept_Group_Key,
				@from_preferred					=	c.List_Preferred,
				@hierarchy_relation_type_key	=	g.Hierarchy_Relation_Type_Key
	FROM		Concept							AS	c
	INNER JOIN	Concept_Group					AS	g
	ON			g.Concept_Group_Key				=	c.Concept_Group_Key
	WHERE		c.Concept_Key					=	@from_concept_key

	SELECT		@to_group						=	c.Concept_Group_Key,
				@to_preferred					=	c.List_Preferred
	FROM		Concept							AS	c
	WHERE		c.Concept_Key					=	@to_concept_key

	RETURN		CASE
					WHEN @from_group IS NULL OR @to_group IS NULL THEN 0
					WHEN @from_preferred <> 1 THEN 0
					WHEN @to_preferred <> 1 THEN 0
					WHEN @from_group <> @to_group THEN 0
					WHEN @relation_type_key <> @hierarchy_relation_type_key THEN 0
					ELSE 1
				END
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_ConceptRelationAffectsLineage]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_ConceptRelationAffectsLineage'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_ConceptRelationAffectsLineage TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_ConceptRelationAffectsLineage TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_ConceptRelationAffectsLineage TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_ConceptRelationAffectsLineage TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_ConceptRelationAffectsLineage TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_ConceptRelationAffectsLineage TO [Dev - JNCC SQL]
	END
GO


/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_DoVagueDatesOverlap]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_DoVagueDatesOverlap
GO

/*===========================================================================*\
  Description:	Returns true if the two VagueDate overlap, otherwise false

  Parameters:	@VagueDate_1_Start 		Start of first VagueDate
				@VagueDate_1_End		End of first VagueDate
				@VagueDate_2_Start		Start of second VagueDate
				@VagueDate_2_End		End of second VagueDate

  Created:	30 October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 9:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_DoVagueDatesOverlap]
(
@VagueDate_1_Start INT,
@VagueDate_1_End INT,
@VagueDate_2_Start INT,
@VagueDate_2_End INT
)

RETURNS BIT

AS
BEGIN

DECLARE @Result BIT

DECLARE @LowIntegerLimit INT
DECLARE @HighIntegerLimit INT

SET @LowIntegerLimit = -2147483648
SET @HighIntegerLimit = 2147483647

SET @VagueDate_1_Start = ISNULL(@VagueDate_1_Start, @LowIntegerLimit)
SET @VagueDate_1_End = ISNULL(@VagueDate_1_End, @HighIntegerLimit)

SET @VagueDate_2_Start = ISNULL(@VagueDate_2_Start, @LowIntegerLimit)
SET @VagueDate_2_End = ISNULL(@VagueDate_2_End, @HighIntegerLimit)

IF ((@VagueDate_1_Start BETWEEN @VagueDate_2_Start AND @VagueDate_2_End)
		OR (@VagueDate_1_End BETWEEN @VagueDate_2_Start AND @VagueDate_2_End)
		OR (@VagueDate_1_Start <= @VagueDate_2_Start AND @VagueDate_1_End >= @VagueDate_2_Start)
		OR (@VagueDate_1_Start <= @VagueDate_2_End AND @VagueDate_1_End >= @VagueDate_2_End))
	SET @Result = 1
ELSE
	SET @Result = 0

	RETURN @Result
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_DoVagueDatesOverlap]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_DoVagueDatesOverlap'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_DoVagueDatesOverlap TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_DoVagueDatesOverlap TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_DoVagueDatesOverlap TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_DoVagueDatesOverlap TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_DoVagueDatesOverlap TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_DoVagueDatesOverlap TO [Dev - JNCC SQL]
	END
GO


/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetConservationStatus]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetConservationStatus
GO

/*===========================================================================*\
  Description:	Returns Conservation Status

  Parameters:	@StatusIndex		- Status Index

  Created:	18 September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 9:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_GetConservationStatus(
  @StatusIndex TINYINT
)
RETURNS varchar(50)

AS
BEGIN

	DECLARE @Output VARCHAR(10)
	DECLARE @Concept_Key CHAR(16)
	SET @Output = 
		CASE @StatusIndex
			WHEN 0 THEN 'Pending'
			WHEN 1 THEN 'Open'
			WHEN 2 THEN 'Closed'
			WHEN 3 THEN 'Postponed'
			WHEN 4 THEN 'Abandoned'
		END

  RETURN @Output

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetConservationStatus]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetConservationStatus'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetConservationStatus TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetConservationStatus TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetConservationStatus TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetConservationStatus TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetConservationStatus TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetConservationStatus TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetDateFromVagueDate]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetDateFromVagueDate
GO

/*===========================================================================*\
  Description:	Returns a Vague Date string in the specified ShortDateFormat

  Parameters:	@VagueDateStart		- Start of Vague Date
				@VagueDateEnd		- End of Vague Date
				@VagueDateType		- Type of Vague Date
				@ShortDateFormat	- Short Date Format

  Created:	18 September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 9:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetDateFromVagueDate]
(
@VagueDateStart INT,
@VagueDateEnd INT,
@VagueDateType VARCHAR(2)
)
RETURNS varchar(50)

AS
BEGIN

DECLARE @ReturnValue VARCHAR(50)
DECLARE @ShortDateFormat VARCHAR(12)
SELECT @ShortDateFormat=DATA FROM SETTING WHERE [NAME]='ShortDates' 

IF @VagueDateType IS NULL GOTO FINISH

--Procedure starts here
DECLARE @Y1 INT
DECLARE @M1 INT
DECLARE @D1 INT

DECLARE @Y2 INT
DECLARE @M2 INT
DECLARE @D2 INT

IF (@VagueDateStart IS NOT NULL)
BEGIN
	SET @Y1 = dbo.ufn_GetDatePartFromInteger(@VagueDateStart, 'y')
	SET @M1 = dbo.ufn_GetDatePartFromInteger(@VagueDateStart, 'm')
	SET @D1 = dbo.ufn_GetDatePartFromInteger(@VagueDateStart, 'd')
END

IF (@VagueDateEnd IS NOT NULL)
BEGIN
	SET @Y2 = dbo.ufn_GetDatePartFromInteger(@VagueDateEnd, 'y')
	SET @M2 = dbo.ufn_GetDatePartFromInteger(@VagueDateEnd, 'm')
	SET @D2 = dbo.ufn_GetDatePartFromInteger(@VagueDateEnd, 'd')
END


DECLARE @StartDate VARCHAR(20)
DECLARE @EndDate VARCHAR(20)

IF @VagueDateType = 'D'
BEGIN
  --Handle day
	IF @VagueDateStart IS NOT NULL
		SET @ReturnValue = dbo.ufn_GetDateInShortDateFormat(@Y1, @M1, @D1, @ShortDateFormat)
END
ELSE IF @VagueDateType = 'DD'
BEGIN
  --Handle day range vague dates
	IF @VagueDateStart IS NOT NULL AND @VagueDateEnd IS NOT NULL
	BEGIN
		SET @StartDate = dbo.ufn_GetDateInShortDateFormat(@Y1, @M1, @D1, @ShortDateFormat)
		SET @EndDate = dbo.ufn_GetDateInShortDateFormat(@Y2, @M2, @D2, @ShortDateFormat)
		SET @ReturnValue = @StartDate + ' - ' + @EndDate
	END
END
ELSE BEGIN
	--Handle all other vague dates
	SET @ReturnValue =
		CASE @VagueDateType 
		  WHEN 'U' THEN 'Unknown'
	    --Day - to convert to a day, we use a dummy year 2003, then correct it to the original year.  
			--This avoids problems with dates prior to 1753
		  WHEN 'Y' THEN CAST(@Y1 AS VARCHAR(4))
		  WHEN 'YY' THEN CAST(@Y1 AS VARCHAR(4)) + ' - ' + CAST(@Y2 AS VARCHAR(4))
		  WHEN 'Y-' THEN CAST(@Y1 AS VARCHAR(4)) + ' -'
		  WHEN '-Y' THEN '- ' + CAST(@Y2 AS VARCHAR(4))
			WHEN 'C' THEN CAST(ROUND(@Y1/100,0)+1 AS VARCHAR(2))+'c'
			WHEN 'CC' THEN CAST(ROUND(@Y1/100,0)+1 AS VARCHAR(2))+'c - ' + CAST(ROUND(@Y2/100,0)+1 AS VARCHAR(2)) + 'c'
			WHEN 'C-' THEN CAST(ROUND(@Y1/100,0)+1 AS VARCHAR(2))+'c -'
			WHEN '-C' THEN '- ' + CAST(ROUND(@Y2/100,0)+1 AS VARCHAR(2)) + 'c'
			WHEN 'O' THEN dbo.ufn_MonthToPreferredMonth(@M1) + ' ' + CAST(@Y1 AS VARCHAR(4))
			WHEN 'OO' THEN 
					dbo.ufn_MonthToPreferredMonth(@M1) + ' ' + CAST(@Y1 AS VARCHAR(4)) + ' - ' +
					dbo.ufn_MonthToPreferredMonth(@M2) + ' ' + CAST(@Y2 AS VARCHAR(4))
			WHEN 'M' THEN dbo.ufn_MonthToPreferredMonth(@M1)
			WHEN 'S' THEN dbo.ufn_MonthToSeason(@M1)
			WHEN 'P' THEN dbo.ufn_MonthToSeason(@M1) + ' ' + CAST(@Y1 AS VARCHAR(4))
		END
END
FINISH:
RETURN @ReturnValue

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetDateFromVagueDate]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetDateFromVagueDate'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetDateFromVagueDate TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetDateFromVagueDate TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetDateFromVagueDate TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetDateFromVagueDate TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetDateFromVagueDate TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetDateFromVagueDate TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetDateInShortDateFormat]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetDateInShortDateFormat
GO

/*===========================================================================*\
  Description:	Returns a date string in the specified ShortDateFormat

  Parameters:	@Y					- Year part of date
				@M					- Month part of date
				@D					- Day part of date
				@ShortDateFormat	- Short Date Format

  Created:	18 September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 9:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetDateInShortDateFormat]
(
@Y INT,
@M INT,
@D INT,
@ShortDateFormat VARCHAR(20)
)
RETURNS VARCHAR(20)
WITH SCHEMABINDING

AS
BEGIN

DECLARE @ReturnValue VARCHAR(20)
	
SET @ReturnValue = @ShortDateFormat

--Translate long years
SET @ReturnValue = REPLACE(@ReturnValue, 'yyyy', CAST(@Y AS VARCHAR(4)))

--Translate short years
SET @ReturnValue = REPLACE(@ReturnValue, 'yy', RIGHT(CAST(@Y AS VARCHAR(4)),2))

--Translate long months - padded with leading zero
SET @ReturnValue = REPLACE(@ReturnValue, 'mm', 
	CASE LEN(CAST(@M AS VARCHAR(2)))
	WHEN 1 THEN 
		'0' + CAST(@M AS VARCHAR(2)) 
	ELSE 
		CAST(@M AS VARCHAR(2)) 
	END)

--Translate short months
SET @ReturnValue = REPLACE(@ReturnValue, 'm', CAST(@M AS VARCHAR(2)))

--Translate long days
SET @ReturnValue = REPLACE(@ReturnValue, 'dd', 
	CASE LEN(CAST(@D AS VARCHAR(2))) 
	WHEN 1 THEN 
		'0' + CAST(@D AS VARCHAR(2)) 
	ELSE 
		CAST(@D AS VARCHAR(2)) 
	END)

--Translate short days
SET @ReturnValue = REPLACE(@ReturnValue, 'd', CAST(@D AS VARCHAR(2)))

RETURN @ReturnValue

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetDateInShortDateFormat]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetDateInShortDateFormat'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetDateInShortDateFormat TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetDateInShortDateFormat TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetDateInShortDateFormat TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetDateInShortDateFormat TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetDateInShortDateFormat TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetDateInShortDateFormat TO [Dev - JNCC SQL]
	END
GO


/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetDatePartFromInteger]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetDatePartFromInteger
GO

/*===========================================================================*\
  Description:	Returns the specified date part of a date that is expressed as an integer

  Parameters:	@IntegerDateValue - Date as integer
				@DatePartToReturn - Date part to be returned 'y'|'m'|'d'

  Created:	18 September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 9:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetDatePartFromInteger]
(
@IntegerDateValue INT,
@DatePartToReturn CHAR(1)
)
RETURNS INT

AS
BEGIN

	DECLARE @Y INT
	DECLARE @M INT
	DECLARE @D INT
	DECLARE @ReturnValue INT

	declare @D1 int
	set @D1=365 							-- days in 1 year
	declare @D4 int
	set @D4 = @D1 * 4 + 1			-- days in 4 years, inc leap year
	declare @D100 int
	set @D100 = @D4 * 25 - 1 	-- days in 100 years (no leap year on 100th year)
	declare @D400 int
	set @D400 = @D100 * 4 + 1	-- days in 400 years - every 400 years you do  get a leap year
	--variables
	declare @T int
	declare @I int
	-- get number of days since 1/1/01 
	set @T = @IntegerDateValue+693593
	set @Y=1
	-- find number of whole 400 year blocks
	set @Y = @T / @D400
	set @T = @T - @Y * @D400
	set @Y = @Y * 400 + 1
	set @I = @T / @D100
	set @D = @T - @I * @D100
	if @I=4 begin
		set @I = @I - 1
		set @D = @D + @D100
	end
	set @Y = @Y + @I * 100
	set @I = @D / @D4
	set @D = @D - @I * @D4
	set @Y = @Y + @I * 4
	set @I = @D / @D1
	set @D = @D - @I * @D1
	if @I = 4 begin
		set @I = @I - 1
		set @D = @D + @D1
	end
	set @Y=@Y + @I

	set @M=1
	while 1=1 begin
		set @I = 
			case @M
				when 1 then 31
				when 2 then 
					case when (@Y % 4 = 0) and ((@Y % 100 <> 0) or (@Y % 400 = 0)) then 29 else 28 end
				when 3 then 31
				when 4 then 30
			  	when 5 then 31
				when 6 then 30
				when 7 then 31
				when 8 then 31
				when 9 then 30
				when 10 then 31
				when 11 then 30
				when 12 then 31
			end
		if @D<@I break
		set @D = @D - @I
		set @M = @M + 1
	end

	IF UPPER(@DatePartToReturn) = 'Y'
		SET @ReturnValue = @Y
	ELSE IF UPPER(@DatePartToReturn) = 'M'
		SET @ReturnValue = @M
	ELSE IF UPPER(@DatePartToReturn) = 'D'
		SET @ReturnValue = @D+1 -- + 1 because 1st day if month is day 1, not zero

	RETURN @ReturnValue
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetDatePartFromInteger]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetDatePartFromInteger'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetDatePartFromInteger TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetDatePartFromInteger TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetDatePartFromInteger TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetDatePartFromInteger TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetDatePartFromInteger TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetDatePartFromInteger TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFieldCollectors]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetFieldCollectors
GO

/*===========================================================================*\
  Description:	Gets the field collectors for a collection_unit_key. If 
		there is only one field collector for a collection_unit_key, 
		then just return that one surname. If there are two, return
		two surnames. If there are more than two, then return the
		first surname and 'et al.'. The surnames are sorted so that
		the survey_event_recorder with the earliest entry_date is
		the first surname returned.

  Parameters:	@CollectionUnitKey

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 9:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetFieldCollectors]
	(@CollectionUnitKey char(16))
RETURNS varchar(70)
AS
BEGIN
	DECLARE	@FieldCollectors varchar(70)
	DECLARE @Surname varchar(30)
	DECLARE @RowCount int
	SET @FieldCollectors = ''
	
	-- Firstly, we need to know how many rows the select statement will bring back. 
	-- NB. If there is a way of getting the row count from the select within the cursor, then this
	-- first select statement that only gets the count could be removed. @@RowCount doesn't work within
	-- the cursor, so this was the only way I could think of doing it.
	SELECT		
			@RowCount = Count(*)
	FROM		Specimen_Field_Data AS SFD
	LEFT JOIN	Occurrence AS O ON O.Occurrence_Key = SFD.Occurrence_Key
	LEFT JOIN	Taxon_Occurrence AS XO ON XO.Taxon_Occurrence_Key = SFD.Taxon_Occurrence_Key
	INNER JOIN	Sample_Recorder AS SR ON (SR.Sample_Key = O.Sample_Key OR SR.Sample_Key = XO.Sample_Key)
	INNER JOIN	Survey_Event_Recorder AS SER ON SER.SE_Recorder_Key = SR.SE_Recorder_Key
	INNER JOIN	Individual AS I ON I.Name_Key = SER.Name_Key
	WHERE 		SFD.Collection_Unit_Key = @CollectionUnitKey
	AND 		SFD.Gathering_Event = 1

	-- Declare the cursor and do the select statement to get the surnames
	DECLARE csrFieldCollector CURSOR FAST_FORWARD
	FOR
		SELECT		
				I.Surname AS GathererName
		FROM		Specimen_Field_Data AS SFD
		LEFT JOIN	Occurrence AS O ON O.Occurrence_Key = SFD.Occurrence_Key
		LEFT JOIN	Taxon_Occurrence AS XO ON XO.Taxon_Occurrence_Key = SFD.Taxon_Occurrence_Key
		INNER JOIN	Sample_Recorder AS SR ON (SR.Sample_Key = O.Sample_Key OR SR.Sample_Key = XO.Sample_Key)
		INNER JOIN	Survey_Event_Recorder AS SER ON SER.SE_Recorder_Key = SR.SE_Recorder_Key
		INNER JOIN	Individual AS I ON I.Name_Key = SER.Name_Key
		WHERE 		SFD.Collection_Unit_Key = @CollectionUnitKey
		AND 		SFD.Gathering_Event = 1
		ORDER BY	SER.Entry_Date ASC
	
	-- If there are any field collectors associated with this collection_unit_key...
	IF @RowCount > 0 
	BEGIN
		OPEN csrFieldCollector		

		-- Put the first surname into the string.
		FETCH NEXT FROM csrFieldCollector INTO @Surname
		IF @@FETCH_STATUS = 0 SELECT @FieldCollectors = @Surname
	
		-- If there are only two surnames returned, show the second one.
		IF @RowCount = 2
		BEGIN
			FETCH NEXT FROM csrFieldCollector INTO @Surname
			IF @@FETCH_STATUS = 0 SELECT @FieldCollectors = @FieldCollectors + ' + ' + @Surname
		END

		-- If there are more than two surnames returned, only show the first one, then add '+ et al.' to it.
		IF @RowCount > 2
		BEGIN
			SELECT @FieldCollectors = @FieldCollectors + ' et al.'
		END
	
		CLOSE csrFieldCollector
	END
	
	DEALLOCATE csrFieldCollector

	RETURN @FieldCollectors
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFieldCollectors]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetFieldCollectors'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetFieldCollectors TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetFieldCollectors TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetFieldCollectors TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetFieldCollectors TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetFieldCollectors TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetFieldCollectors TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedIndividualByParams]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetFormattedIndividualByParams
GO

/*===========================================================================*\
  Description:	Returns the individual's formatted name for the given parameters.
		The rule for individual name format is:
			Title + (Forename | Initials) + Surname

		Null fields are omitted.

  Parameters:	@Title:		Title
				@Initials:	Initials
				@Forename:	Forname
				@Surname:	Surname

  Created:	27 August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 9:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetFormattedIndividualByParams]
(
@Title VARCHAR(4),
@Initials VARCHAR(8),
@ForeName VARCHAR(20),
@Surname VARCHAR(30)
)
RETURNS varchar(100)


AS
BEGIN
	DECLARE	@FormattedName varchar(100)
	SET @FormattedName = ''

	SET @FormattedName = 
		CASE WHEN @Forename IS NULL THEN
			CASE WHEN @Initials IS NULL THEN
				CASE WHEN @Title IS NULL THEN 
					@Surname
				ELSE 
					@Title + ' ' + @Surname
				END
			ELSE
				@Initials + ' ' + @Surname
			END
		ELSE 
			@Forename + ' ' + @Surname 
		END

	RETURN @FormattedName
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedIndividualByParams]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetFormattedIndividualByParams'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetFormattedIndividualByParams TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetFormattedIndividualByParams TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetFormattedIndividualByParams TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetFormattedIndividualByParams TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetFormattedIndividualByParams TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetFormattedIndividualByParams TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetFormattedName
GO


/*===========================================================================*\
  Description:	Returns the formatted name for the given name key.
		The rule for individual name format is:
			Title + (Forename | Initials) + Surname

		The rule for organisation name format is
			Acronym + ', ' + Full_Name

		Null fields are omitted.

  Parameters:	@NameKey

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 9:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetFormattedName]
	(@NameKey char(16))
RETURNS varchar(100)
AS
BEGIN
	DECLARE	@FormattedName varchar(100)
	SET @FormattedName = ''

	IF EXISTS(SELECT * FROM [Name] WHERE Name_Key = @NameKey AND Organisation = 0)
	BEGIN
		SELECT	@FormattedName = 
			CASE WHEN Forename IS NULL THEN
				CASE WHEN Initials IS NULL THEN
					CASE WHEN Title IS NULL THEN Surname
					ELSE Title + ' ' + Surname END
				ELSE Initials + ' ' + Surname END
			ELSE Forename + ' ' + Surname END
		FROM	Individual
		WHERE	Name_Key = @NameKey
	END ELSE BEGIN
		SELECT	@FormattedName = 
			CASE WHEN Acronym IS NOT NULL THEN Acronym + ', ' + Full_Name
			ELSE Full_Name END
		FROM Organisation 
		WHERE Name_Key = @NameKey
	END
	RETURN @FormattedName
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetFormattedName'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetFormattedName TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetFormattedName TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetFormattedName TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetFormattedName TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetFormattedName TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetFormattedName TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedTaxonNameByParams]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetFormattedTaxonNameByParams
GO

/*===========================================================================*\
  Description:	Returns the Taxon name as a formatted string given the input parameters.
		The rule for Taxon name format is:
			Common_Name + (Actual_Name Authority)
		where only the Actual_Name is mandatory and other fields can be in italics

		Null fields are omitted.

  Parameters:	@Actual_Name 			Actual name of Taxon
				@Actual_Name_Italic		Whether name should be in italics
				@Common_Name			Common name of Taxon
				@Common_Name_Italic		Whether name should be in italics
				@Authority				Taxon Authority
				@ShowCommonNames 		Whether or not Common names should be shown

  Created:	28 August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 9:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetFormattedTaxonNameByParams]
(
@Actual_Name VARCHAR(60),
@Actual_Name_Italic BIT,
@Common_Name VARCHAR(60),
@Common_Name_Italic BIT,
@Authority VARCHAR(65),
@ShowCommonNames BIT

)
RETURNS varchar(185)


AS
BEGIN
	DECLARE	@FormattedName varchar(185)
	SET @FormattedName = ''

	--Ignore common name if not required or same as preferred name
	IF @ShowCommonNames = 0 OR @Actual_Name=@Common_Name
		SET @Common_Name = NULL

	SET @FormattedName = 
	CASE 
	WHEN @Common_Name IS NULL THEN
		CASE @Actual_Name_Italic
		WHEN 0 THEN
			CASE WHEN @Authority IS NULL THEN 
				@Actual_Name
			ELSE 
				@Actual_Name + ' ' + @Authority
			END
		ELSE
			CASE WHEN @Authority IS NULL THEN 
				'<i>' + @Actual_Name + '</i>'
			ELSE 
				'<i>' + @Actual_Name + '</i> ' + @Authority
			END
		END
	WHEN @Common_Name_Italic = 0 THEN
		CASE @Actual_Name_Italic
		WHEN 0 THEN
			CASE WHEN @Authority IS NULL THEN 
				@Common_Name + ' (' + @Actual_Name + ')'
			ELSE 
				@Common_Name + ' (' + @Actual_Name + ' ' + @Authority + ')' 
			END
		ELSE 
			CASE WHEN @Authority IS NULL THEN 
				@Common_Name + ' (<i>' + @Actual_Name + '</i>)'
			ELSE 
				@Common_Name + ' (<i>' + @Actual_Name + '</i> ' + @Authority + ')'
			END
		END
	WHEN @Common_Name_Italic = 1 THEN
		CASE @Actual_Name_Italic
		WHEN 0 THEN
			CASE WHEN @Authority IS NULL THEN 
				'<i>' + @Common_Name + '</i> (' + @Actual_Name + ')'
			ELSE 
				'<i>' + @Common_Name + '</i> (' + @Actual_Name + ' ' + @Authority + ')' 
			END
		ELSE 
			CASE WHEN @Authority IS NULL THEN 
				'<i>' + @Common_Name + '</i> (<i>' + @Actual_Name + '</i>)'
			ELSE 
				'<i>' + @Common_Name + '</i> (<i>' + @Actual_Name + '</i> ' + @Authority + ')'
			END
		END
	END

	RETURN @FormattedName
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedTaxonNameByParams]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetFormattedTaxonNameByParams'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetFormattedTaxonNameByParams TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetFormattedTaxonNameByParams TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetFormattedTaxonNameByParams TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetFormattedTaxonNameByParams TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetFormattedTaxonNameByParams TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetFormattedTaxonNameByParams TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedTerm]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetFormattedTerm
GO

/*===========================================================================*\
  Description:	Returns the formatted term for the supplied term, author, common
		term and author

		Null fields are omitted.

  Parameters:	@Term, @Author, @CommonTerm, @CommonAuthor

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 9:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetFormattedTerm]
	(@Term varchar(150),
   @Author varchar(100),
   @CommonTerm varchar(150),
   @CommonAuthor varchar(100))
RETURNS varchar(505)
AS
BEGIN
  DECLARE @FormalName varchar(251)  -- Term (varchar(150)) + space + Author (varchar(100)
  DECLARE @CommonName varchar(251)
	DECLARE	@FormattedTerm varchar(505) -- 2 names + extra space and 2 brackets

	SET @FormattedTerm = ''

  SET @FormalName = @Term
  IF @Author IS NOT NULL
    SET @FormalName = @FormalName + ' ' + @Author

  IF @CommonTerm IS NOT NULL BEGIN
    SET @CommonName = @CommonTerm
    IF @CommonAuthor IS NOT NULL
      SET @CommonName = @CommonName + ' ' + @CommonAuthor
  END

  IF @CommonName IS NOT NULL AND @CommonName<>@FormalName
    SET @FormattedTerm = @CommonName + ' (' + @FormalName + ')'
  ELSE
   SET @FormattedTerm = @FormalName

  RETURN @FormattedTerm
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedTerm]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetFormattedTerm'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetFormattedTerm TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetFormattedTerm TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetFormattedTerm TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetFormattedTerm TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetFormattedTerm TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetFormattedTerm TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetLoanDates]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetLoanDates
GO

/*===========================================================================*\
  Description:	Returns the dates of a loan.

  Parameters:	@Key	Movement_Key

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 9:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetLoanDates]
	(@Key char(16))
RETURNS varchar(50)
AS
BEGIN
	DECLARE	@DateString varchar(50)

	DECLARE @OutboundDate varchar(20)
	DECLARE @InboundDate varchar(20)
	DECLARE @ExpectedDate varchar(20)
	
	DECLARE @InboundVagueDateStart int
	DECLARE @ExpectedVagueDateStart int
	
	-- Outbound (start) date
	SELECT TOP 1	@OutboundDate = dbo.ufn_GetDateFromVagueDate(MM.Vague_Date_Start, MM.Vague_Date_End, MM.Vague_Date_Type)
	FROM		Movement_Direction AS MD
	INNER JOIN	Movement_Of_Material AS MM ON MM.Movement_Direction_Key = MD.Movement_Direction_Key
						AND MD.OutBound = 1
	WHERE		MD.Movement_Key = @Key
	ORDER BY	MM.Vague_Date_Start ASC
	
	-- Inbound (end) date
	SELECT TOP 1	@InboundDate = dbo.ufn_GetDateFromVagueDate(MM.Vague_Date_Start, MM.Vague_Date_End, MM.Vague_Date_Type),
			@InboundVagueDateStart = Vague_Date_Start
	FROM		Movement_Direction AS MD
	INNER JOIN	Movement_Of_Material AS MM ON MM.Movement_Direction_Key = MD.Movement_Direction_Key
						AND MD.OutBound = 0
	WHERE		MD.Movement_Key = @Key
	ORDER BY	MM.Vague_Date_Start DESC
	
	-- Getting expected completion date
	SELECT 		@ExpectedDate = dbo.ufn_GetDateFromVagueDate(Exp_Vague_Date_Start, Exp_Vague_Date_End, Exp_Vague_Date_Type),
			@ExpectedVagueDateStart = Exp_Vague_Date_Start
	FROM		Movement
	WHERE		Movement_Key = @Key
	
	-- Choose what to print. If the Expected Completion date is larger than the latest Inbound date, then use the 
	-- expected completion date. Otherwise, use the last inbound date.
	IF @ExpectedVagueDateStart > @InboundVagueDateStart 
		SET @DateString = ISNULL(@OutboundDate + ' - ' + @ExpectedDate, @OutboundDate)
	ELSE
		SET @DateString = ISNULL(@OutboundDate + ' - ' + @InboundDate, @OutboundDate)
	

	RETURN @DateString
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetLoanDates]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetLoanDates'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetLoanDates TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetLoanDates TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetLoanDates TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetLoanDates TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetLoanDates TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetLoanDates TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetMovementStatus]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetMovementStatus
GO

/*===========================================================================*\
  Description:	Returns movement status for a movement

  Parameters:	@Key		- movement key

  Created:	Aug 2004

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 9:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_GetMovementStatus(
  @Key CHAR(16)
)
RETURNS varchar(50)

AS
BEGIN

	DECLARE @TotalComplete int
	DECLARE @TotalIncomplete int
	DECLARE @Status VARCHAR(50)
	
	/*==============================================*\
  	   Count how many have been completed.
	\*==============================================*/
	SELECT @TotalComplete = Count(*)
	FROM Movement_Of_Ownership M
	INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key=M.Movement_Direction_Key
	WHERE MD.Movement_Key=@Key
	AND Completed=1
	
	SELECT @TotalComplete = @TotalComplete + Count(*)
	FROM Movement_Of_Material M
	INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key=M.Movement_Direction_Key
	WHERE MD.Movement_Key=@Key
	AND Completed=1

	/*==============================================*\
  	   Count how many have not been completed.
	\*==============================================*/
	SELECT @TotalIncomplete = Count(*)
	FROM Movement_Of_Ownership M
	INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key=M.Movement_Direction_Key
	WHERE MD.Movement_Key=@Key
	AND Completed=0
	
	SELECT @TotalIncomplete = @TotalIncomplete + Count(*)
	FROM Movement_Of_Material M
	INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key=M.Movement_Direction_Key
	WHERE MD.Movement_Key=@Key
	AND Completed=0

	/*===============================*\
  	   Determine the status.
	\*===============================*/
	IF @TotalIncomplete = 0 AND @TotalComplete <> 0
		SET @Status = 'Completed'
	ELSE IF @TotalIncomplete <> 0 AND @TotalComplete = 0
		SET @Status = 'Not started'
	ELSE
		SET @Status = 'Incomplete'

	RETURN @Status

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetMovementStatus]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetMovementStatus'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetMovementStatus TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetMovementStatus TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetMovementStatus TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetMovementStatus TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetMovementStatus TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetMovementStatus TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetMovementTypeName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetMovementTypeName
GO

/*===========================================================================*\
  Description:	Returns the Movement_Type_Name as a string

  Parameters:	@MovementType - MovementType as integer

  Created:	22 September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 9:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetMovementTypeName]
(
@MovementType INT
)
RETURNS VARCHAR(25)

AS
BEGIN

DECLARE @ReturnValue VARCHAR(25)

	SET @ReturnValue = 
	CASE @MovementType
		WHEN 0 THEN 'Accession'
		WHEN 1 THEN 'Exchange'
		WHEN 2 THEN 'Loan In'
		WHEN 3 THEN 'Loan Out'
		WHEN 4 THEN 'Destroyed'
		WHEN 5 THEN 'Disposed'
		WHEN 6 THEN 'Internal Transfer'
		WHEN 7 THEN 'Lost'
		WHEN 8 THEN 'Sold'
		WHEN 9 THEN 'Hosted Material'
	END

RETURN @ReturnValue
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetMovementTypeName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetMovementTypeName'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetMovementTypeName TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetMovementTypeName TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetMovementTypeName TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetMovementTypeName TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetMovementTypeName TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetMovementTypeName TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetRegNumber]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetRegNumber
GO

/*===========================================================================*\
  Description:	Returns the registration number of a collection unit.

  Parameters:	@Key	Collection_Unit_Key

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 9:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetRegNumber]
	(@Key char(16))
RETURNS varchar(30)
AS
BEGIN
	DECLARE	@CollectionUnitNumber varchar(30)
	SET @CollectionUnitNumber = ''

	SELECT	TOP 1	@CollectionUnitNumber = CUN.Number

	FROM		Collection_Unit_Number AS CUN
	INNER JOIN	Concept AS C ON C.Concept_Key = CUN.Type_Concept_Key

	WHERE		C.Meaning_Key = 'SYSTEM0000000001'
	AND		CUN.Collection_Unit_Key = @Key
	AND		C.Preferred = 1
	AND		CUN.Preferred = 1

	RETURN @CollectionUnitNumber
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetRegNumber]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetRegNumber'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetRegNumber TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetRegNumber TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetRegNumber TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetRegNumber TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetRegNumber TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetRegNumber TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetSpecimenGatheringDate]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetSpecimenGatheringDate
GO

/*===========================================================================*\
  Description:	For a Collection_Unit_Key, the location is returned.

  Parameters:	@Key 	Collection_Unit_Key

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 9:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetSpecimenGatheringDate]
	(@Key char(16))
RETURNS varchar(12)
AS
BEGIN
	DECLARE	@Date varchar(12)

	IF EXISTS(SELECT * FROM Specimen_Field_Data WHERE Collection_Unit_Key = @Key AND Gathering_Event = 1)
		SELECT		@Date = dbo.ufn_GetDateFromVagueDate
					(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type)
	
		FROM		Specimen_Field_Data AS SFD
	
		INNER JOIN	Occurrence As O ON O.Occurrence_Key = SFD.Occurrence_Key
		INNER JOIN	Sample AS S ON S.Sample_Key = O.Sample_Key

		WHERE		SFD.Collection_Unit_Key = @Key

	ELSE
		SET @Date = 'Unknown'

	RETURN @Date
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetSpecimenGatheringDate]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetSpecimenGatheringDate'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringDate TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringDate TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringDate TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringDate TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringDate TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringDate TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetSpecimenGatheringSite]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetSpecimenGatheringSite
GO

/*===========================================================================*\
  Description:	For a Collection_Unit_Key, the gathering site is returned.

  Parameters:	@Key 	Collection_Unit_Key

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 9:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetSpecimenGatheringSite]
	(@Key char(16))
RETURNS varchar(100)
AS
BEGIN
	DECLARE	@SpecimenLocation varchar(100)

	IF EXISTS(SELECT * FROM Specimen_Field_Data WHERE Collection_Unit_Key = @Key AND Gathering_Event = 1)
		SELECT		@SpecimenLocation = LN.Item_Name
	
		FROM		Specimen_Field_Data AS SFD
	
		LEFT JOIN	Taxon_Occurrence AS XO ON XO.Taxon_Occurrence_Key = SFD.Taxon_Occurrence_Key
		LEFT JOIN	Occurrence AS O ON O.Occurrence_Key = SFD.Occurrence_Key
		INNER JOIN	[Sample] AS S ON (S.Sample_Key = O.Sample_Key OR S.Sample_Key = XO.Sample_Key)
		INNER JOIN	Location_Name AS LN ON LN.Location_Key = S.Location_Key

		WHERE		SFD.Collection_Unit_Key = @Key AND LN.Preferred = 1
	ELSE
		SET @SpecimenLocation = 'Unknown'

	RETURN @SpecimenLocation
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetSpecimenGatheringSite]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetSpecimenGatheringSite'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringSite TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringSite TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringSite TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringSite TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringSite TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringSite TO [Dev - JNCC SQL]
	END
GO

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
    $Date: 27/08/04 9:54 $
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
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetVagueDateIntegerFromDate]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetVagueDateIntegerFromDate
GO

/*===========================================================================*\
  Description:	Returns the the integer part of a Vague date. Only intended to 
				work for post 1753 dates.

  Parameters:	@DateTime - Input Date

  Created:	07 October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 9:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetVagueDateIntegerFromDate]
(
@DateTime DATETIME
)
RETURNS INT

AS
BEGIN
	RETURN CAST(@DateTime AS INT) + 2 --Difference between Date cast as integer by Delphi SQL server
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetVagueDateIntegerFromDate]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetVagueDateIntegerFromDate'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetVagueDateIntegerFromDate TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetVagueDateIntegerFromDate TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetVagueDateIntegerFromDate TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetVagueDateIntegerFromDate TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetVagueDateIntegerFromDate TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetVagueDateIntegerFromDate TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[ufn_LineageSequenceNumber]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_LineageSequenceNumber
GO

/*===========================================================================*\
  Description:	Sequence number from the last level of the given lineage.

  Parameters:   @lineage				Lineage

  Created:		Jan 2004

  Last revision information:
	$Revision: 1 $
	$Date: 27/08/04 9:54 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_LineageSequenceNumber(
	@lineage			VARCHAR(900))
RETURNS
	VARCHAR(8)
AS
BEGIN
	DECLARE     @len		INT,
				@result		VARCHAR(8)

	SET			@len		=	CHARINDEX('\', REVERSE(@lineage)) - 1

	IF @len < 0
	BEGIN
		SET			@len		=	LEN(@lineage)
	END

	RETURN		RIGHT(@lineage, @len)
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_LineageSequenceNumber]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_LineageSequenceNumber'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_LineageSequenceNumber TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_LineageSequenceNumber TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_LineageSequenceNumber TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_LineageSequenceNumber TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_LineageSequenceNumber TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_LineageSequenceNumber TO [Dev - JNCC SQL]
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
    $Date: 27/08/04 9:54 $
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
	FROM dbo.VW_ConceptTermCommon
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

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_MonthToSeason]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_MonthToSeason
GO

/*===========================================================================*\
  Description:	Converts a Month into a Season

  Parameters:	@Month		- Month number

  Created:	18 September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 9:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_MonthToSeason(
  @Month INT
)
RETURNS varchar(150)

AS
BEGIN

	DECLARE @Output VARCHAR(6)
	DECLARE @Concept_Key CHAR(16)

	IF @Month=1 OR @Month=2 OR @Month=12 
	  SET @Concept_Key = 'SYSTEM000000008E'
	ELSE IF @Month=3 OR @Month=4 OR @Month=5
	  SET @Concept_Key = 'SYSTEM000000008F'
	ELSE IF @Month=6 OR @Month=7 OR @Month=8
	  SET @Concept_Key = 'SYSTEM000000008G'
	ELSE IF @Month=9 OR @Month=10 OR @Month=11
	  SET @Concept_Key = 'SYSTEM000000008H'


	SELECT @Output = PlainText 
	FROM dbo.VW_ConceptTermCommon
	WHERE Concept_Key = @Concept_Key

  RETURN @Output

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_MonthToSeason]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_MonthToSeason'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_MonthToSeason TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_MonthToSeason TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_MonthToSeason TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_MonthToSeason TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_MonthToSeason TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_MonthToSeason TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[ufn_NextChildLineage]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_NextChildLineage
GO

/*===========================================================================*\
  Description:	Next available lineage for a child of the given lineage in
  				the specified concept group.

  Parameters:   @parent_lineage			Lineage of parent
				@concept_group_key		Concept group key

  Created:		Jan 2004

  Last revision information:
	$Revision: 1 $
	$Date: 27/08/04 9:54 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_NextChildLineage(
	@parent_lineage		VARCHAR(900),
	@concept_group_key	CHAR(16))
RETURNS
	VARCHAR(900)
AS
BEGIN
	DECLARE     @lineage_pattern	VARCHAR(900),
				@sequence			VARCHAR(8)

	IF LEN(@parent_lineage) > 0
	BEGIN
		SELECT		@sequence				=	l.Last_Sequence_Number
		FROM		Concept_Lineage			AS	l
		INNER JOIN	Concept					AS	c
		ON			c.Concept_Key			=	l.Concept_Key
		WHERE		l.Lineage				=	@parent_lineage
		AND			c.Concept_Group_Key		=	@concept_group_key
	END
	ELSE
	BEGIN
		SELECT		@sequence				=	Last_Sequence_Number
		FROM		Concept_Group
		WHERE		Concept_Group_Key		=	@concept_group_key
	END

	SET			@sequence	=	CASE WHEN @sequence IS NULL
									THEN '0'
									ELSE dbo.ufn_Base36Sum(@sequence, 1)
								END

	RETURN		ISNULL(@parent_lineage, '')
				+ CASE WHEN LEN(@parent_lineage) > 0 THEN '\' ELSE '' END
				+ @sequence
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_NextChildLineage]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_NextChildLineage'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_NextChildLineage TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_NextChildLineage TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_NextChildLineage TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_NextChildLineage TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_NextChildLineage TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_NextChildLineage TO [Dev - JNCC SQL]
	END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[ufn_RemoveHtmlMarkup]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_RemoveHtmlMarkup
GO

/*===========================================================================*\
  Description:	Remove HTML markup from the given string.

				Note that this is currently a very simplistic implementation
				that only deals with italics (the '<i>' element).

  Parameters:	@item_name				Text to be processed

  Created:		November 2003

  Last revision information:
	$Revision: 1 $
	$Date: 27/08/04 9:54 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_RemoveHtmlMarkup(
	@item_name		NVARCHAR(300))
RETURNS
	NVARCHAR(300)
AS
BEGIN
	DECLARE		@result		NVARCHAR(300)

	SET			@result		=	REPLACE (@item_name, '<i>', '')
	SET			@result		=	REPLACE (@result, '</i>', '')

	RETURN		@result
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_RemoveHtmlMarkup]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_RemoveHtmlMarkup'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_RemoveHtmlMarkup TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_RemoveHtmlMarkup TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_RemoveHtmlMarkup TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_RemoveHtmlMarkup TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_RemoveHtmlMarkup TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_RemoveHtmlMarkup TO [Dev - JNCC SQL]
	END
GO

